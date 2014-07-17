namespace Logary

/// A scheduling actor that can call `Sample` on the metric/probe/health check.
module Sampler =
  // creds to Dave Thomas for his F# snippet
  open System.Threading
  
  open FSharp.Actor

  open NodaTime

  type ScheduleMsg<'a> =
    | Schedule of ('a -> unit) * 'a * Duration * Duration * CancellationTokenSource ReplyChannel
    | ScheduleOnce of ('a -> unit) * 'a * Duration * CancellationTokenSource ReplyChannel

  let private ms (d : Duration) =
    d.ToTimeSpan().TotalMilliseconds |> int

  let scheduleOnce (delay : Duration) msg receiver (cts: CancellationTokenSource) = async {
    do! Async.Sleep (ms delay)
    if cts.IsCancellationRequested then
      cts.Dispose ()
    else
      receiver msg
    }
  
  let scheduleMany initialDelay msg receiver delayBetween cts =
    let rec loop time (cts: CancellationTokenSource) = async {
      do! Async.Sleep time
      if cts.IsCancellationRequested then
        cts.Dispose ()
      else
        receiver msg
      return! loop delayBetween cts
    }
    loop initialDelay cts

  let scheduler (inbox : IActor<_>) =
    let rec loop() = async {
      let! msg, _ = inbox.Receive ()
      let cs = new CancellationTokenSource()
      match msg with
      | Schedule (receiver, msg : 'a, initialDelay, delayBetween, replyChan) ->
        Async.StartImmediate (scheduleMany (ms initialDelay) msg receiver (ms delayBetween) cs)
        replyChan.Reply cs
        return! loop ()
      | ScheduleOnce (receiver, msg:'a, delay, replyChan) ->
        Async.StartImmediate (scheduleOnce delay msg receiver cs)
        replyChan.Reply cs
        return! loop ()
    }
    loop ()
  
  /// Schedules a message to be sent to the receiver after the initialDelay.
  /// If delayBetween is specified then the message is sent reoccuringly at the
  /// delay between interval.
  let schedule scheduler receiver msg initialDelay (delayBetween : _ option) =
    let buildMessage replyChan =
      match delayBetween with
      | Some x ->
        Schedule (receiver, msg, initialDelay, x, replyChan)
      | _ ->
        ScheduleOnce (receiver, msg, initialDelay, replyChan)
    scheduler |> Actor.reqReply buildMessage Infinite

module Metric =
  open FSharp.Actor

  // inspiration: https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_probe.md

  /// A data point is the name (atom) of a measure taken by a metric.
  type DP = DP of string

  /// The main interface to talk to metric instances with
  type MetricMsg =
    /// The GetValue implementation shall retrieve the value of one or more data points from the probe.
    | GetValue of DP list * ReplyChannel<(DP * ``measure``) list>
    /// The GetDataPoints shall return a list with all data points supported by the probe
    | GetDataPoints of ReplyChannel<DP list>
    /// Incorporate a new value into the metric maintained by the metric.
    | Update of ``measure``
    /// The Sample implementation shall sample data from the subsystem the probe is integrated with.
    | Sample
    /// The custom probe shall release any resources associated with the given state and return ok.
    | Terminate
    /// The Reset shall reset the state of the probe to its initial state.
    | Reset

  

  module Reservoir =

    /// TODO: this is really a specific set of calculation functions; can I present
    /// it ore like such?
    ///
    /// TODO: what about working on floats and other value types?
    module Snapshot =
      open System
      // TODO: when I need to read values multiple times:
      // memoized snapshot to avoid recalculation of values after reading

      type snapshot =
        { values : int64 [] }

      let create unsorted =
        Array.sortInPlace unsorted
        { values = unsorted }

      let size s = s.values.Length

      let quantile s q =
        if q < 0. || q > 1. then
          invalidArg "q" "quantile is not in [0., 1.]"
        if size s = 0 then
          0.
        else
          let pos = q * float (s.values.Length + 1)
          match pos with
          | _ when pos < 1. ->
            float s.values.[0]
          | _ when pos >= float s.values.Length ->
            float s.values.[s.values.Length - 1]
          | _ ->
            let lower = s.values.[int pos - 1]
            let upper = s.values.[int pos]
            float lower + (pos - floor pos) * float (upper - lower)

      let median s = quantile s 0.5
      let percentile75th s = quantile s 0.75
      let percentile95th s = quantile s 0.95
      let percentile98th s = quantile s 0.98
      let percentile99th s = quantile s 0.99
      let percentile999th s = quantile s 0.999

      let values s = s.values
      let min s = if size s = 0 then 0L else Array.min s.values
      let max s = if size s = 0 then 0L else Array.max s.values

      let private meanAndSum s =
        if size s = 0 then 0., 0. else
        let mutable sum = 0.
        for x in s.values do
          sum <- sum + float x
        let mean = float sum / float s.values.Length
        mean, sum

      let mean = fst << meanAndSum

      let stdDev s =
        let size = size s
        if size = 0 then 0. else
        let mean = mean s
        let sum = s.values |> Array.map (fun d -> Math.Pow(float d - mean, 2.)) |> Array.sum
        sqrt (sum / float (size - 1))

    // starting off single-threaded
    module Uniform =
      open System
      open System.Diagnostics.Contracts

      open Logary.Internals

      let private DefaultSize = 1028

      /// Mutable uniform distribution
      type UniformState =
        { count  : bigint
          values : int64 [] }

      [<Pure>]
      let create size =
        { count  = 0I
          values = Array.zeroCreate size }

      [<Pure>]
      let empty = create DefaultSize

      [<Pure>]
      let size r = int (min r.count (bigint r.values.Length))

      [<Pure>]
      let snapshot r =
        Snapshot.create (r.values |> Seq.take (size r) |> Array.ofSeq)

      // impure update the reservoir
      let update r value =
        let count' = r.count + 1I
        if count' <= bigint r.values.Length then
          r.values.[int count' - 1] <- value
        else
          let rnd = Rnd.nextInt' (min (int count') Int32.MaxValue)
          if rnd < r.values.Length then
            r.values.[rnd] <- value

        { r with count = r.count + 1I }

    module SlidingWindow =
      open System

      let private DefaultSize = 1028

      type SlidingState =
        { count  : bigint
          values : int64 [] }

      let create size =
        { count  = 0I
          values = Array.zeroCreate size }

      let size r = int (min r.count (bigint r.values.Length))

      let snapshot r =
        Snapshot.create (r.values |> Seq.take (size r) |> Array.ofSeq)

      let update r value =
        let count' = r.count + 1I
        r.values.[int (r.count % (bigint r.values.Length))] <- value
        { r with count = count' }

    /// The an exponentially weighted moving average that gets ticks every
    /// period (a period is a duration between events), but can get
    /// `update`s at any point between the ticks.
    module ExpWeightedMovAvg =
      open NodaTime

      /// The period in between ticks; it's a duration of time between two data
      /// points.
      let private SamplePeriod = Duration.FromSeconds 5L

      let private OneMinute = 1.
      let private FiveMinutes = 5.
      let private FifteenMinutes = 15.

      /// calculate the alpha coefficient from a number of minutes
      ///
      /// - `duration` is how long is between each tick
      /// - `mins` is the number of minutes the EWMA should be calculated over
      let xMinuteAlpha duration mins =
        1. - exp (- Duration.minutes duration / mins)

      /// alpha coefficient for the `SamplePeriod` tick period, with one minute
      /// EWMA
      let M1Alpha = xMinuteAlpha SamplePeriod OneMinute, SamplePeriod
    
      /// alpha coefficient for the `SamplePeriod` tick period, with five minutes
      /// EWMA
      let M5Alpha = xMinuteAlpha SamplePeriod FiveMinutes, SamplePeriod
    
      /// alpha coefficient for the `SamplePeriod` tick period, with fifteen minutes
      /// EWMA
      let M15Alpha = xMinuteAlpha SamplePeriod FifteenMinutes, SamplePeriod

      type EWMAState =
        { inited    : bool
          rate      : float
          uncounted : int64
          alpha     : float
          /// interval in ticks
          interval  : float }

      /// Create a new EWMA state that you can do `update` and `tick` on.
      ///
      /// Alpha is dependent on the duration between sampling events ("how long
      /// time is it between the data points") so they are given as a pair.
      let create (alpha, duration : Duration) =
        { inited    = false
          rate      = 0.
          uncounted = 0L
          alpha     = alpha
          interval  = float duration.Ticks }

      /// duration: SamplePeriod
      let oneMinuteEWMA =
        create M1Alpha

      /// duration: SamplePeriod
      let fiveMinutesEWMA =
        create M5Alpha

      /// duration: SamplePeriod
      let fifteenMinuteEWMA =
        create M15Alpha

      let update state value =
        { state with uncounted = state.uncounted + value }

      let private calcRate currentRate alpha instantRate =
        currentRate + alpha * (instantRate - currentRate)

      let tick state =
        let count = float state.uncounted
        let instantRate = count / state.interval
        if state.inited then
          { state with uncounted = 0L
                       rate      = calcRate state.rate state.alpha instantRate }
        else
          { state with uncounted = 0L
                       inited    = true
                       rate      = instantRate }

      let rate (timeUnit : timeunit) state =
        // we know rate is in samples per tick
        state.rate * (TimeUnit.ticksPerUnit timeUnit)
