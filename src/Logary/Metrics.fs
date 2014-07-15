module Logary.Metrics

open Logary

// https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/ExponentiallyDecayingReservoir.java
// http://dimacs.rutgers.edu/~graham/pubs/papers/fwddecay.pdf
// https://www.youtube.com/watch?v=qURhXHbxbDU
// https://gist.github.com/haf/5e770676d8c007ca80c1
// https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/UniformReservoir.java
// http://www.cs.umd.edu/~samir/498/vitter.pdf
// http://researcher.watson.ibm.com/researcher/files/us-dpwoodru/tw11.pdf
// https://en.wikipedia.org/wiki/Reservoir_sampling

/// A meter measures the rate of events over time (e.g., "requests per second").
/// In addition to the mean rate, meters also track 1-, 5-, and 15-minute moving
/// averages.
type Meter =
  inherit Named
  abstract Mark : uint32 -> unit

/// A histogram measures the statistical distribution of values in a stream of data.
/// In addition to minimum, maximum, mean, etc., it also measures median, 75th,
/// 90th, 95th, 98th, 99th, and 99.9th percentiles.
type Histogram =
  abstract Update : float -> unit


module Reservoir =

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
    let private SamplePeriod = NodaTime.Duration.FromSeconds 5L

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
    let fifteenMinutesAWMA =
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

/// A timer measures both the rate that a particular piece of code is called
/// and the distribution of its duration.
type Timer =
  inherit Named
  abstract Start : unit -> TimerContext
and TimerContext =
  abstract Stop : unit -> unit

module Time =
  open System.Diagnostics

  open Logary.Internals
  open Logary.Logger
  open Logary.Measure

  /// Capture a timer metric with a given metric-level and metric-path.
  [<CompiledName "TimeLevel">]
  let timelvl (logger : logger) lvl path f =
    if lvl < logger.Level then f ()
    else
      let now = Date.utcNow ()
      let sw = Stopwatch.StartNew()
      try
        f ()
      finally
        sw.Stop()
        { m_value     = None
          m_value'    = Some (sw.ElapsedTicks)
          m_value''   = None
          m_path      = path
          m_timestamp = now
          m_level     = lvl
          m_unit      = Time Ticks
          m_tags      = []
          m_data      = Map.empty }
        |> logger.Measure

  /// Capture a timer metric with a given metric-path
  [<CompiledName "Time">]
  let time logger path = timelvl logger LogLevel.Info path

  /// Capture a timer metric with the logger's name as the metric-path
  [<CompiledName "TimeLog">]
  let timeLog logger = timelvl logger LogLevel.Info (logger.Name)

  /// Time a function execution with a 'path' equal to the passed argument.
  /// Path may be null, and is then replaced with the logger name
  [<CompiledName "TimePath">]
  let timePath (logger : logger) lvl path (f : System.Func<_>) =
    let path = match path with null -> logger.Name | p -> p
    timelvl logger lvl path (fun () -> f.Invoke())
