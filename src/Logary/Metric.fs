module Logary.Metric

open NodaTime

open FSharp.Actor

open Logary.Internals

// inspiration: https://github.com/Feuerlabs/exometer/blob/master/doc/exometer_probe.md

/// a DP Segment is a string that tells a tale about a segment of a hierarchial
/// DP name
type DPSegment = string // TODO: use or remove

/// A data point is the name (atom) of a measure taken by a metric. It's not
/// globally unique, but specific to a metric instance.
type DP = DP of string

/// The metric types available, which specify how logary polls/gets the metric.
type MetricType =
  | Metric
  | Probe
  | HealthCheck
  override x.ToString() =
    match x with
    | Metric      -> "metric"
    | Probe       -> "probe"
    | HealthCheck -> "healthcheck"

/// The main protocol to talk to metric instances with
type MetricMsg =
  /// The GetValue implementation shall retrieve the value of one or more data
  /// points from the probe.
  | GetValue of DP list * ReplyChannel<(DP * Measure) list>
  /// The GetDataPoints shall return a list with all data points supported by
  /// the probe
  | GetDataPoints of ReplyChannel<DP list>
  /// Incorporate a new value into the metric maintained by the metric.
  | Update of Measure
  /// The Sample implementation shall sample data from the subsystem the probe
  /// is integrated with.
  | Sample
  /// The custom probe shall release any resources associated with the given
  /// state and return ok.
  | Shutdown of Acks ReplyChannel
  /// The Reset shall reset the state of the probe to its initial state.
  | Reset

[<CustomEquality; CustomComparison>]
type MetricConf =
  { name     : string
    ``type`` : MetricType
    initer   : RuntimeInfo -> IActor
    /// the period to sample the metric's value at
    sampling : Duration }
with
  override x.Equals(yobj) =
      match yobj with
      | :? MetricConf as y -> (x.name = y.name)
      | _ -> false

  override x.GetHashCode() =
    hash x.name

  interface System.IComparable with
    member x.CompareTo yobj =
      match yobj with
      | :? MetricConf as y -> compare x.name y.name
      | _ -> invalidArg "yobj" "cannot compare values of different types"

/// Start configuring a metric with a metric factory
let confMetric name sampling (factory : string -> Duration -> MetricConf) =
  factory name sampling

let validate (conf : MetricConf) = conf

/// initialises the configuration with the metadata to create a new metric-actor
let init metadata conf =
  conf.initer metadata

/// The GetValue implementation shall retrieve the value of one or more data
/// points from the probe.
let getValue (dps : DP list) =
  Actor.reqReply
    (fun chan -> GetValue(dps, chan))
    Infinite

/// The GetDataPoints shall return a list with all data points supported by
/// the probe
let getDataPoints (m : #IActor) =
  Actor.reqReply
    GetDataPoints
    Infinite
    m

/// Incorporate a new value into the metric maintained by the metric.
let update (m : Measure) actor =
  actor <-- Update m
  actor

/// The Sample implementation shall sample data from the subsystem the probe
/// is integrated with.
let sample actor =
  actor <-- Sample

/// The custom probe shall release any resources associated with the given
/// state and return ok.
let shutdown (a : #IActor) =
  Actor.reqReply
    Shutdown
    Infinite
    a

module MetricUtils =
  /// Called by metric implementations; each metric implementation has as its
  /// own responsibility to configure itself, so that is not done through this
  /// function. Not to be called directly, only called from inside Logary;
  /// each module: `Probe`, `Metric` and `HealthCheck` is responsible for having
  /// a function that can create standard named metrics which is usable from
  /// outside Logary.
  let stdNamedMetric ``type`` loop name sampling =
    { name     = name
      ``type`` = ``type``
      initer   = fun metadata ->
        Actor.spawn
          (Ns.create (sprintf "%O/%s" ``type`` name))
          (loop metadata)
      sampling = sampling }

module Reservoir =

  /// TODO: this is really a specific set of calculation functions; can I present
  /// it ore like such?
  ///
  /// TODO: what about working on floats and other value types?
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Snapshot =
    open System
    // TODO: when I need to read values multiple times:
    // memoized snapshot to avoid recalculation of values after reading

    type Snapshot =
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

    let rate (timeUnit : TimeUnit) state =
      // we know rate is in samples per tick
      state.rate * (TimeUnit.ticksPerUnit timeUnit)
