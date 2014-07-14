module Logary.Metrics

open Logary

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


module Snapshot =
  open System
  // TODO: memoized snapshot to avoid recalculation of values after reading

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
  let min s = Array.min s.values
  let max s = Array.max s.values

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
module UniformReservoir =
  open System.Diagnostics.Contracts

  open Logary.Internals

  let private DefaultSize = 1028

  /// Mutable uniform distribution
  type UniformState =
    { count  : int
      values : int64 [] }

  [<Pure>]
  let create size =
    { count  = size
      values = Array.zeroCreate size }

  [<Pure>]
  let empty = create DefaultSize

  [<Pure>]
  let size r = min r.count r.values.Length

  [<Pure>]
  let snapshot r =
    Snapshot.create (r.values |> Seq.take (size r) |> Array.ofSeq)

  // impure update the reservoir
  let update r value =
    let count' = r.count + 1
    if count' <= r.values.Length then
      r.values.[count' - 1] <- value
    else
      let rnd = Rnd.nextInt' count'
      if rnd < r.values.Length then
        r.values.[rnd] <- value

    { r with count = r.count + 1 }

module Histograms =
  let x () = ()

  // https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/Reservoir.java

  type Reservoir =
    { size   : uint32
      update : uint64 -> Reservoir }

  // https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/ExponentiallyDecayingReservoir.java
  // http://dimacs.rutgers.edu/~graham/pubs/papers/fwddecay.pdf

  // https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/UniformReservoir.java
  // http://www.cs.umd.edu/~samir/498/vitter.pdf
  // http://researcher.watson.ibm.com/researcher/files/us-dpwoodru/tw11.pdf
  // https://en.wikipedia.org/wiki/Reservoir_sampling


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
          m_unit      = Units.Seconds
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
