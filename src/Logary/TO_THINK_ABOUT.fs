module TO_THINK_ABOUT

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
      let now = Date.now ()
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


module internal Play =
  open System

  open Measure
  open HealthCheck
  open WinPerfCounter

  let mkMeasure' fValueTr fLevel rawValue =
    let m = Measure.mkMeasure "" (fValueTr rawValue)
    { m with m_level = fLevel (getValueFloat m) }

  module Categorisation =
    /// Finds the bucket that is less than or equal in value to the sample, yielding
    /// its corresponding label.
    let lteBucket (buckets : _ seq) (labels : _ seq) sample =
      Seq.take (Seq.length buckets) labels // all but last
      |> Seq.map2 (fun b l -> b, l, sample <= b) buckets // find first that is lte
      |> Seq.tryFind (fun (b, l, ok) -> ok) // any matches?
      |> Option.map (fun (_, l, _) -> l) // find its label then
      |> Option.fold (fun s t -> t) (Seq.last labels) // otherwise pick last label

    /// Divides a given value first by the divisor, then assigns it a bucket of
    /// `levels`
    let percentBucket divisor buckets labels =
      mkMeasure'
        (fun (v : float) -> v / divisor)
        (lteBucket buckets labels)

    /// Divides a given value first by the divisor, then assigns it a bucket of
    /// Info, Warn or Error.
    let percentBucket' divisor =
      percentBucket divisor [0.8; 0.9] [Info; Warn; Error]

  let cpus =
    [ "% Processor Time"
      "% User Time"
      "% Interrupt Time"
      "% Processor Time" ]
    |> List.map (fun counter ->
      let wpc = { category = "Processor"
                  counter  = counter
                  instance = Some AllInstances }
      toHealthCheck wpc (Categorisation.percentBucket' 100.))

  open System.Text

  let clr_proc =
    let cat = ".NET CLR Memory"
    let inst = pidToInstance cat (pid ())
    let wpc = { category = cat; counter  = "% Time in GC"; instance = inst }
    let MiB = 1024.f * 1024.f
    let toMiB = (fun v -> v / MiB)
    let descCounters =
      [ "Gen 0 Heap Size", toMiB, "MiB"
        "Gen 1 Heap Size", toMiB, "MiB"
        "Gen 2 Heap Size", toMiB, "MiB"
        "# Gen 0 Collections", id, ""
        "# Gen 1 Collections", id, ""
        "# Gen 2 Collections", id, "" ]
      |> List.map (fun (counter, fval, valUnit) -> mkPc' cat counter inst, fval, valUnit)
      |> List.filter (fun (c, _, _) -> Option.isSome c)
      |> List.map (fun (c, fval, valUnit) -> c.Value, fval, valUnit)

    let tf =
      Categorisation.percentBucket 100. [0.05; 0.5] [Info; Warn; Error]
      >> fun measuree ->
        descCounters
        |> List.fold
            (fun (sb : StringBuilder) (counter, fval, valUnit) ->
              let line = String.Format("{0}: {1:0.###} {2}", counter.CounterName, fval(counter.NextValue()), valUnit)
              sb.AppendLine(line) |> ignore
              sb)
            (StringBuilder())
        |> sprintf "%O"
        |> (fun desc -> HealthCheck.setDesc desc measuree)

    toHealthCheck wpc tf |> hasResources (descCounters |> List.map (fun (c, _, _) -> c))

  let printAll checks =
    let printSingle (check : healthcheck) =
      match check.GetValue() with
      | NoValue -> printfn "%s: -" check.Name
      | HasValue v ->
        let m = v.Measure
        printfn ">>> [%O] %s: %f\n%s" m.m_level check.Name (getValueFloat m) v.Description
    checks |> Seq.iter printSingle

  // printAll my_appdomain
  // printAll [clr_proc]

  // TODO: register new health checks as conf
  // TODO: register and unregister health checks at runtime
  // TODO: 'acceptor' for metrics
