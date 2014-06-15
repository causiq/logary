namespace Logary

/// A counter is just a gauge for an AtomicLong instance. You can increment or
/// decrement its value. For example, we may want a more efficient way of
/// measuring the pending job in a queue:
type Counter =
  inherit Named
  abstract Inc : uint32 -> unit
  abstract Dec : uint32 -> unit

/// A gauge is an instantaneous measurement of a value.
/// This interface is implemented by the user and registered in Logary.
type Gauge =
  inherit Named
  abstract Put : float -> unit

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

/// Module for capturing metrics in their raw form
module Metrics =
  open System.Diagnostics

  open FSharp.Actor

  open NodaTime

  open Logary.Internals

  [<AbstractClass>]
  type MetricInstance(name, targets) =
    member x.Targets : IActor list = targets
    member x.Name = name
    interface Named with
      member x.Name = name

  /// Write a metric/measure of your chosing
  [<CompiledName "Metric">]
  let metric logger ms =
    (logger : Logger).Metric ms

  /// Increment the counter at the path 'path' at the info level
  let incr logger path =
    { value     = 1.
      path      = path
      timestamp = Date.utcNow()
      level     = Info
      mtype     = Counter
      data      = Map.empty }
    |> metric logger

  let incrBy logger path amount =
    { value     = amount
      path      = path
      timestamp = Date.utcNow ()
      level     = Info
      mtype     = Counter
      data      = Map.empty }
    |> metric logger

  let decr logger path =
    { value     = -1.
      path      = path
      timestamp = Date.utcNow ()
      level     = Info
      mtype     = Counter
      data      = Map.empty }
    |> metric logger

  let decrBy logger path amount =
    { value     = -amount
      path      = path
      timestamp = Date.utcNow ()
      level     = Info
      mtype     = Counter
      data      = Map.empty }
    |> metric logger

  let gauge logger path amount =
    { value = amount
      path  = path
      timestamp = Date.utcNow ()
      level     = Info
      mtype     = Gauge
      data      = Map.empty }
    |> metric logger

  /// Capture a timer metric with a given metric-level and metric-path.
  [<CompiledName "TimeLevel">]
  let timelvl (logger : Logger) lvl path f =
    if lvl < logger.Level then f ()
    else
      let now = Date.utcNow ()
      let sw = Stopwatch.StartNew()
      try
        f ()
      finally
        sw.Stop()
        { value     = sw.ElapsedTicks |> float
          path      = path
          timestamp = now
          level     = lvl
          mtype     = Timer(Duration.FromTicks(sw.ElapsedTicks))
          data      = Map.empty  }
        |> metric logger

  /// Capture a timer metric with a given metric-path
  [<CompiledName "Time">]
  let time logger path = timelvl logger LogLevel.Info path

  /// Capture a timer metric with the logger's name as the metric-path
  [<CompiledName "TimeLog">]
  let timeLog logger = timelvl logger LogLevel.Info (logger.Name)

  /// Time a function execution with a 'path' equal to the passed argument.
  /// Path may be null, and is then replaced with the logger name
  [<CompiledName "TimePath">]
  let timePath (logger : Logger) lvl path (f : System.Func<_>) =
    let path = match path with null -> logger.Name | p -> p
    timelvl logger lvl path (fun () -> f.Invoke())
