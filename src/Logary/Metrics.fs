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

/// A meter measures the rate of events over time (e.g., “requests per second”).
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

open FSharp.Actor
open NodaTime

/// A MetricType is a discriminated union that specifies the type of the metric
/// collected.
type MetricType =
  | Gauge
  | Counter
  | Timer of Duration
with
  /// Custom ToString that corresponds to all-lowercase.
  override x.ToString() =
    match x with
    | Gauge   -> "gauge"
    | Counter -> "counter"
    | Timer _ -> "timer"

/// The thinking behind the format of Metric is that it is similar to
/// how Nimrod deals with it with logging: http://sbtourist.github.io/nimrod/
/// A data-point is used to log to some statistics based service.
/// Nimrod has second-resolution timers, but we're adding the Duration
/// as a piece of data on the metric type Timer, so that the nimrod target
/// doesn't have to rely on a loosely typed behaviour of the float value.
type Measure =
  { value     : float
  /// The identifier for the metric - defaults to the path of the logger sending it
  ; path      : string
  /// When the metric was captured (start of capture of metric)
  ; timestamp : Instant
  /// The level of the metric
  ; level     : LogLevel
  /// What type of metric it is
  ; mtype     : MetricType }

module Metrics =
  open FSharp.Actor

  type MetricInstance(name, targets) =
    member x.Targets : IActor list = targets
    member x.Name = name
    interface Named with
      member x.Name = name
