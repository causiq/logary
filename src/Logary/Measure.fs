namespace Logary

open FSharp.Actor
open NodaTime

/// A MetricType is a discriminated union that specifies the type of the metric
/// collected.
type MetricType =
  | Counter
  /// A timer is a Meter, but carries with it a more precise unit (that's all
  /// that's different). In this case the `value` of the `Measure` would be the
  /// same as this duration - in milliseconds (?).
  | Timer of Duration
  | Gauge
  | Meter
with
  /// Custom ToString that corresponds to all-lowercase.
  override x.ToString() =
    match x with
    | Gauge   -> "gauge"
    | Counter -> "counter"
    | Meter   -> "meter"
    | Timer _ -> "timer"

/// The thinking behind the format of Metric is that it is similar to
/// how Nimrod deals with it with logging: http://sbtourist.github.io/nimrod/
/// A data-point is used to log to some statistics based service.
/// Nimrod has second-resolution timers, but we're adding the Duration
/// as a piece of data on the metric type Timer, so that the nimrod target
/// doesn't have to rely on a loosely typed behaviour of the float value.
///
/// Besides nimrod, you can also attach arbitrary metadata to a metric that can
/// be used for filtering, aggregating etc at the point of collection, e.g. in
/// Riemann.
type Measure =
  { /// The value of the measurement
    value     : float
    /// The identifier for the measure - defaults to the path of the logger
    /// sending it -- this is also the 'name' of the measeure.
    path      : string
    /// When the measurement was taken (start of capture of metric)
    timestamp : Instant
    /// The level of the metric
    level     : LogLevel
    /// What type of metric it is
    mtype     : MetricType
    /// A map of string to anything; it is up to the target in question how to
    /// handle extra data on the metric
    data     : Map<string, obj> }
