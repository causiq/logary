namespace Logary.Metric

/// Most simple: just represents a value at an instant
module Gauge =
  open FSharp.Actor
  open Logary
  open Targets
  open Metrics
  open Logary.Internals.Date

  /// Construct a new metric gauge at the instant this function is called
  /// with the passed value and name
  let gauge name value =
    { value     = value
    ; path      = name
    ; timestamp = utcNow ()
    ; level     = LogLevel.Info
    ; mtype     = Gauge }

  /// Logary's representation of a gauge.
  type GaugeInstance(name, targets) =
    inherit MetricInstance(name, targets)
    with
      interface Gauge with
        member x.Put value = x.Targets <-* Metric( gauge name value )

  /// Used by the registry to convert the target instance list to a
  /// gauge instance.
  let internal fromTargets name (targets : (_ * TargetInstance * _) list) =
    GaugeInstance(name, targets |> List.map (fun (_, ti, _) -> actor ti))

  /// Put the gauge metric to the gauge instance.
  /// (Would like to slice the metric on mtype and have only the Gauge union case
  /// of mtype possible to pass here.)
  let put (gauge : GaugeInstance) value =
    gauge.Targets <-* Metric value

  /// Put the gauge float metric to the gauge instance.
  let putf (gauge : GaugeInstance) value =
    (gauge :> Gauge).Put value
