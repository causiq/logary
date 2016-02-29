namespace Logary.Configuration

open NodaTime

open Hopac

open Logary
open Logary.Internals
open Logary.Target
open Logary.Metric

/// The logary configuration structure having a memory of all configured
/// targets as well as the rules that map log lines and metrics to them.
type LogaryConf =
  { /// A list of rules that guide what targets are invoked for a given
    /// message.
    rules       : Rule list

    /// A map of the targets by name. Some targets may not have been initialised
    /// (LogaryConf describes what-is-to-be before running it, thereafter it describes
    /// what has been configured, and the targets instances aren't None anymore.)
    targets     : Map<PointName, TargetConf * TargetInstance option>

    /// A map of metrics by name.
    metrics     : Map<PointName, MetricConf>

    /// Service metadata - what name etc.
    runtimeInfo : RuntimeInfo }

  static member rules_ =
    (fun x -> x.rules),
    fun v x -> { x with rules = v }

  static member targets_ =
    (fun x -> x.targets),
    fun v x -> { x with targets = v }

  static member runtimeInfo_ =
    (fun x -> x.runtimeInfo),
    fun v (x : LogaryConf) -> { x with runtimeInfo = v }

  static member metrics_ =
    (fun x -> x.metrics),
    fun v x -> { x with metrics = v }