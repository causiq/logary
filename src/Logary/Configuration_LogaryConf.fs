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
    /// log line or measure.
    rules    : Rule list

    /// A map of the targets by name. Some targets may not have been initialised
    /// (LogaryConf describes what-is-to-be before running it, thereafter it describes
    /// what has been configured, and the targets instances aren't None anymore.)
    targets  : Map<string, TargetConf * Option<TargetInstance>>

    /// Service metadata - what name etc.
    metadata : RuntimeInfo

    /// A map of metrics by name
    metrics  : Map<string, MetricConf * Option<MetricInstance>>

    /// how often do we poll metrics
    pollPeriod : Duration }

  static member rules_ =
    (fun x -> x.rules),
    fun v x -> { x with rules = v }

  static member targets_ =
    (fun x -> x.targets),
    fun v x -> { x with targets = v }

  static member metadata_ =
    (fun x -> x.metadata),
    fun v (x : LogaryConf) -> { x with metadata = v }

  static member metrics_ =
    (fun x -> x.metrics),
    fun v x -> { x with metrics = v }

  static member pollPeriod_ =
    (fun x -> x.pollPeriod),
    fun v x -> { x with pollPeriod = v }