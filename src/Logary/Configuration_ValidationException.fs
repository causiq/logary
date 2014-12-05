namespace Logary.Configuration

open System
open System.Diagnostics

open Logary
open Logary.Target
open Logary.Metric

type ValidationError = string

/// Thrown from `validate` if the configuration is wrong.
[<DebuggerDisplay("{ToString()}")>]
type ValidationException(rErr : ValidationError, invalidRules : Rule Set,
                         tErr : ValidationError, invalidTargets : TargetConf Set,
                         mErr : ValidationError, invalidMetrics : MetricConf Set) =
  inherit Exception()

  let message =
    let join xs = xs |> Set.map (sprintf "%O") |> String.concat ", "
    let present = function | "" -> "" | err -> sprintf " - %s" err
    sprintf "Validation of the logary instantiation failed:\n\
             InvalidRules: [%s]%s\n\
             InvalidTargets: [%s]%s\n\
             InvalidMetrics: [%s]%s"
      (join invalidRules) (present rErr)
      (join invalidTargets) (present tErr)
      (join invalidMetrics) (present mErr)

  /// Gets the invalid rules that failed validation by means of having no
  /// targets configured for their 'target' properties.
  member x.InvalidRules   = invalidRules

  /// Gets the invalid tagets that failed validation by not having any rules
  /// that points to them.
  member x.InvalidTargets = invalidTargets

  /// The invalid metrics are such that there would be no loggers available to
  /// log their output: and loggers are created from the 'hieras' that are
  /// specified in the rules. Make sure you have given adequate rules to match
  /// the names of the metrics. This is different from how rules/names of
  /// loggers are normally correlated; by means of GetLogger(name); as opposed
  /// to a registered component's name (the metric's).
  member x.InvalidMetrics = invalidMetrics

  override x.Message = message

  override x.ToString() = message