namespace Logary.Configuration

open Logary
open Logary.Target
open Logary.Metric

/// Thrown from 'validateLogary' if the configuration is wrong
type ValidationException(msg,
                         invalidRules   : Rule Set,
                         invalidTargets : TargetConf Set,
                         invalidMetrics : MetricConf Set) =
  inherit System.Exception(msg)
  /// Gets the invalid rules that failed validation
  member x.InvalidRules   = invalidRules
  /// Gets the invalid tagets that failed validation
  member x.InvalidTargets = invalidTargets
  /// Gets the invalid metrics which didn't have any attached rules
  member x.InvalidMetrics = invalidMetrics

  /// Builds a little string from the validation exception
  override x.ToString() =
    sprintf "ValidationException: %s\nInvalidRules: %A\nInvalidTargets: %A"
      x.Message x.InvalidRules x.InvalidTargets