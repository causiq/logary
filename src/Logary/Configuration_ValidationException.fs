namespace Logary.Configuration

open Logary.Targets

/// Thrown from 'validateLogary' if the configuration is wrong
type ValidationException(msg, invalidRules : Rule list, invalidTargets : TargetConf list) =
  inherit System.Exception(msg)
  /// Gets the invalid rules that failed validation
  member x.InvalidRules   = invalidRules
  /// Gets the invalid tagets that failed validation
  member x.InvalidTargets = invalidTargets
