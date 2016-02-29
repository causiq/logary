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
                         tErr : ValidationError, invalidTargets : TargetConf Set) =
  inherit Exception()

  let message =
    let join xs = xs |> Set.map (sprintf "%O") |> String.concat ", "
    let present = function | "" -> "" | err -> sprintf " - %s" err
    sprintf "Validation of the logary instantiation failed:\n\
             InvalidRules: [%s]%s\n\
             InvalidTargets: [%s]%s"
      (join invalidRules) (present rErr)
      (join invalidTargets) (present tErr)

  /// Gets the invalid rules that failed validation by means of having no
  /// targets configured for their 'target' properties.
  member x.InvalidRules   = invalidRules

  /// Gets the invalid tagets that failed validation by not having any rules
  /// that points to them.
  member x.InvalidTargets = invalidTargets

  override x.Message = message

  override x.ToString() = message