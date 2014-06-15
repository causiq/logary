/// Module for dealing with rules
module Logary.Rules

open System
open System.Text.RegularExpressions

open Logary.Targets

/// Find all rules matching the name, from the list of rules passed.
let matching (name : string) (rules : Rule list) =
  rules |> List.filter (fun r -> r.hiera.IsMatch name)

/////////////////////
// Creating rules: //
/////////////////////

/// Create a rule with the given regex/hiera, accept function and level given a target configuration.
let forTarget hiera accept level (t : TargetConf) =
  { hiera  = hiera
    target = t.name
    accept = accept
    level  = level }

/// Create a rule that accepts any input for a specified target (that's the name param)
let forAny (name : string) =
  { hiera  = Regex ".*"
    target = name
    accept = fun _ -> true
    level  = Verbose }
