module Logary.Tests.Targets.Diagnostics

open Expecto
open Logary.Targets
open Logary.Tests

[<Tests>]
let tests =
  testList "diagnostics" [
    TargetBaseline.basicTests "System.Diagnostics.Trace" (DiagnosticsTrace.create DiagnosticsTrace.empty) false
  ]
  |> testLabel "targets"
  |> testLabel "logary"
