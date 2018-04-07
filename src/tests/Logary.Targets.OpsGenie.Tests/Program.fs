module Logary.Targets.OpsGenie.Tests.Program

open Logary
open Logary.Tests
open Logary.Targets.OpsGenie
open Expecto

[<Tests>]
let tests =
  TargetBaseline.basicTests "OpsGenie" (create empty)

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv