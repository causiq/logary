module Logary.Targets.OpsGenie.Tests.Program

open Logary
open Logary.Tests
open Logary.Targets.OpsGenie
open Expecto

let conf =
  { empty with apiKey = "3f0db134-19fe-4f59-92bc-92f5ad8b8cf3" }

[<Tests>]
let tests =
  TargetBaseline.basicTests "OpsGenie" (create conf)

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv