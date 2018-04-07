module Logary.Targets.OpsGenie.Tests.Program

open Logary
open Logary.Tests
open Logary.Targets.OpsGenie
open Expecto

let conf =
  { empty with apiKey = "6eb5f896-5836-48a4-a079-8881679c5327" }

[<Tests>]
let tests =
  TargetBaseline.basicTests "OpsGenie" (create conf)

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv