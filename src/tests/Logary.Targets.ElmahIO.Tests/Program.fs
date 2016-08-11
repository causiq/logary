module Program

open System
open Fuchu
open Hopac
open Logary
open Logary.Targets
open Logary.Tests.Targets
open Logary.Tests

let target =
  ElmahIO.create { logId = envForce "ELMAH_IO_LOG_ID" Guid.Parse }

[<Tests>]
let tests =
  testList "elmah.io tests" [
    Targets.basicTests "elmah.io" target
    Targets.integrationTests "elmah.io" target
  ]

[<EntryPoint>]
let main argv =
  Tests.defaultMainThisAssembly argv