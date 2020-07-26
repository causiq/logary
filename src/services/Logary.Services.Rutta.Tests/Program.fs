module Logary.Services.Rutta.Tests.Program

open Expecto
open System
open Logary.Configuration

[<Tests>]
let tests =
  testList "rutta" [
    for (KeyValue (k, _)) in TargetConfig.schemeToConfAndDefault do
      yield testCase (sprintf "create target config for %s" k) <| fun () ->
        TargetConfig.create (Uri (sprintf "%s://./" k))
          |> ignore
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv