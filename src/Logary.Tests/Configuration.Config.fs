module Logary.Tests.Config

open Logary.Configuration
open Logary.Model
open Expecto

[<Tests>]
let tests =
  testList "config" [
    testCase "create" <| fun () ->
      Resource.create("tests", "hostname-123")
        |> Config.create
        |> ignore
  ]
  |> testLabel "logary"