module Logary.Services.Rutta.Tests.Parsing

open Expecto
open Logary.Services.Rutta
open Logary.Services.Rutta.Program

[<Tests>]
let commandLineParsing =
  testList "parse" [
    testList "parsers" [
      testCase "binding" <| fun _ ->
        ignore (Parsers.binding "127.0.0.1:8080")
    ]
  ]
  |> testLabel "rutta"
