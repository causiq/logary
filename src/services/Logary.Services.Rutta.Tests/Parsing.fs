module Logary.Services.Rutta.Tests.Parsing

open Expecto
open Expecto.Flip
open Logary
open Logary.Ingestion
open Logary.Services.Rutta
open Logary.Services.Rutta.Program

[<Tests>]
let commandLineParsing =
  testList "parse" [
    testList "parsers" [
      testCase "bindingString" <| fun _ ->
        let (Binding (scheme, nic, port)) = Parsers.bindingString "127.0.0.1:8080"
        port
          |> Expect.equal "Has 8080 as port" (Port 8080us)
        nic
          |> Expect.equal "Has loopback" (NIC "127.0.0.1")
    ]
  ]
  |> testLabel "rutta"
  |> testLabel "logary"
