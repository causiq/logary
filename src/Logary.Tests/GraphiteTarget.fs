module Logary.Tests.GraphiteTarget

open Fuchu
open Swensen.Unquote

open System.Text.RegularExpressions

open Fac

open Logary
open Logary.Target
open Logary.Targets
open Logary.Internals
open Logary.Internals.Tcp

open Logary.Tests.StubTcp
open Logary.Tests.TestDSL

[<Tests>]
let tests =
  testList "graphite target" [
    testCase "initialising" <| fun _ ->
      let client = new StubWriterClient(false)
      let conf = Graphite.GraphiteConf.Create("localhost", clientFac = fun a b -> client :> WriteClient)
      let graphite = Graphite.create conf "graphite-target"
      let instance = graphite.initer { serviceName = "tests"; logger = NullLogger() }
      instance.name =? "graphite-target"

      (because "shutting down the target" <| fun () ->
        instance |> finaliseTarget
        client.WasDisposed)
      |> should be true
      |> thatsIt

    ]
