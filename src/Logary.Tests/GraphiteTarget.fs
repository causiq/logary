module Logary.Tests.GraphiteTarget

open Fuchu
open Swensen.Unquote

open System.Text.RegularExpressions

open Fac

open Logary
open Logary.Configuration
open Logary.Target
open Logary.Targets
open Logary.Measure
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
//
// TODO: unit test graphite target
//    testCase "expecting output in form 'metric_path value timestamp\n'" <| fun _ ->
//      let client = new StubWriterClient(false)
//      let conf = Graphite.GraphiteConf.Create("localhost", clientFac = fun a b -> client :> WriteClient)
//      let graphite = Graphite.create conf "graphite-target"
//
//      let logary =
//        confLogary "tests"
//        |> withRule ( graphite |> Rules.forTarget (Regex(".*")) (fun x -> true) Info )
//        |> withTarget graphite
//        |> validateLogary
//        |> runLogary
//
//      let gauge = "test-gauge" |> Registry.getGauge logary.registry |> Async.RunSynchronously
//      4.2 |> Gauge.putf gauge
//      logary |> shutdownLogary |> Async.RunSynchronously |> ignore
//
//      Assert.Equal("string should match", true, Regex.IsMatch (client.ToString(), @"test\-gauge 4\.2 \d+"))

    ]
