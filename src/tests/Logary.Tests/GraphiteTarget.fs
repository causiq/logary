module Logary.Tests.GraphiteTarget

open Fuchu
open Swensen.Unquote
open System.Text.RegularExpressions
open Hopac
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
      let graphite = Graphite.create conf (pn "graphite-target")
      let instance = graphite.initer { serviceName = "tests"; logger = NullLogger() } |> run
      instance.name =? (pn "graphite-target")

      (because "shutting down the target" <| fun () ->
        instance |> finaliseTarget
        client.WasDisposed)
      |> should be true
      |> thatsIt

    testCase "sanitizePath" <| fun _ ->
      let testPath = PointName.ofList ["This is a metric path"; "path$Section%2.5"; "GET /post/1.2/"; "Multiple . spaces"]
      Graphite.Impl.sanitisePath testPath =? (PointName.ofList ["This_is_a_metric_path"; "path$Section%2_5"; "GET_-post-1_2-"; "Multiple___spaces"])
    ]
