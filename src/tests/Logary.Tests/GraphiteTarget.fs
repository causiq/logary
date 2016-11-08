module Logary.Tests.GraphiteTarget

open NodaTime
open Expecto
open System.Text.RegularExpressions
open Hopac
open Fac
open Logary
open Logary.Target
open Logary.Targets
open Logary.Tests.Targets
open Logary.Internals
open Logary.Tests.TestDSL

[<Tests>]
let tests =
  testList "graphite target" [
    testCase "initialising" <| fun _ ->
      Tests.skiptest "until custom tcp"
      let conf = Graphite.GraphiteConf.create("localhost")
      let graphite = Graphite.create conf "graphite-target"
      let instance =
        graphite.initer Fac.emptyRuntime
        |> run

      //start instance.server
      Expect.equal instance.name (PointName.ofSingle "graphite-target")
                   "instance name should match"

      (because "shutting down the target" <| fun () ->
        Target.finalise instance
        true)
      |> should be true
      |> thatsIt

    testCase "sanitizePath" <| fun _ ->
      let testPath = PointName.ofList ["This is a metric path"; "path$Section%2.5"; "GET /post/1.2/"; "Multiple . spaces"]
      let sanitised = Graphite.Impl.sanitisePath testPath
      let expected = PointName.ofList ["This_is_a_metric_path"; "path$Section%2_5"; "GET_-post-1_2-"; "Multiple___spaces"]
      Expect.equal sanitised expected "sanitised equals"
    ]
