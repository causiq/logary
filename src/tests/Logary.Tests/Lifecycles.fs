module Logary.Tests.Lifecycles

open NodaTime
open Swensen.Unquote
open Fuchu
open Logary.Configuration
open Logary
open Logary.Logging
open Logary.Targets
open Logary.Metrics
open Hopac
open TestDSL

[<Tests>]
let tests =
  testList "lifecycles" [

    testCase "logary" <| fun _ ->
      Config.confLogary "tests"
      |> Config.validate
      |> Config.runLogary
      |> run
      |> Config.shutdownSimple
      |> run
      |> ignore

    testCase "target" <| fun _ ->
      let target =
        Target.confTarget (pn "tw") (TextWriter.create (TextWriter.TextWriterConf.Create(Fac.textWriter(), Fac.textWriter())))
        |> Target.validate
        |> Target.init Fac.emptyRuntime
      (Message.debug "Hello") |> Target.send target |> run |> ignore
      Target.shutdown target |> run |> ignore

    testCase "metric" <| fun _ ->
      Metric.confMetric
        (pn "no op metric")
        (Duration.FromMilliseconds 500L)
        (Noop.create { Noop.NoopConf.isHappy = true })
      |> Metric.validate
      |> Metric.init Fac.emptyRuntime
      |> Metric.update (Message.metric (pn "tests") (Float 1.0M))
      |> Metric.shutdown
      |> run
      |> ignore
    ]
