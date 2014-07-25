module Logary.Tests.Lifecycles

open NodaTime
open Swensen.Unquote
open Fuchu

open Logary.Configuration

open Logary
open Logary.Logging
open Logary.Targets
open Logary.Metrics

open TestDSL

[<Tests>]
let tests =
  testList "lifecycles" [

    testCase "logary" <| fun _ ->
      Config.confLogary "tests"
      |> Config.validate
      |> Config.runLogary
      |> Config.shutdown
      |> run |> ignore

    testCase "target" <| fun _ ->
      Target.confTarget "tw" (TextWriter.create (TextWriter.TextWriterConf.Default(Fac.textWriter(), Fac.textWriter())))
      |> Target.validate
      |> Target.init Fac.emptyRuntime
      |> Target.send (LogLine.debug "Hello")
      |> Target.shutdown
      |> run |> ignore

    testCase "metric" <| fun _ ->
      Metric.confMetric
        "no op metric"
        (Duration.FromMilliseconds 500L)
        (NoopMetric.create { NoopMetric.NoopConf.isHappy = true })
      |> Metric.validate
      |> Metric.init Fac.emptyRuntime
      |> Metric.update (Measure.create "tests" 1.)
      |> Metric.shutdown
      |> run |> ignore
    ]
