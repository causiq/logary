module Program

open System
open System.Threading
open Hopac
open Logary
open Logary.Targets
open Logary.Metric
open Logary.Metrics
open Logary.Configuration
open NodaTime

module Sample =

  let randomWalk pn : Job<Metric> =
    let reducer state = function
      | _ ->
        state

    let ticker (rnd : Random, prevValue) =
      let value =
        let v = rnd.NextDouble() * 0.3
        if v + prevValue < -1. || v + prevValue > 1. then -v + prevValue
        else v + prevValue

      let msg = Message.metric pn (Float value)

      (rnd, value), [ msg ]

    let state =
      let rnd = Random()
      rnd, rnd.NextDouble()

    Metric.create reducer state ticker

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  let influxConf =
    InfluxDb.create (InfluxDb.InfluxDbConf.create(Uri "http://192.168.99.100:8086/write", "logary"))
                    (PointName.ofSingle "influxdb")

  use logary =
    withLogaryManager "Logary.Examples.MetricsWriter" (
      withTargets [
        Console.create (Console.empty) (PointName.ofSingle "console")
        influxConf
      ] >>
      withMetrics [
        MetricConf.create (Duration.FromMilliseconds 500L) (PointName.ofSingle "henrik") Sample.randomWalk
      ] >>
      withRules [
        Rule.createForTarget (PointName.ofSingle "console")
        Rule.createForTarget (PointName.ofSingle "influxdb")
      ] >>
      withInternalTargets Info [
        Console.create Console.empty (PointName.ofSingle "console")
      ]
    )
    |> run

  mre.Wait()
  0

