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


module DerivedSample =

  /// This sample demonstrates how to create a derived metric from other simpler
  /// ones. It generates an exponentially weighted moving average from
  /// login gauges. The login gauges are sent one-by-one from the login code.
  ///
  /// By wrapping it up like this, you can drastically reduce the amount of code
  /// a given service sends by pre-computing much of it.
  ///
  /// It's also a good sample of reservoir usage; a fancy name of saying that
  /// it's an algorithm which works on more than one gauge at a time, to produce
  /// a derived metric.
  let loginLoad : Job<Stream<Message>> = job {
    let! counter = Counters.counter (PointName.ofSingle "logins")
    let! ewma = Reservoirs.ewma (PointName.ofSingle "loginsEWMA")
    do! ewma |> Metric.consume (Metric.tap counter)
    return Metric.tapMessages ewma
  }

module Sample =

  let randomWalk pn : Job<Metric> =
    let reducer state = function
      | _ ->
        state

    let ticker (rnd : Random, prevValue) =
      let value =
        let v = (rnd.NextDouble() - 0.5) * 0.3
        if abs v < 0.03 then rnd.NextDouble() - 0.5
        elif v + prevValue < -1. || v + prevValue > 1. then -v + prevValue
        else v + prevValue

      let msg = Message.gauge pn (Float value)

      (rnd, value), [ msg ]

    let state =
      let rnd = Random()
      rnd, rnd.NextDouble()

    Metric.create reducer state ticker

[<EntryPoint>]
let main argv =
  let inline ms v = Duration.FromMilliseconds (int64 v)
  let pn name = PointName [| "Logary"; "Samples"; name |]
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  let influxConf =
    InfluxDb.create (InfluxDb.InfluxDbConf.create(Uri "http://192.168.99.100:8086/write", "logary", batchSize = 500us))
                    "influxdb"

  use logary =
    withLogaryManager "Logary.Examples.MetricsWriter" (
      withTargets [
        Console.create (Console.empty) "console"
        influxConf
      ]
      >> withMetrics [
        MetricConf.create (ms 500) "randomWalk" Sample.randomWalk
      ]
      >> withRules [
        Rule.createForTarget "console"
        Rule.createForTarget "influxdb"
      ]
      >> withInternalTargets Info [
        Console.create Console.empty "console"
      ]
      >> run
    )
    |> run

  mre.Wait()
  0