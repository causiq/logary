#if !INTERACTIVE
module Logary.ConsoleApp.Program
#else
#I "bin/Release"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "NodaTime.dll"
#r "Logary.dll"
#r "Logary.Riemann.dll"
#endif

open System
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Metrics
open Logary.EventsProcessing
open Logary.EventsProcessing.Transformers

module RandomWalk =

  let create pn =
    let reducer state = function
      | _ ->
        state

    let ticker (rnd : Random, prevValue) =
      let value =
        let v = (rnd.NextDouble() - 0.5) * 0.3
        if abs v < 0.03 then rnd.NextDouble() - 0.5
        elif v + prevValue < -1. || v + prevValue > 1. then -v + prevValue
        else v + prevValue

      let msg = Message.gaugeWithUnit pn value Seconds

      (rnd, value), msg

    let state =
      let rnd = Random()
      rnd, rnd.NextDouble()

    Ticker.create state reducer ticker

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Timing =

  open Hopac.Infixes

  type Timing =
    private { source: Stream.Src<Value * Units> }

  let create () = { source = Stream.Src.create() }

  let update timing (v, u) = Stream.Src.value timing.source (v, u)

  let report timing (x,v) = update timing v >>-. x

  let reportAsync timing result = async.Bind(result, Job.toAsync << report timing)

  let metric name =
    let minName = name |> PointName.setEnding "min" |> PointName.format
    let maxName = name |> PointName.setEnding "max" |> PointName.format
    let medianName = name |> PointName.setEnding "median" |> PointName.format
    let countName = name |> PointName.setEnding "count" |> PointName.format
    let upper95 = name |> PointName.setEnding "upper_95" |> PointName.format

    let reduce state = function
      | Gauge(v, Seconds) ->
        (v * float Constants.NanosPerSecond) :: state
      | Gauge(v, _) -> v :: state

    let tick state =
      let snap = Snapshot.create (state |> List.map int64 |> List.toArray)
      [], [| snap |> Snapshot.size |> float |>  Message.gauge countName
             snap |> Snapshot.median |> float |>  Message.gauge medianName
             snap |> Snapshot.min |> float |> Message.gauge minName
             snap |> Snapshot.max |> float |> Message.gauge maxName
             snap |> Snapshot.percentile95th |> float |> Message.gauge upper95 |]

    Ticker.create [] reduce tick

module NormalUsage =

  let act (logger : Logger) =
    Message.templateFormat("{userName} logged in", "haf")
    |> Logger.logSimple logger

    Message.eventFormat (Info, "{userName} logged in", "adam")
    |> Logger.logSimple logger


[<EntryPoint>]
let main argv =
  use mre = new System.Threading.ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  // sample configuration of a RMQ target
  let rmqConf =
    { RabbitMQ.empty with
        appId = Some "Logary.ConsoleApp"
        username = "appuser-12345"
        password = "TopSecret1234"
        tls = { RabbitMQ.TlsConf.certPath = "./certs/mycert.pfx"
                RabbitMQ.TlsConf.certPassword = Some "AnotherSecret1243567" }
              |> Some
        compression = RabbitMQ.Compression.GZip
    }

  let randomness =
    RandomWalk.create "Logary.ConsoleApp.randomWalk"

  let timing = 
    Timing.metric (PointName.parse "Logary.ConsoleApp.sampleTiming")
  
  let randomWalkPipe =
    Events.events
    |> Pipe.tickTimer (randomness) (TimeSpan.FromMilliseconds 500.)

  let processing =
    Events.compose [
      Events.events |> Events.miniLevel LogLevel.Fatal |> Events.sink ["fatal"]

      Events.events
      |> Pipe.tickTimer (WinPerfCounters.appMetrics (PointName.ofSingle "app")) (TimeSpan.FromMilliseconds 5000.)
      |> Pipe.map Array.toSeq
      |> Events.flattenToProcessing
      |> Events.sink ["console"; "influxdb"]

      Events.events
      |> Pipe.tickTimer (WinPerfCounters.systemMetrics (PointName.ofSingle "system")) (TimeSpan.FromMilliseconds 5000.)
      |> Pipe.map Array.toSeq
      |> Events.flattenToProcessing
      |> Events.sink ["console"; "influxdb"]

      randomWalkPipe 
      |> Events.sink ["console"; "influxdb"]

      randomWalkPipe
      |> Pipe.choose (Message.tryGetGauge "Logary.ConsoleApp.randomWalk")
      |> Pipe.tickTimer timing (TimeSpan.FromSeconds 10.)
      |> Pipe.map Array.toSeq
      |> Events.flattenToProcessing
      |> Events.sink ["console";]
    ]

  let logary =
    Config.create "Logary.ConsoleApp" "localhost"
    |> Config.targets [
        LiterateConsole.create LiterateConsole.empty "console"
        Console.create Console.empty "fatal"
        RabbitMQ.create rmqConf "rabbitmq"
        InfluxDb.create (InfluxDb.InfluxDbConf.create(Uri "http://192.168.99.100:8086/write", "logary", batchSize = 500us))
                        "influxdb"
      ]
    |> Config.ilogger (ILogger.Console Info)
    |> Config.middleware Middleware.dnsHost
    |> Config.processing processing
    |> Config.build
    |> run

  let logger = logary.getLogger (PointName [| "Logary"; "Samples"; "main" |])

  
  NormalUsage.act logger

  mre.Wait()
  0
