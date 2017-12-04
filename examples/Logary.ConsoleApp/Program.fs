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
open System.Diagnostics
open NodaTime
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Metrics
open Logary.EventsProcessing
open Logary.EventsProcessing.Transformers


open System.Threading

module ParallelJob =
  open Hopac.Infixes

  let mutable numSpawns = 0L

  let rec fib n = job {
    if n < 2L then
      return n
    else
      let! (x, y) = fib (n-2L) <*> fib (n-1L)
      return x + y
  }

  let run n =
    let timer = Stopwatch.StartNew ()
    let r = run (fib n)
    let d = timer.Elapsed
    printfn "ParJob: %d - %fs (%f jobs/s)\n"
     r d.TotalSeconds (float numSpawns / d.TotalSeconds)

module SerialFun =

  let mutable numSpawns = 0L
  let rec fib n =
    if n < 2L then
      n
    else
      numSpawns <- numSpawns + 1L
      fib (n-2L) + fib (n-1L)

  let run n =
    printfn "Running SerialFun"
    numSpawns <- 0L
    let timer = Stopwatch.StartNew ()
    let r = fib n
    let d = timer.Elapsed
    printfn "SerFun: %d - %fs (%d recs)\n" r d.TotalSeconds numSpawns
    ()

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

      (rnd, value), [| msg |]

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

  let metric (timing : Timing) name =
    let minName = name |> PointName.setEnding "min"
    let maxName = name |> PointName.setEnding "max"
    let medianName = name |> PointName.setEnding "median"
    let countName = name |> PointName.setEnding "count"
    let upper95 = name |> PointName.setEnding "upper_95"

    let reduce state = function
      | Float v, Seconds ->
        int64 (v * float Constants.NanosPerSecond) :: state
      | Int64 v, Scaled (Seconds, scale) as input when scale = float Constants.NanosPerSecond ->
        v :: state
      | _ -> state

    let tick state =
      let snap = Snapshot.create (Array.ofList state)
      [], [| snap |> Snapshot.size |> int64 |> Int64 |> Message.gauge countName
             snap |> Snapshot.median |> int64 |> Int64 |> Message.gauge medianName
             snap |> Snapshot.min |> Int64 |> Message.gauge minName
             snap |> Snapshot.max |> Int64 |> Message.gauge maxName
             snap |> Snapshot.percentile95th |> int64 |> Int64|> Message.gauge upper95 |]

    job {
      let! metric = Metric.create reduce [] tick
      do! metric |> Metric.consume (Stream.Src.tap timing.source)
      return metric }

module NormalUsage =
  open Hopac.Infixes

  let act (logger : Logger) timing randomness =
    Message.templateFormat("{userName} logged in", [| "haf" |])
    |> Logger.logSimple logger

    Message.eventFormat (Info, "{userName} logged in", [| "adam" |])
    |> Logger.logSimple logger

    //Stopwatch.time (fun () -> SerialFun.run 42L)
    //|> Timing.report timing
    //|> run

    queue (randomness >>- Metric.tap >>- Stream.consumeJob (Timing.update timing))


[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
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

  /// This defines a timing stream source
  let timing =
    Timing.create ()

  let randomness =
    memo (RandomWalk.create (PointName.parse "Logary.ConsoleApp.randomWalk"))

  use logary =
    withLogaryManager "Logary.ConsoleApp" (
      withTargets [
        LiterateConsole.create LiterateConsole.empty "console"
        Console.create Console.empty "fatal"
        //RabbitMQ.create rmqConf "rabbitmq"
        InfluxDb.create (InfluxDb.InfluxDbConf.create(Uri "http://192.168.99.100:8086/write", "logary", batchSize = 500us))
                        "influxdb"
      ] >>
      withMetrics [
        MetricConf.create (Duration.FromSeconds 10L) "Logary.ConsoleApp.sampleTiming" (Timing.metric timing)
        MetricConf.create (Duration.FromMilliseconds 500L) "Logary.ConsoleApp.randomWalk" (fun _ -> upcast randomness)
        //WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "cpuTime" (Duration.FromMilliseconds 500L)
        MetricConf.create (Duration.FromMilliseconds 5000L) "app" WinPerfCounters.appMetrics
        MetricConf.create (Duration.FromMilliseconds 5000L) "system" WinPerfCounters.systemMetrics
        //MetricConf.create (Duration.FromMilliseconds 500L) "gpu" Sample.m6000s
      ] >>
      withRules [
        Rule.createForTarget "console"
        |> Rule.setHieraString "sampleTiming$"
        
        Rule.createForTarget "console" |> Rule.setHieraString "app|system"
        Rule.createForTarget "influxdb" |> Rule.setHieraString "app|system"

        Rule.createForTarget "fatal"
        |> Rule.setLevel Fatal
        //Rule.createForTarget "rabbitmq"
        //Rule.createForTarget "influxdb"
      ] >>
      withInternalTargets Info [
        Console.create Console.empty "console"
      ] >>
      withMiddleware (fun next msg ->
        msg
        |> Message.setContextValue "host" (String (System.Net.Dns.GetHostName()))
        |> next)
    )
    |> run

  let logger =
    logary.getLogger (PointName [| "Logary"; "Samples"; "main" |])


  NormalUsage.act logger timing randomness

  mre.Wait()
  0
