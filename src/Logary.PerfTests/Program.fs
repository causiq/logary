namespace Logary.PerfTests

open System
open System.Threading
open Expecto
open Expecto.Flip
open Logary
open Logary.Configuration
open Logary.Message
open BenchmarkDotNet
open BenchmarkDotNet.Code
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Attributes.Jobs
open BenchmarkDotNet.Attributes.Exporters
open BenchmarkDotNet.Attributes.Columns
open BenchmarkDotNet.Attributes.Jobs

[<AutoOpen>]
module Values =
  let run = Hopac.Hopac.run
  let targets =
    Map [
      "noop", Targets.Noop.create Targets.Noop.empty "sink"
      "badboy_single_nodelay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
      "badboy_single_delay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
      "badboy_batch_nodelay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
      "badboy_batch_delay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
    ]
  let baseJob =
    Job.Default
      .WithInvocationCount(20480)
      .WithWarmupCount(16)
      .WithLaunchCount(1)
      .WithGcServer(true)
      .WithGcConcurrent(true)

type ClrJobAttribute(isBaseline) =
  inherit JobConfigBaseAttribute(Job(Job.Clr, baseJob))
  new() = ClrJobAttribute(false)

type CoreJobAttribute(isBaseline) =
  inherit JobConfigBaseAttribute(Job(Job.Core, baseJob))
  new() = CoreJobAttribute(false)

[<Struct>]
type LogaryValue =
  val logger: Logger
  val target: string
  new (target: string) =
    let logary =
      Config.create "Logary.ConsoleApp" "localhost"
      |> Config.targets [ targets |> Map.find target ]
      |> Config.ilogger (ILogger.Console Warn)
      |> Config.processing (Events.events |> Events.sink [ "sink" ])
      |> Config.build
      |> run
    { logger = logary.getLogger (PointName [| "PerfTestLogger" |])
      target = target }

type LogaryParam(value: LogaryValue) =
  interface IParam with
    member x.Value = box value
    member x.DisplayText = sprintf "Logary with target=%s" value.target
    member x.ToSourceCode() = sprintf "new Logary.PerTests.LogaryValue(%s)" value.target

[<MemoryDiagnoser>]
[<ClrJob; CoreJob(isBaseline=true)>]
[<RPlotExporter; RankColumn>]
type LogEndToEndNoOutput() =

  [<ParamsSource("Configs"); DefaultValue>]
  val mutable logary: LogaryValue

  let toParam (x: IParam) = x

  member x.Configs() =
    [ "noop"; "badboy_single_nodelay" ]
    |> Seq.map (LogaryValue >> LogaryParam >> toParam)

  [<Benchmark>]
  member x.LogHelloWorld_WithAck() =
    run (x.logary.logger.debugWithAck (eventX "Hello world"))

  [<Benchmark>]
  member x.LogHelloWorld_WithBP() =
    run (x.logary.logger.debugWithBP (eventX "Hello world"))

  [<Benchmark>]
  member x.LogHelloWorld_Simple() =
    x.logary.logger.logSimple (eventX "Hello world" Debug)

module Tests =
  [<Tests>]
  let benchmarks =
    testList "benchmarks" [
      test "noop sink - baseline" {
        let summary =
          benchmark<LogEndToEndNoOutput> benchmarkConfig (id >> box)
        ()
      }
    ]

module Program =
  [<EntryPoint>]
  let main argv =
    Environment.SetEnvironmentVariable("System.GC.Server", "true")
    Environment.SetEnvironmentVariable("gcServer", "1")
    use cts = new CancellationTokenSource()
    runTestsInAssemblyWithCancel cts.Token defaultConfig argv
