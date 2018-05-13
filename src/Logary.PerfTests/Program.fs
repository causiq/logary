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
      "single_nodelay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
      "single_delay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
      "batch_nodelay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
      "batch_delay", Targets.BadBoy.create Targets.BadBoy.empty "sink"
    ]
  let baseJob =
    Job.Default
      .WithInvocationCount(20_000)
      .WithWarmupCount(4)
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
      |> Config.target (targets |> Map.find target)
      |> Config.ilogger (ILogger.LiterateConsole Verbose)
      |> Config.processing (Events.events |> Events.sink [ "sink" ])
      |> Config.loggerMinLevel ".*" Debug
      |> Config.build
      |> run
    { logger = logary.getLogger (PointName [| "PerfTestLogger" |])
      target = target }

type LogaryParam(value: LogaryValue) =
  interface IParam with
    member x.Value = box value
    member x.DisplayText = sprintf "Logary_%s" value.target
    member x.ToSourceCode() = sprintf "new LogaryValue(\"%s\")" value.target

[<MemoryDiagnoser>]
//[<ClrJob; CoreJob(isBaseline=true)>]
[<CoreJob(isBaseline=true)>]
[<RPlotExporter; RankColumn>]
type Lgry() =

  [<ParamsSource("Configs"); DefaultValue>]
  val mutable logary: LogaryValue

  let toParam (x: IParam) = x

  member x.Configs() =
    [ "single_nodelay"
      "batch_delay"
    ]
    |> Seq.map (LogaryValue >> LogaryParam >> toParam)

  [<Benchmark>]
  member x.wACK() =
    run (x.logary.logger.warnWithAck (eventX "Hello world"))

  [<Benchmark>]
  member x.wBP() =
    run (x.logary.logger.warnWithBP (eventX "Hello world"))

  [<Benchmark>]
  member x.simp() =
    x.logary.logger.logSimple (eventX "Hello world" Warn)

module Tests =
  // http://adamsitnik.com/the-new-Memory-Diagnoser/
  // https://benchmarkdotnet.org/Advanced/Params.htm
  // https://github.com/logary/logary/pull/323/files
  [<Tests>]
  let benchmarks =
    testList "benchmarks" [
      test "noop sink - baseline" {
        let summary =
          benchmark<Lgry> benchmarkConfig (id >> box)
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
