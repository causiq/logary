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
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Attributes.Jobs
open BenchmarkDotNet.Attributes.Exporters
open BenchmarkDotNet.Attributes.Columns
open BenchmarkDotNet.Attributes.Jobs

module TestData =
  let helloWorld =
    eventX "Hello world"

  let multiGaugeMessage level =
    Message.event level "Processor.% Idle"
    |> Message.addGauges [
      "Core 1", (Gauge (Fraction (1L, 1000L), Percent))
      "Core 2", (Gauge (Float 0.99, Percent))
      "Core 3", (Gauge (Float 0.473223755, Percent))
    ]
    |> Message.setContext "host" "db-001"
    |> Message.setContext "service" "api-web"

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
      .WithInvocationCount(800)
      .WithWarmupCount(4)
      .WithLaunchCount(1)
      .WithIterationTime(TimeInterval.Millisecond * 200)
      .WithGcServer(true)
      .WithGcConcurrent(true)

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

type BP() =
  [<ParamsSource("Configs"); DefaultValue>]
  val mutable logary: LogaryValue

  let toParam (x: IParam) = x

  member x.Configs() =
    [ "single_nodelay"; "batch_delay" ]
    |> Seq.map (LogaryValue >> LogaryParam >> toParam)

  [<Benchmark>]
  member x.wBP() =
    run (x.logary.logger.warnWithBP TestData.multiGaugeMessage)

type ACK() =
  [<ParamsSource("LogaryConfigs"); DefaultValue>]
  val mutable logary: LogaryValue

  let toParam (x: IParam) = x

  member x.LogaryConfigs() =
    [ "single_nodelay"; "batch_delay" ]
    |> Seq.map (LogaryValue >> LogaryParam >> toParam)

  [<Benchmark>]
  member x.wACK() =
    run (x.logary.logger.warnWithAck TestData.multiGaugeMessage)


  //[<Benchmark>]
  //member x.simp() =
  //  x.logary.logger.logSimple (TestData.multiGaugeMessage Warn)

module Tests =
  open BenchmarkDotNet.Diagnosers
  open BenchmarkDotNet.Exporters.Csv
  open BenchmarkDotNet.Reports

  // http://adamsitnik.com/the-new-Memory-Diagnoser/
  // https://benchmarkdotnet.org/Advanced/Params.htm
  // https://github.com/logary/logary/pull/323/files
  [<Tests>]
  let benchmarks =
    let config xJ =
      { benchmarkConfig with
          exporters =
            [ new CsvExporter(CsvSeparator.Comma)
              new Exporters.HtmlExporter()
              new Exporters.RPlotExporter()
            ]
          diagnosers =
            [ new MemoryDiagnoser() ]
          jobs = [ xJ ]
      }

    testList "benchmarks" [
      test "backpressure" {
        let cfg =
          create (Job(Job.Core, baseJob))
        let summary: Summary = benchmark<BP> config (id >> box) |> unbox
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
