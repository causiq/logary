module Logary.PerfTests.Program

open System.Threading
open Expecto
open Expecto.Flip

open Logary
open Logary.Configuration
open Logary.Message
open System

let run = Hopac.Hopac.run

open BenchmarkDotNet
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Attributes.Jobs
open BenchmarkDotNet.Attributes.Exporters
open BenchmarkDotNet.Attributes.Columns
open BenchmarkDotNet.Attributes.Jobs

let baseJob =
  Job.Default
    .WithInvocationCount(1024)
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

[<ClrJob; CoreJob(isBaseline=true)>]
[<RPlotExporter; RankColumn>]
type LogEndToEndNoOutput() =
  let logary =
    Config.create "Logary.ConsoleApp" "localhost"
    |> Config.targets [ Targets.Noop.create Targets.Noop.empty "noop" ]
    |> Config.ilogger (ILogger.Console Warn)
    |> Config.processing (Events.events |> Events.sink [ "noop" ])
    |> Config.build
    |> run

  let logger = logary.getLogger (PointName [| "EndToEndNoOutput" |])

  [<Benchmark>]
  member x.LogHelloWorld_WithAck() =
    run (logger.debugWithAck (eventX "Hello world"))

  [<Benchmark>]
  member x.LogHelloWorld_WithBP() =
    run (logger.debugWithBP (eventX "Hello world"))

  [<Benchmark>]
  member x.LogHelloWorld_Simple() =
    logger.logSimple (eventX "Hello world" Debug)

[<Tests>]
let benchmarks =
  testList "benchmarks" [
    test "log end to end with no output" {
      benchmark<LogEndToEndNoOutput> benchmarkConfig (fun summary -> null)
        |> ignore
    }
  ]

[<EntryPoint>]
let main argv =
  Environment.SetEnvironmentVariable("System.GC.Server", "true")
  Environment.SetEnvironmentVariable("gcServer", "1")
  use cts = new CancellationTokenSource()
  runTestsInAssemblyWithCancel cts.Token defaultConfig argv
