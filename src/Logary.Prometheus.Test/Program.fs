// Learn more about F# at http://fsharp.org

open System

open System
open System.Threading
open Expecto
open BenchmarkDotNet.Code
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Attributes.Exporters
open Logary.Prometheus

[<Literal>]
let iterationCount = 67108864 // 1 <<< 26

type SingleThreadCounter() =
  let mutable doubleAdderCounter = new DoubleAdder()

  let mutable interlockedCounter = 0L

  [<Benchmark(Baseline = true)>]
  member x.interLockedAdd () =
    let mutable loop = true
    while(loop) do
      let originLocal = interlockedCounter
      let added = BitConverter.Int64BitsToDouble originLocal + 2. |> BitConverter.DoubleToInt64Bits
      loop <- originLocal <> Interlocked.CompareExchange(&interlockedCounter,added, originLocal)

  [<Benchmark>]
  member x.doubleAdder () = doubleAdderCounter.Add 2.

type Counter() =
  let mutable interlockedCounter = 0L

  let interLockedAdd value =
    let mutable loop = true
    while(loop) do
      let originLocal = interlockedCounter
      let added = BitConverter.Int64BitsToDouble originLocal + value |> BitConverter.DoubleToInt64Bits
      loop <- originLocal <> Interlocked.CompareExchange(&interlockedCounter,added, originLocal)

  let mutable doubleAdderCounter = new DoubleAdder()

  [<Params(1,2,4,8,16,32,64,128,256)>]
//  [<Params(1,32)>]
  [<DefaultValue>]
  val mutable threadCount:int

  [<DefaultValue>]
  val mutable invokeFunc:double -> unit

  let prepareDone = new ManualResetEventSlim(false)

  [<DefaultValue>]
  val mutable threads: Thread[]

  member x.launchWorker () =
    interlockedCounter <- 0L
    doubleAdderCounter <- new DoubleAdder()
    prepareDone.Reset()

    let invokeCountEachThread = iterationCount / x.threadCount
    x.threads <- Array.zeroCreate x.threadCount
    for threadIdx in 1..x.threadCount do
      let floatValueToBeAdded = float threadIdx
      let thread = new Thread(fun () ->
          prepareDone.Wait()
          for i in 1..invokeCountEachThread do
            x.invokeFunc floatValueToBeAdded
       )
      thread.Start()
      x.threads.[threadIdx-1] <- thread

  [<Benchmark(OperationsPerInvoke = iterationCount)>]
  member x.doubleAdder () =
    x.launchWorker ()
    x.invokeFunc <- doubleAdderCounter.Add
    x.runAndEnsureResult <| fun () -> doubleAdderCounter.Sum ()

  [<Benchmark(Baseline = true, OperationsPerInvoke = iterationCount)>]
  member x.interLocked () =
    x.launchWorker ()
    x.invokeFunc <- interLockedAdd
    x.runAndEnsureResult <| fun () -> BitConverter.Int64BitsToDouble interlockedCounter

  member x.runAndEnsureResult getActuralFunc =
    prepareDone.Set()
    for thread in x.threads do thread.Join ()

    let invokeCountEachThread = float (iterationCount / x.threadCount)
    let expect = Seq.fold (fun acc addValue -> acc + (float addValue) * invokeCountEachThread) 0. [1..x.threadCount]
    let actural = getActuralFunc ()
    if expect <> actural then failwithf "expect %A not equal actural %A" expect actural


module Tests =
  open BenchmarkDotNet.Diagnosers
  open BenchmarkDotNet.Exporters.Csv
  open BenchmarkDotNet.Reports


  [<Tests>]
  let benchmarks =
    let config xJ =
      { benchmarkConfig with
          exporters =
            [
              BenchmarkDotNet.Exporters.MarkdownExporter.GitHub
            ]
          diagnosers =
            [ new MemoryDiagnoser() ]
          jobs = [ xJ ]
      }

    testList "benchmarks" [
      test "Counter" {
        let testJob4Debug = Job.Default.WithLaunchCount(1).WithWarmupCount(2).WithTargetCount(4).WithInvocationCount(2).WithUnrollFactor(2)
        let cfg = config (Job(Job.Default))
        let cfg = config (Job(testJob4Debug))

        benchmark<Counter> cfg box |> ignore

        let cfg = config (Job(Job.Default))
        benchmark<SingleThreadCounter> cfg box |> ignore
      }

      testCase "DoubleAdder should work" <| fun _ ->
        let adder = new DoubleAdder(100000.)
        for i in 1..100000 do
          adder.Add 1.
        let actual = adder.Sum ()
        Expect.equal actual 200000. "should worked"

    ]


[<EntryPoint>]
let main argv =
    use cts = new CancellationTokenSource()
    let defaultConfig = { defaultConfig with ``parallel`` = false }
    runTestsInAssemblyWithCancel cts.Token defaultConfig argv
