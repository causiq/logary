#if INTERACTIVE
#I "bin/Release"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "NodaTime.dll"
#r "Logary.dll"
#r "Logary.Riemann.dll"
#endif

open System
open NodaTime
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Metric
open Logary.Metrics
open Logary.Metrics.WinPerfCounter

module internal Sample =

  let loginsPerSecond : Job<Stream<Message>> = job {
    let! counter = Counters.counter (PointName.ofSingle "logins")
    let! ewma = Reservoirs.ewma (PointName.ofSingle "logins")
    do! ewma |> Metric.consume (Metric.tap counter)
    return Metric.tapMessages ewma
  }

  let cpuTime pn : Job<Metric> =
    let reducer state = function
      | _ ->
        state

    let toValue (counter : PerfCounter) =
      let pc = toPC counter |> Option.get // from state initialisation post-condition
      Float (WinPerfCounter.nextValue pc)
      |> Message.metricWithUnit pn Units.Scalar
      |> Message.setName (PointName.ofPerfCounter counter)

    let ticker state =
      state, state |> List.map toValue

    let counters =
      WinPerfCounters.Common.cpuTime
      |> List.filter (WinPerfCounter.toPC >> Option.isSome)

    Metric.create reducer counters ticker

open System.Threading

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  use logary =
    withLogaryManager "Logary.Examples.ConsoleApp" (
      withTargets [
        Console.create (Console.empty) (PointName.ofSingle "console")
      ] >>
      withMetrics [
        //WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "cpuTime" (Duration.FromMilliseconds 500L)
        MetricConf.create (Duration.FromMilliseconds 500L) (PointName.ofSingle "cpu") Sample.cpuTime
      ] >>
      withRules [
        Rule.createForTarget (PointName.ofSingle "console")
      ] >>
      withInternalTargets Info [
        Console.create Console.empty (PointName.ofSingle "console")
      ]
    )
    |> run

  mre.Wait()
  0
