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

module internal Sample =

  let loginsPerSecond : Job<Stream<Message>> = job {
    let! counter = Core.counter (PointName.ofSingle "logins")
    let! ewma = Reservoirs.ewma (PointName.ofSingle "logins")
    do! ewma |> Metric.consume (Metric.tap counter)
    return Metric.tapMessages ewma
  }

[<EntryPoint>]
let main argv =
  use logary =
    withLogaryManager "Logary.ConsoleApp" (
      withTargets [
        Console.create (Console.empty) (PointName.ofSingle "console")
      ] >>
      withMetrics (Duration.FromSeconds 4L) [
        //WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "cpuTime" (Duration.FromMilliseconds 500L)

      ] >>
      withRules [
        Rule.createForTarget (PointName.ofSingle "console")
      ] >>
      withInternalTargets Info [
        Console.create Console.empty (PointName.ofSingle "console")
      ]
    )
    |> run

  Console.ReadKey true |> ignore
  0
