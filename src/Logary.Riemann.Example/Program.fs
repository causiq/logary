#if INTERACTIVE
#r "bin/Release/FSharp.Actor.dll"
#r "bin/Release/NodaTime.dll"
#r "bin/Release/Intelliplan.Logary.dll"
#r "bin/Release/Intelliplan.Logary.Riemann.dll"
#endif

open System

open NodaTime

open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Metrics

[<EntryPoint>]
let main argv =
  use logary =
    withLogary' "Riemann.Example" (
      withTargets [
//        Riemann.create (Riemann.RiemannConf.Create(tags = ["riemann-health"])) "riemann"
        Console.create (Console.empty) "console"
      ] >>
      withMetrics (Duration.FromSeconds 4L) [
        WinPerfCounters.create (WinPerfCounters.Common.cpuTime) "cpuTime" (Duration.FromMilliseconds 500L)
      ] >>
      withRules [
//        Rule.createForTarget "riemann"
        Rule.createForTarget "console"
      ] >>
      withInternalTargets Info [
        Console.create (Console.empty) "console"
      ]
    )

  Console.ReadKey true |> ignore
  0
