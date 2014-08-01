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
        Console.create (Console.ConsoleConf.Default) "console"
      ] >>
      withMetrics [
        WinPerfCounters.create (WinPerfCounters.Common.cpuTime) "cpuTime" (Duration.FromMilliseconds 500L)
      ] >>
      withRules [
//        Rule.forAny "riemann"
        Rule.forAny "console"
      ] >>
      withInternalTargets Info [
        Console.create (Console.ConsoleConf.Default) "console"
      ]
    )

  Console.ReadKey true |> ignore
  0
