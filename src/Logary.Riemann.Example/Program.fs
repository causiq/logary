#if INTERACTIVE
#r "bin/Release/FSharp.Actor.dll"
#r "bin/Release/NodaTime.dll"
#r "bin/Release/Intelliplan.Logary.dll"
#r "bin/Release/Intelliplan.Logary.Riemann.dll"
#endif

open System
open System.Threading

open NodaTime

open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Metrics

[<EntryPoint>]
let main argv =
  use mres = new ManualResetEventSlim()
  use logary =
    withLogary' "Riemann.Example" (
      withTargets [
        Console.create (Console.empty) "console"
        Dash.create Dash.empty "dash"
      ] >>
      withMetrics (Duration.FromSeconds 4L) [
        WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "cpuTime" (Duration.FromMilliseconds 500L)
      ] >>
      withRules [
        Rule.createForTarget "console"
        Rule.createForTarget "dash"
      ]
    )

  mres.Wait()
  0
