#if INTERACTIVE
#r "bin/Release/FSharp.Actor.dll"
#r "bin/Release/NodaTime.dll"
#r "bin/Release/Intelliplan.Logary.dll"
#r "bin/Release/Intelliplan.Logary.Riemann.dll"
#endif

open System
open System.Text.RegularExpressions

open FSharp.Actor
open NodaTime

open Logary
open Logary.Configuration

open Logary.Target
open Logary.Targets

open Logary.Metrics

open Logary.Logging

[<EntryPoint>]
let main argv =
  use logary =
    withLogary' "Riemann.Example" (
      withTargets [
        Riemann.create (Riemann.RiemannConf.Create(tags = ["riemann-health"])) "riemann"
        Console.create (Console.ConsoleConf.Default) "console"
      ] >>
      withMetrics [
        WinPerfCounters.create (WinPerfCounters.Common.cpuTime) "cpuTime" (Duration.FromMilliseconds 500L)
      ] >>
      withRules [
        Rule.forAny "riemann"
        Rule.forAny "console"
        Rule.forAny "cpuTime"
      ]
    )

//  clr_proc |> Registry.RegisterHealthCheck logary
//  cpus |> Registry.registerHealthChecks logary

// TODO:
//  let logger = logary.GetLogger "Riemann.Example"
//  ("disk /", 0.456) ||> Metrics.gauge logger

  0 // return an integer exit code
