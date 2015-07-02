#if INTERACTIVE
#r "bin/Release/FSharp.Actor.dll"
#r "bin/Release/NodaTime.dll"
#r "bin/Release/Logary.dll"
#r "bin/Release/Logary.Riemann.dll"
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
    withLogary' "Logary.ConsoleApp" (
      withTargets [
        Riemann.create (Riemann.RiemannConf.Create(tags = ["riemann-health"])) "riemann"
        Console.create (Console.empty) "console"
        Logstash.create (Logstash.LogstashConf.Create("logstash.prod.corp.tld", 1939us)) "logstash"
      ] >>
      withMetrics (Duration.FromSeconds 4L) [
        WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "cpuTime" (Duration.FromMilliseconds 500L)
      ] >>
      withRules [
        Rule.createForTarget "riemann"
        Rule.createForTarget "console"
        Rule.createForTarget "logstash"
      ] >>
      withInternalTargets Info [
        Console.create (Console.empty) "console"
      ]
    )

  Console.ReadKey true |> ignore
  0
