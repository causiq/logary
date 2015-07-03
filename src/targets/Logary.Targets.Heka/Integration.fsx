#I "bin/Debug"
#r "FSharp.Actor.dll"
#r "NodaTime.dll"
#r "Newtonsoft.Json.dll"
#r "Logary.dll"
#r "Logary.Targets.Heka.dll"
#r "protobuf-net.dll"

open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Targets.Heka
open Logary.Metrics
open NodaTime

let logary =
  withLogary' "Heka.Example" (
    withTargets [
      Heka.create (HekaConfig.Empty) "heka"
      Console.create (Console.empty) "console"
    ] >>
    withMetrics (Duration.FromSeconds 4L) [
      WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "cpuTime" (Duration.FromMilliseconds 500L)
    ] >>
    withRules [
      Rule.createForTarget "heka"
      Rule.createForTarget "console"
    ] >>
    withInternalTargets Debug [
      Console.create (Console.empty) "console"
    ]
  )





logary.Dispose()
