#I "bin/Debug"
#r "FSharp.Actor.dll"
#r "NodaTime.dll"
#r "Newtonsoft.Json.dll"
#r "Logary.dll"
#r "protobuf-net.dll"
#load "../Logary.Targets.Riemann/ProtoBufUtils.fs"
#load "Messages.fs"
#load "Constants.fs"
#load "Types.fs"
#load "Client.fs"
#load "Targets_Heka.fs"

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
    withInternalTargets Info [
      Console.create (Console.empty) "console"
    ]
  )

logary.Dispose()
