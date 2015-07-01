#!/usr/bin/env fsharpi
#I "bin/"
#r "FSharp.Core.dll"
#r "FSharp.Actor.dll"
#r "Logary.dll"
#r "NodaTime.dll"
#r "protobuf-net.dll"
#r "Logary.Targets.Heka.dll"

open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Targets.Heka
open Logary.Metrics
open NodaTime

printfn "starting logary"

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

printfn "blocking forever! yey!"

open System.Threading
let mre = new ManualResetEventSlim(false)
mre.Wait() // wait forever

logary.Dispose()
