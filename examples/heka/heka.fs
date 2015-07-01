module Program
open System.Threading
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Targets.Heka
open Logary.Metrics
open NodaTime

[<EntryPoint>]
let main args =
  printfn "starting logary"
  use logary =
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

  use mre = new ManualResetEventSlim(false)
  mre.Wait() // wait forever
  0
