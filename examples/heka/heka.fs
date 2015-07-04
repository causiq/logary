module Program

open System
open System.Threading
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Targets.Heka
open Logary.Metrics
open System.Net
open NodaTime

[<EntryPoint>]
let main args =
  printfn "starting logary with heka target..."

  use logary =
    withLogary' "Heka.Example" (
      withTargets [
        Heka.create ({ HekaConfig.Empty with endpoint = IPEndPoint(IPAddress.Parse("127.0.0.1"), 5565 ), false }) "heka"
        Console.create (Console.empty) "console"
      ] >>
      withMetrics (Duration.FromSeconds 4L) [
        WinPerfCounters.create (WinPerfCounters.Common.cpuTime) "cpuTime" (Duration.FromMilliseconds 500L)
      ] >>
      withRules [
        Rule.createForTarget "heka"
        Rule.createForTarget "console"
      ] >>
      withInternalTargets Debug [
        Console.create (Console.empty) "console"
      ]
    )

  printfn "started"

  use mre = new ManualResetEventSlim(false)
  Console.CancelKeyPress.Add(fun _ -> mre.Set() |> ignore)
  mre.Wait()

  0
