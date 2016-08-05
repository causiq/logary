#if INTERACTIVE
#I "bin/Release"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "NodaTime.dll"
#r "Logary.dll"
#r "Logary.Riemann.dll"
#endif

open System
open NodaTime
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Metric
open Logary.Metrics
open Logary.Metrics.WinPerfCounter
open Logary.Metrics.WinPerfCounter

module internal Sample =

  let loginsPerSecond : Job<Stream<Message>> = job {
    let! counter = Counters.counter (PointName.ofSingle "logins")
    let! ewma = Reservoirs.ewma (PointName.ofSingle "logins")
    do! ewma |> Metric.consume (Metric.tap counter)
    return Metric.tapMessages ewma
  }

  let metricFrom counters pn : Job<Metric> =
    let reducer state = function
      | _ ->
        state

    let toValue (counter : PerfCounter, pc : PC) =
      let value = WinPerfCounter.nextValue pc
      Float value
      |> Message.derivedWithUnit pn Units.Scalar
      |> Message.setName (PointName.ofPerfCounter counter)

    let ticker state =
      state, state |> List.map toValue

    Metric.create reducer counters ticker

  let cpuTime pn : Job<Metric> =
    metricFrom WinPerfCounters.Common.cpuTime pn

  let m6000s pn : Job<Metric> =
    let gpu counter instance =
      { category = "GPU"
        counter  = counter
        instance = Instance instance }

    let counters =
      [ for inst in [ "08:00"; "84:00" ] do
          let inst' = sprintf "quadro m6000(%s)" inst
          yield gpu "GPU Fan Speed (%)" inst'
          yield gpu "GPU Time (%)" inst'
          yield gpu "GPU Memory Usage (%)" inst'
          yield gpu "GPU Memory Used (MB)" inst'
          yield gpu "GPU Power Usage (Watts)" inst'
          yield gpu "GPU SM Clock (MHz)" inst'
          yield gpu "GPU Temperature (degrees C)" inst'
      ]
      |> WinPerfCounters.Common.ofPerfCounters

    metricFrom counters pn


open System.Threading

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  use logary =
    withLogaryManager "Logary.Examples.ConsoleApp" (
      withTargets [
        Console.create (Console.empty) (PointName.ofSingle "console")
      ] >>
      withMetrics [
        //WinPerfCounters.create (WinPerfCounters.Common.cpuTimeConf) "cpuTime" (Duration.FromMilliseconds 500L)
        MetricConf.create (Duration.FromMilliseconds 500L) (PointName.ofSingle "cpu") Sample.cpuTime
        MetricConf.create (Duration.FromMilliseconds 500L) (PointName.ofSingle "gpu") Sample.m6000s
      ] >>
      withRules [
        Rule.createForTarget (PointName.ofSingle "console")
      ] >>
      withInternalTargets Info [
        Console.create Console.empty (PointName.ofSingle "console")
      ]
      >> run
    )
    |> run

  let logger =
    logary.getLogger (PointName [| "Logary"; "Samples"; "main" |])

  Message.templateFormat "{userName} logged in" [| "haf" |]
  |> Logger.logSimple logger

  Message.eventFormat (Info, "{userName} logged in", [| "adam" |])
  |> Logger.logSimple logger

  mre.Wait()
  0
