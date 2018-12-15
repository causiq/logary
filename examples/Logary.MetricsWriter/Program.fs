module Program

open System
open System.Threading
open Hopac
open Logary
open Logary.Configuration
open NodaTime
open Logary.Targets
open Logary.Metrics.WinPerfCounters
open Logary.Configuration

module Sample =

  let randomWalk measurement =
    let reducer state = function
      | _ ->
        state

    let ticker (rnd: Random, prevValue) =
      let value =
        let v = (rnd.NextDouble() - 0.5) * 0.3
        if abs v < 0.03 then rnd.NextDouble() - 0.5
        elif v + prevValue < -1. || v + prevValue > 1. then -v + prevValue
        else v + prevValue

      let msg = Message.gauge PointName.empty measurement (Float value)

      (rnd, value), msg

    let state =
      let rnd = Random()
      rnd, rnd.NextDouble()

    Ticker.create state reducer ticker

[<EntryPoint>]
let main argv =
  let inline ms v = Duration.FromMilliseconds (int64 v)
  let pn name = PointName [| "Logary"; "Samples"; name |]
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())
  let clock = SystemClock.Instance
  let tenSecondsEWMATicker = EWMATicker (Duration.FromSeconds 1L, Duration.FromSeconds 10L, clock)
  let randomWalk = Sample.randomWalk "randomWalk"
  let walkPipe =  Events.events |> Pipe.tickTimer randomWalk (Duration.FromMilliseconds 500.)
  let systemMetrics = Events.events |> Pipe.tickTimer (systemMetrics (PointName.parse "sys")) (Duration.FromSeconds 10.)
  let processing =
    Events.compose [
       walkPipe
       |> Events.sink ["WalkFile";]

       walkPipe
       |> Pipe.choose (Message.tryGetGauge "randomWalk")
       |> Pipe.counter (fun _ -> 1L) (Duration.FromSeconds 2.)
       |> Pipe.map (fun counted -> Message.eventFormat (Info, "There are {totalNumbers} randomWalk within 2s", [|counted|]))
       |> Events.sink ["Console";]

       walkPipe
       |> Pipe.choose (Message.tryGetGauge "randomWalk")
       |> Pipe.map (fun _ -> 1L) // think of randomWalk as an event, mapping to 1
       |> Pipe.tickTimer tenSecondsEWMATicker (Duration.FromSeconds 5.)
       |> Pipe.map (fun rate -> Message.eventFormat (Info, "tenSecondsEWMA of randomWalk's rate is {rateInSec}", [|rate|]))
       |> Events.sink ["Console";]

       systemMetrics
       |> Pipe.map Array.toSeq
       |> Events.flattenSeq
       |> Events.sink ["LiterateConsole"; "WPCMetricFile";]

    ]

  let console = Console.create Console.empty "Console"
  let literalConsole = LiterateConsole.create LiterateConsole.empty "LiterateConsole"
  let randomWalkFileName = File.Naming ("{service}-RandomWalk-{date}", "log")
  let wpcFileName = File.Naming ("{service}-wpc-{date}", "log")
  let randomWalkTarget = File.create (File.FileConf.create Environment.CurrentDirectory randomWalkFileName) "WalkFile"
  let wpcFileTarget = File.create (File.FileConf.create Environment.CurrentDirectory wpcFileName) "WPCMetricFile"
  let logary =
    Config.create "Logary.Examples.MetricsWriter" "localhost"
    |> Config.targets [console; literalConsole; randomWalkTarget; wpcFileTarget;]
    |> Config.ilogger (ILogger.Console Verbose)
    |> Config.processing processing
    |> Config.build
    |> run

  mre.Wait()
  0