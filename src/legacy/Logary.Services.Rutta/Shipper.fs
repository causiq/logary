namespace Logary.Services.Rutta

module Shipper =
  open System
  open System.Threading
  open MBrace.FsPickler
  open NodaTime
  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Metrics
  open Logary.Target
  open Logary.Targets
  open Logary.Internals
  open Logary.Configuration
  open fszmq
  open fszmq.Socket
  open Logary.EventsProcessing

  let private runLogary shipperConf =
    use mre = new ManualResetEventSlim(false)
    use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())
    let hostName = System.Net.Dns.GetHostName()

    let systemMetrics =
      let sys = WinPerfCounters.systemMetrics (PointName.parse "sys")
      Events.events
      |> Pipe.tickTimer sys (TimeSpan.FromMilliseconds 400.)

    let processing =
      Events.compose [
        systemMetrics
        |> Pipe.map Array.toSeq
        |> Events.flattenToProcessing
        |> Events.sink ["rutta-shipper"]
      ]

    let logary =
      Config.create "Rutta" hostName
      |> Config.targets [
          //Noop.create (Noop.empty) (PointName.ofSingle "noop")
          //Console.create (Console.empty) (PointName.ofSingle "console")
          Shipper.create shipperConf "rutta-shipper"
        ]
      |> Config.loggerLevels [ ".*", Verbose ]
      |> Config.processing processing
      |> Config.ilogger (ILogger.Console Debug)
      |> Config.build
      |> run

    mre.Wait()
    Choice1Of2 ()

  let internal pushTo connectTo pars: Choice<unit, string> =
    printfn "%s" "spawning shipper in PUSH mode"
    runLogary (Shipper.PushTo connectTo)

  let internal pubTo connectTo pars: Choice<unit, string> =
    printfn "%s" "spawning shipper in PUB mode"
    runLogary (Shipper.PublishTo connectTo)

