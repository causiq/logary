namespace Logary.Services.Rutta

module Shipper =
  open System
  open System.Threading
  open MBrace.FsPickler
  open NodaTime
  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Target
  open Logary.Targets
  open Logary.Internals
  open Logary.Configuration
  open Logary.Targets.Shipper
  open fszmq
  open fszmq.Socket

  let private runLogary shipperConf: IDisposable =
    let hostName = System.Net.Dns.GetHostName()

    //let systemMetrics =
      //let sys = WinPerfCounters.systemMetrics (PointName.parse "sys")
      //Events.events
      //|> Pipe.tickTimer sys (TimeSpan.FromMilliseconds 400.)

    //let processing =
    //  Events.compose [
    //    systemMetrics
    //    |> Events.flattenSeq
    //    |> Events.sink ["rutta-shipper"]
    //  ]

    let logary =
      Config.create "Rutta" hostName
      |> Config.targets [
          //Noop.create (Noop.empty) (PointName.ofSingle "noop")
          //Console.create (Console.empty) (PointName.ofSingle "console")
          Shipper.create shipperConf "rutta-shipper"
        ]
      |> Config.loggerLevels [ ".*", Verbose ]
      //|> Config.processing processing
      |> Config.ilogger (ILogger.Console Debug)
      |> Config.build
      |> run

    { new IDisposable with member x.Dispose () = run (logary.shutdown()) }

  let internal pushTo endpoint =
    printfn "%s" "spawning shipper in PUSH mode"
    runLogary (ShipperConf.createPush endpoint)

  let internal pubTo endpoint =
    printfn "%s" "spawning shipper in PUB mode"
    runLogary (ShipperConf.createPub endpoint)