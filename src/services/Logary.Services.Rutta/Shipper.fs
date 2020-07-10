module Logary.Services.Rutta.Shipper

open System
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets.Shipper

let private runLogary (internalLogary: LogManager) shipperConf: IDisposable =
  let ilogger = internalLogary.getLogger "Logary.Rutta.Shipper"

  //let systemMetrics =
    //let sys = WinPerfCounters.systemMetrics (PointName.parse "sys")
    //Events.events
    //|> Pipe.tickTimer sys (TimeSpan.FromMilliseconds 400.)

  //let processing =
  //  Events.compose [
  //    systemMetrics
  //    |> Events.flattenSeq
  //    |> Events.setTargets ["rutta-shipper"]
  //  ]

  let logary =
    internalLogary.runtimeInfo.resource
    |> Config.create
    |> Config.targets [
        //Noop.create (Noop.empty) "noop"
        //Console.create (Console.empty) "console"
        create shipperConf "rutta-shipper"
      ]
    //|> Config.processing processing
    |> Config.loggerLevels [ ".*", Verbose ]
    |> Config.disableGlobals
    |> Config.ilogger (ILogger.External ilogger)
    |> Config.buildAndRun

  { new IDisposable with member x.Dispose () = run (logary.shutdown()) }

let internal pushTo internalLogary endpoint =
  printfn "%s" "spawning shipper in PUSH mode"
  runLogary internalLogary (ShipperConf.createPush endpoint)

let internal pubTo internalLogary endpoint =
  printfn "%s" "spawning shipper in PUB mode"
  runLogary internalLogary (ShipperConf.createPub endpoint)