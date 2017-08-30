/// The `Config` module handles interaction with the Registry by building the
/// required configuration for Logary.
namespace Logary.Configuration

open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Targets

/// Specifies the internal logging level for Logary.
[<RequireQualifiedAccess>]
type ILogger =
  | Console of minLevel:LogLevel
  | LiterateConsole of minLevel:LogLevel
  | Target of config:TargetConf

module Config =
  type T =
    private {
      targets      : HashMap<string, TargetConf>
      host         : string
      service      : string
      getTimestamp : unit -> EpochNanoSeconds
      getSem       : unit -> obj
      ilogger      : ILogger
      middleware   : Middleware list
      processing   : Processing
      setGlobals   : bool
    }

  let create service host =
    let mids =
      [ Middleware.host host
        Middleware.service service ]
    { targets      = HashMap.empty
      host         = host
      service      = service
      getTimestamp = Global.getTimestamp
      getSem       = Global.getConsoleSemaphore
      middleware   = mids
      ilogger      = ILogger.Console Warn
      setGlobals   = true
      processing   = Pipe.start 
    }

  let target name tconf lconf =
    { lconf with targets = lconf.targets |> HashMap.add name tconf }

  let targets tconfs lconf =
    tconfs |> Seq.fold (fun lconf (name, tconf) -> lconf |> target name tconf) lconf

  let host host lconf =
    { lconf with host = host }

  let service name lconf =
    { lconf with service = name }

  let timestamp getTimestamp lconf =
    { lconf with getTimestamp = getTimestamp }

  let consoleSemaphore getConsoleSemaphore lconf =
    { lconf with getSem = getConsoleSemaphore }

  let middleware mid (lconf : T) =
    { lconf with middleware = mid :: lconf.middleware }

  let ilogger ilogger lconf =
    { lconf with ilogger = ilogger }

  let processing processor lconf =
    { lconf with processing = processor }

  let disableGlobals lconf =
    { lconf with setGlobals = false }

  let build (lconf : T) : Job<Registry.T> =
    let ri : RuntimeInfo.T =
      { service = lconf.service
        host = lconf.host
        getTimestamp = lconf.getTimestamp
        getConsoleSemaphore = lconf.getSem
        logger = NullLogger.instance }

    let rule, itarget =
      match lconf.ilogger with
      | ILogger.Console minLevel ->
        Rule.setMinLevel minLevel Rule.empty,
        Console.create Console.empty "internal"

      | ILogger.LiterateConsole minLevel ->
        Rule.setMinLevel minLevel Rule.empty,
        LiterateConsole.create LiterateConsole.empty "internal"

      | ILogger.Target conf ->
        Rule.compile conf.rules,
        conf

    InternalLogger.create ri rule >>= fun ilogger ->
    InternalLogger.add itarget ilogger >>= fun () ->

    let middleware = Array.ofList lconf.middleware
    let ri = { ri with logger = ilogger }

    let conf =
      { new LogaryConf with
          member x.targets = lconf.targets
          member x.runtimeInfo = upcast ri
          member x.middleware = middleware
          member x.processing = lconf.processing
      }
    Registry.create conf

  let toLogManager (registry : Registry.T) : LogManager =
    Registry.toLogManager registry