/// The `Config` module handles interaction with the Registry by building the
/// required configuration for Logary.
namespace Logary.Configuration

open Logary
open Logary.Internals

/// Specifies the internal logging level for Logary.
[<RequireQualifiedAccess>]
type ILogger =
  | Console
  | LiterateConsole
  | Target of config:TargetConf

module Config =
  type T =
    private {
      targets      : Map<string, TargetConf>
      metrics      : Map<string, MetricConf>
      healthChecks : Map<string, HealthCheckConf>
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
    { targets      = Map.empty
      metrics      = Map.empty
      healthChecks = Map.empty
      host         = host
      service      = service
      getTimestamp = Globals.defaultConfig.getTimestamp
      getSem       = Globals.defaultConfig.getConsoleSemaphore
      middleware   = mids
      ilogger      = ILogger.Console
      setGlobals   = true
      processing   = Processing (fun _ _ -> Alt.always ()) }

  let target name tconf lconf =
    { lconf with targets = lconf.targets |> Map.add name tconf }

  let targets tconfs lconf =
    tconfs |> Seq.fold (fun lconf (name, tconf) -> lconf |> target name tconf) lconf

  let metric name mconf lconf =
    { lconf with metrics = lconf.metrics |> Map.add name mconf }

  let metrics mconfs lconf =
    mconfs |> Seq.fold (fun lconf (name, mconf) -> lconf |> metric name mconf) lconf

  let healthCheck name hcc lconf =
    { lconf with healthChecks = lconf.healthChecks |> Map.add name hcc }

  let healthChecks hccs lconf =
    hccs |> Seq.fold (fun lconf (name, hcc) -> lconf |> healthCheck name hcc) lconf

  let host host lconf =
    { lconf with host = host }

  let service name lconf =
    { lconf with service = name }

  let timestamp getTimestamp lconf =
    { lconf with getTimestamp = getTimestamp }

  let consoleSemaphore getConsoleSemaphore lconf =
    { lconf with getSem = getConsoleSemaphore }

  let middleware mid lconf =
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

    let itarget =
      match lconf.ilogger with
      | ILogger.Console -> Console.create Console.empty "internal"
      | ILogger.LiterateConsole -> LiterateConsole.create LiterateConsole.empty "internal"
      | ILogger.Target conf -> conf

    InternalLogger.create ri Rule.empty >>= fun ilogger ->
    InternalLogger.add itarget ilogger >>= fun () ->

    let middleware = Array.ofList lconf.middleware
    let ri = { ri with logger = ilogger }

    let conf =
      { new LogaryConf with
          member x.targets = lconf.targets
          member x.metrics = lconf.metrics
          member x.healthChecks = lconf.healthChecks
          member x.runtimeInfo = upcast ri
          member x.middleware = middleware
          member x.processing = lconf.processing
      }
    Registry.create conf

  let toLogManager (registry : Registry.T) : LogManager =
    Registry.toLogManager registry