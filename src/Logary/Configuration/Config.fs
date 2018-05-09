/// The `Config` module handles interaction with the Registry by building the
/// required configuration for Logary.
namespace Logary.Configuration

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Internals
open Logary.Targets
open Logary.Configuration
open Logary.MessageTemplates.Destructure
open Logary.Formatting

/// Specifies the internal logger targets for Logary.
[<RequireQualifiedAccess>]
type ILogger =
  | Console of minLevel:LogLevel
  | LiterateConsole of minLevel:LogLevel
  | Targets of config:TargetConf list

module Config =
  type T =
    private {
      targets: HashMap<string, TargetConf>
      host: string
      service: string
      getTimestamp: unit -> EpochNanoSeconds
      getSem: unit -> obj
      ilogger: ILogger
      middleware: Middleware list
      processing: Events.Processing
      setGlobals: bool
      loggerLevels : (string * LogLevel) list
    }

  let create service host =
    { targets      = HashMap.empty
      host         = host
      service      = service
      getTimestamp = Global.getTimestamp
      getSem       = Global.getConsoleSemaphore
      middleware   = List.empty
      ilogger      = ILogger.Console Warn
      setGlobals   = true
      processing   = Events.events
      loggerLevels = [(".*", LogLevel.Info)]
    }

  let target tconf lconf =
    { lconf with targets = lconf.targets |> HashMap.add tconf.name tconf }

  let targets tconfs lconf =
    tconfs |> Seq.fold (fun lconf tconf -> lconf |> target tconf) lconf

  let host host lconf =
    { lconf with host = host }

  let service name lconf =
    { lconf with service = name }

  let timestamp getTimestamp lconf =
    { lconf with getTimestamp = getTimestamp }

  let consoleSemaphore getConsoleSemaphore lconf =
    { lconf with getSem = getConsoleSemaphore }

  let middleware mid (lconf: T) =
    { lconf with middleware = mid :: lconf.middleware }

  let ilogger ilogger lconf =
    { lconf with ilogger = ilogger }

  let processing processor lconf =
    { lconf with processing = processor }

  let loggerLevels levels lconf =
     { lconf with loggerLevels = levels }

  /// config the min loglevel of logger which belong to this path,
  /// path can be regex or specific logger name.
  /// specific path should config last, be careful with the config order.
  /// logger which is not set minlevel is Info by default.
  let loggerMinLevel path minLevel lconf =
    { lconf with loggerLevels = (path, minLevel) :: lconf.loggerLevels }

  let disableGlobals lconf =
    { lconf with setGlobals = false }

  let inline private setToGlobals (logManager: LogManager) =
    let config =
      { Global.defaultConfig with
          getLogger = logManager.getLogger
          getLoggerWithMiddleware = logManager.getLoggerWithMiddleware }
    Global.initialise config

  let internal createInternalTargets = function
    | ILogger.Console minLevel ->
      let target = Console.create Console.empty "internal"
      let rule = Rule.empty |> Rule.setMinLevel minLevel
      [ TargetConf.setRule rule target ]

    | ILogger.LiterateConsole minLevel ->
      let target = LiterateConsole.create LiterateConsole.empty "internal"
      let rule = Rule.empty |> Rule.setMinLevel minLevel
      [ TargetConf.setRule rule target ]

    | ILogger.Targets conf ->
      conf

  let internal createInternalLogger (ri: RuntimeInfo.T) (itargets: TargetConf list) =
    job {
      let! ilogger = InternalLogger.create ri
      do! itargets |> Seq.Con.iterJob (fun itarget -> InternalLogger.add itarget ilogger)
      return { ri with logger = ilogger }, ilogger
    }

  let build (lconf: T): Job<LogManager> =
    let ri: RuntimeInfo.T =
      { service = lconf.service
        host = lconf.host
        getTimestamp = lconf.getTimestamp
        getConsoleSemaphore = lconf.getSem
        logger = NullLogger.instance }

    createInternalLogger ri (createInternalTargets lconf.ilogger) >>= fun (ri, ilogger) ->
    let mids =
      [ Middleware.host lconf.host
        Middleware.service lconf.service ]
    let middleware = Array.ofList (lconf.middleware @ mids)

    let conf =
      { new LogaryConf with
          member x.targets = lconf.targets
          member x.runtimeInfo = upcast ri
          member x.middleware = middleware
          member x.processing = lconf.processing
          member x.loggerLevels = lconf.loggerLevels
      }
    Registry.create conf >>- fun registry ->
    let logManager = Registry.toLogManager registry
    if lconf.setGlobals then do setToGlobals logManager
    logManager


  // TO CONSIDER: config below around registry,not globals

  let projection projectionExpr =
    Logary.Internals.Global.Destructure.configProjection projectionExpr

  let destructurer<'t> (factory: CustomDestructureFactory<'t>) =
    Logary.Internals.Global.Destructure.configDestructure<'t> factory

  let jsonEncoder<'t> (factory: JsonEncoderFactory<'t>) =
    Logary.Internals.Global.Json.configureEncoder<'t> factory