/// The `Config` module handles interaction with the Registry by building the
/// required configuration for Logary.
namespace Logary.Configuration

open System.Threading
open FSharp.Control.Tasks.V2.ContextInsensitive
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Metric
open Logary.Internals
open Logary.Targets
open Logary.Configuration
open NodaTime

/// Specifies the internal logger targets for Logary.
[<RequireQualifiedAccess>]
type ILogger =
  | Console of minLevel:LogLevel
  | Targets of config:TargetConf list

module Config =

  type T =
    private {
      targets: Map<string, TargetConf>
      host: string
      service: string
      getTimestamp: unit -> EpochNanoSeconds
      consoleLock: DVar<Lock>
      ilogger: ILogger
      middleware: Middleware list
      processing: Processing
      setGlobals: bool
      loggerLevels: (string * LogLevel) list
      logResultHandler: ProcessResult -> unit
      waitForTargetsTimeout: Duration
      metricRegistry: MetricRegistry
    }

  let create service host =
    let simple = SimpleMessageWriter() :> MessageWriter
    let errorHandler (xR: ProcessResult) =
      match xR with
      | Result.Error em ->
        task {
          use cts = new CancellationTokenSource()
          try do! simple.write(System.Console.Error, em, cts.Token)
          with _ -> ()
        }

        |> ignore
      | _ -> ()

    { targets          = Map.empty
      host             = host
      service          = service
      getTimestamp     = Global.getTimestamp
      consoleLock      = Global.semaphoreD
      middleware       = List.empty
      ilogger          = ILogger.Console LogLevel.Warn
      setGlobals       = true
      processing       = Events.events
      loggerLevels     = [(".*", LogLevel.Info)]
      logResultHandler = errorHandler
      waitForTargetsTimeout = Duration.FromSeconds 3L
      metricRegistry = MetricRegistry()
    }

  let target (tconf: TargetConf) lconf =
    { lconf with targets = lconf.targets |> Map.add tconf.name tconf }

  let targets tconfs lconf =
    tconfs |> Seq.fold (fun lconf tconf -> lconf |> target tconf) lconf

  let host host lconf =
    { lconf with host = host }

  let service name lconf =
    { lconf with service = name }

  let timestamp getTimestamp lconf =
    { lconf with getTimestamp = getTimestamp }

  let consoleSemaphore getConsoleSemaphore lconf =
    { lconf with consoleLock = getConsoleSemaphore }

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

  let logResultHandler handler lconf =
    { lconf with logResultHandler = handler }

  let disableGlobals lconf =
    { lconf with setGlobals = false }

  let metricRegistry metricRegistry lconf =
    { lconf with metricRegistry = metricRegistry }

  let inline private setToGlobals (logManager: LogManager) =
    let config = { Global.defaultConfig with getLogger = logManager.getLogger }
    Global.initialise config

  let internal createInternalTargets = function
    | ILogger.Console minLevel ->
      let target = Console.create Console.empty "internal"
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
        consoleLock = lconf.consoleLock
        logger = NullLogger.instance }

    createInternalLogger ri (createInternalTargets lconf.ilogger) >>= fun (ri, _) ->
    let mids =
      [ Middleware.host lconf.host
        Middleware.service lconf.service
        Middleware.metricsToRegistry lconf.metricRegistry
      ]
    let middleware = Array.ofList (lconf.middleware @ mids)

    let conf =
      { new LogaryConf with
          member x.targets = lconf.targets
          member x.runtimeInfo = upcast ri
          member x.middleware = middleware
          member x.processing = lconf.processing
          member x.loggerLevels = lconf.loggerLevels
          member x.logResultHandler = lconf.logResultHandler
          member x.waitForTargetsTimeout = lconf.waitForTargetsTimeout
          member x.metricRegistry = lconf.metricRegistry
      }

    Registry.create conf >>- fun registry ->
    let logManager = Registry.toLogManager registry
    if lconf.setGlobals then do setToGlobals logManager
    logManager

  let buildAndRun lconf: LogManager =
    build lconf |> run
