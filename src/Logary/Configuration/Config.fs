/// The `Config` module handles interaction with the Registry by building the
/// required configuration for Logary.
namespace Logary.Configuration

open System.Threading
open FSharp.Control.Tasks.Builders
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Internals.Resources
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
      resource: Resource
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

  let create (resource: Resource) =
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
      resource         = resource
      getTimestamp     = Global.getTimestamp
      consoleLock      = Global.lockD
      middleware       = List.empty
      ilogger          = ILogger.Console LogLevel.Warn
      setGlobals       = true
      processing       = Events.events
      loggerLevels     = [(".*", LogLevel.Info)]
      logResultHandler = errorHandler
      waitForTargetsTimeout = Duration.FromSeconds 3L
      metricRegistry   = Global.defaultConfig.metrics
    }

  let target (tconf: TargetConf) (conf: T) =
    { conf with targets = conf.targets |> Map.add tconf.name tconf }

  let targets tconfs (conf: T) =
    tconfs |> Seq.fold (fun conf tconf -> conf |> target tconf) conf

  let host host (conf: T) =
    { conf with resource = conf.resource |> Resource.setDetail (function Hostname _ -> true | _ -> false) host }

  let service name (conf: T) =
    { conf with resource = { conf.resource with service = name } }

  let resource resource (conf: T) =
    { conf with resource=resource }

  let timestamp getTimestamp (conf: T) =
    { conf with getTimestamp = getTimestamp }

  let consoleSemaphore getConsoleSemaphore (conf: T) =
    { conf with consoleLock = getConsoleSemaphore }

  let middleware mid (conf: T) =
    { conf with middleware = mid :: conf.middleware }

  let ilogger ilogger (conf: T) =
    { conf with ilogger = ilogger }

  let processing processor (conf: T) =
    { conf with processing = processor }

  let loggerLevels levels (conf: T) =
     { conf with loggerLevels = levels }

  /// config the min loglevel of logger which belong to this path,
  /// path can be regex or specific logger name.
  /// specific path should config last, be careful with the config order.
  /// logger which is not set minlevel is Info by default.
  let loggerMinLevel path minLevel (conf: T) =
    { conf with loggerLevels = (path, minLevel) :: conf.loggerLevels }

  let logResultHandler handler (conf: T) =
    { conf with logResultHandler = handler }

  let disableGlobals (conf: T) =
    { conf with setGlobals = false }

  let metricRegistry metricRegistry (conf: T) =
    { conf with metricRegistry = metricRegistry }

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

  let internal createInternalLogger (ri: RuntimeInfoValue) (internalTargets: TargetConf list) =
    job {
      let! ilogger = InternalLogger.create ri
      do! internalTargets |> Seq.Con.iterJob (fun t -> InternalLogger.add t ilogger)
      return { ri with logger = ilogger }, ilogger
    }

  let build (conf: T): Job<LogManager> =
    let ri: RuntimeInfoValue =
      { resource = conf.resource
        getTimestamp = conf.getTimestamp
        consoleLock = conf.consoleLock
        logger = NullLogger.instance }

    createInternalLogger ri (createInternalTargets conf.ilogger) >>= fun (ri, _) ->

    let middleware =
      [|  yield! conf.middleware
          yield Middleware.setResource conf.resource
          yield Middleware.metricsToRegistry conf.metricRegistry |]

    let config =
      { new LogaryConf with
          member x.targets = conf.targets
          member x.runtimeInfo = upcast ri
          member x.middleware = middleware
          member x.processing = conf.processing
          member x.loggerLevels = conf.loggerLevels
          member x.logResultHandler = conf.logResultHandler
          member x.waitForTargetsTimeout = conf.waitForTargetsTimeout
          member x.metricRegistry = conf.metricRegistry
      }

    Registry.create config >>- fun registry ->
    let logManager = Registry.toLogManager registry
    if conf.setGlobals then do setToGlobals logManager
    logManager

  let buildAndRun (conf: T): LogManager =
    build conf |> run
