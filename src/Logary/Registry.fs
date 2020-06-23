/// The registry is the composition root of Logary
namespace Logary

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Metric
open Logary.Internals
open Logary.Configuration
open Logary.Model
open NodaTime
open System.Text.RegularExpressions
open System.Collections.Concurrent

/// This is the logary configuration structure having a memory of all
/// configured targets, middlewares, etc.
type LogaryConf =
  /// A map of the targets by name.
  abstract targets: Map<string, TargetConf>
  /// Service metadata - what name etc.
  abstract runtimeInfo: RuntimeInfo
  /// Extra middleware added to every resolved logger. (composed and invoked at call-site)
  abstract middleware: Middleware[]
  /// Optional stream transformer.
  abstract processing: Processing
  /// The minimum log level accepted, by logger name/logger name regex.
  abstract loggerLevels: (string * LogLevel) list
  /// This handler is invoked with the result of each completed message. It provides a "port" where you can read every
  /// result going through Logary, beyond the caller's thread.
  abstract logResultHandler: (ProcessResult -> unit)
  /// time out duration waiting for each target buffer to be available to take the message, if users choose logging message and waitForBuffers
  abstract waitForTargetsTimeout: Duration
  /// the metric registry for metric
  abstract metricRegistry: MetricRegistry

/// This is the main state container in Logary.
module Registry =

  /// The holder for the channels of communicating with the registry.
  type T =
    private {
      runtimeInfo: RuntimeInfo

      emit: Model.LogaryMessageBase -> LogResult

      /// to show whether or not registry is shutdown, used to control the
      /// message communication channel to avoid the case of endless blocking
      isClosed: IVar<unit>

      /// Flush all pending messages from the registry to await shutdown and
      /// ack on the `ackCh` when done. If the client nacks the request, the
      /// `nack` promise is filled with a unit value. Optional duration of how
      /// long the flush 'waits' for targets before returning a FlushInfo.
      flushCh: Ch<Ch<FlushInfo> * Promise<unit> * Duration option>

      /// Shutdown the registry in full. This operation cannot be cancelled and
      /// so the caller is promised a ShutdownInfo.
      shutdownCh: Ch<IVar<ShutdownInfo> * Duration option>

      /// each logger's minLevel stores here, can be dynamically change
      loggerLevels: ConcurrentDictionary<string,LogLevel>

      /// default logger rules from logary conf, will be applied when create new logger
      defaultLoggerLevelRules: (string * LogLevel) list

      /// metric registry
      metricRegistry: MetricRegistry
    }

  /// Flush all pending messages for all targets. Flushes with no timeout; if
  /// this Alternative yields, all targets were flushed.
  let flush (t: T): Alt<unit> =
        t.isClosed
    <|> (t.flushCh *<+->- fun flushCh nack -> flushCh, nack, None) ^-> ignore

  let private isShutDown = FlushInfo (["The Registry is shut down"], [])

  /// Flush all pending messages for all targets. This Alternative always
  /// yields after waiting for the specified `timeout`; then giving back the
  /// `FlushInfo` data-structure that recorded what targets were successfully
  /// flushed and which ones timed out.
  let flushWithTimeout (t: T) (timeout: Duration): Alt<FlushInfo> =
        t.isClosed ^->. isShutDown
    <|> t.flushCh *<+->- fun flushCh nack -> flushCh, nack, Some timeout


  /// Shutdown the registry and flush all targets before shutting it down. This
  /// function does not specify a timeout, neither for the flush nor for the
  /// shutting down of targets, and so it does not return a `ShutdownInfo`
  /// data-structure.
  let shutdown (t: T): Alt<unit> =
    t.isClosed
    <|>
    ((t.flushCh *<+->- fun flushCh nack -> flushCh, nack, None) ^=> fun _ ->
      (t.shutdownCh *<+=>- fun shutdownCh -> shutdownCh, None) ^-> ignore)


  /// Shutdown the registry and flush all targets before shutting it down. This
  /// function specifies both a timeout for the flushing of targets and the
  /// shutting down of the registry. The Alternative yields after a maximum of
  /// `shutdownTimeout` + `flushTimeout`, with information about the shutdown.
  let shutdownWithTimeouts (t: T) (flushTimeout: Duration) (shutdownTimeout: Duration): Alt<FlushInfo * ShutdownInfo> =
    (t.isClosed ^->. (FlushInfo(["registry is closed"],[]), ShutdownInfo(["registry is closed"],[])))
    <|>
    ((t.flushCh *<+->- fun flushCh nack -> flushCh, nack, Some flushTimeout) ^=> (fun flushInfo ->
      (t.shutdownCh *<+=>- fun shutdownCh -> shutdownCh, Some shutdownTimeout) ^-> (fun shutdownInfo ->
        flushInfo, shutdownInfo)))



  module private Impl =
    let computeLevel (registry: T) =
      fun (loggerName: string) ->
        registry.defaultLoggerLevelRules
          |> List.tryFind (fun (path, _) -> path = loggerName || Regex.IsMatch(loggerName, path))
          |> Option.map snd
          |> Option.defaultValue Info

    let getLogger (registry: T) name =
      let computeLevel = computeLevel registry
      { new Logger with
          member x.name = name
          member x.logWithAck(waitForBuffers, message) =
            if Promise.Now.isFulfilled registry.isClosed then LogResult.registryClosed () else
            if message.level < x.level then LogResult.success else
            let message = message.getAsBase Model.Event
            message.ensureName name
            registry.emit message
          member x.level =
            let loggerName = x.name.ToString()
            registry.loggerLevels.GetOrAdd(loggerName, computeLevel)
      }

    let switchLoggerLevel (t: T) path minLevel =
      let regPath = Regex(path)

      t.loggerLevels
        |> Seq.filter (fun (KeyValue (name, _)) -> name = path || regPath.IsMatch name)
        |> Seq.map (fun (KeyValue (name, _)) -> name)
        |> Seq.toArray
        |> Array.iter (fun name -> t.loggerLevels.[name] <- minLevel)

    let spawnTargets (ri: RuntimeInfo) targets =
      targets
      |> Map.toList
      |> List.traverseJobA (fun (_, conf) -> Target.create ri conf)
      |> Job.map List.toArray

    let generateProcessResult name processAlt (timeout:Duration option) =
      match timeout with
      | None ->
        processAlt ^->. (name, true)
      | Some duration ->
            timeOut (duration.toTimeSpanSafe()) ^->. (name, false)
        <|> processAlt ^->. (name, true)

    let partitionResults results =
      results
      |> List.ofSeq
      |> List.partition snd
      |> (fun (acks, timeouts) ->
            let acks = List.map fst acks
            let timeouts = List.map fst timeouts
            (acks, timeouts))

    let shutdown (targets: Target.T[]) (timeout: Duration option): Job<ShutdownInfo> =
      let shutdownTarget (target: Target.T) =
        Target.shutdown target ^=> fun ack -> generateProcessResult target.name ack timeout

      (targets |> Seq.Con.mapJob shutdownTarget)
      >>- (partitionResults >> ShutdownInfo)

    let flushPending (targets: Target.T[]) (timeout: Duration option): Job<FlushInfo> =
      let flushTarget (target: Target.T) =
        generateProcessResult target.name (Target.flush target) timeout

      Seq.Con.mapJob flushTarget targets
      >>- (partitionResults >> FlushInfo)

    let setMetricFailBehaviour (logger: Logger) (metricRegistry: MetricRegistry) =
      metricRegistry.setFailWith logger.warn

  // Middlewares at:
  //  - LogaryConf (goes on all loggers) (compose at call-site)
  //  - TargetConf (goes on specific target) (composes when creating target,not compose at call-site when sending message)
  //  - individual loggers (compose at call-site)
  let create (conf: LogaryConf): Job<T> =
    let ri, registryName = conf.runtimeInfo, PointName [| "Logary"; "Registry" |]
    let registryLogger = ri.logger |> Logger.setPointName registryName
    let flushCh, shutdownCh, isClosed = Ch (), Ch (), IVar ()

    Impl.spawnTargets ri conf.targets >>= fun targets ->
    let targetsByName = targets |> Array.map (fun t -> t.name, t) |> Map.ofArray

    let rec running (cancellations: ResizeArray<Cancellation>) =
      Alt.choose [
        flushCh ^=> fun (ackCh, nack, timeout) ->
          let flushOrAbort =
                memo (Impl.flushPending targets timeout) ^=> Ch.give ackCh
            <|> nack

          registryLogger.timeAlt(flushOrAbort, "flushOrAbort")
          >>=. running cancellations

        shutdownCh ^=> fun (res, timeout) ->
          registryLogger.eventAck "Shutting down"
          ^=>. Seq.Con.iterJob Cancellation.cancel cancellations
          >>=. Impl.shutdown targets timeout
          >>= fun shutdownInfo ->
              InternalLogger.shutdown ri.logger ^=>. res *<= shutdownInfo
          >>= IVar.fill isClosed
      ]

    let runnable (msg: LogaryMessageBase) =
      let targets =
        if Set.isEmpty msg.targets then
          targets
        else
          msg.targets |> Seq.choose (fun name -> Map.tryFind name targetsByName) |> Array.ofSeq

      if Array.isEmpty targets then
        NoResult
      else
        let putBufferTimeOut = if msg.waitForTargets then conf.waitForTargetsTimeout else Duration.Zero
        msg
          |> Target.logAllReduce putBufferTimeOut targets
          |> Alt.afterFun (fun result ->
              try conf.logResultHandler result
              with e -> eprintfn "%O" e
              result)
          |> HasResult

    // pipe.run should only be invoke once, because state in pipes is captured when pipe.run
    let pipe =
      conf.processing |> Pipe.run conf.runtimeInfo runnable

    Impl.setMetricFailBehaviour ri.logger conf.metricRegistry

    let middleware = Middleware.compose conf.middleware

    pipe >>= fun (runPipeOnMessage, cancellations) ->
    let state =
      { runtimeInfo = ri
        emit = middleware >> runPipeOnMessage >> PipeResult.defaultValue LogResult.success
        isClosed = isClosed
        flushCh = flushCh
        shutdownCh = shutdownCh
        loggerLevels = ConcurrentDictionary<string, LogLevel>()
        defaultLoggerLevelRules = conf.loggerLevels
        metricRegistry = conf.metricRegistry }

    Job.startIgnore (Job.supervise registryLogger (Policy.restartDelayed 512u) (running cancellations))
    >>-. state

  let toLogManager (t: T): LogManager =
    { new LogManager with
        member x.getLogger name =
          Impl.getLogger t name
        member x.runtimeInfo =
          t.runtimeInfo
        member x.flushPending dur =
          flushWithTimeout t dur
        member x.shutdown (flushTO, shutdownTO) =
          shutdownWithTimeouts t flushTO shutdownTO
        member x.switchLoggerLevel (path, logLevel) =
          Impl.switchLoggerLevel t path logLevel
        member x.metrics = t.metricRegistry
    }