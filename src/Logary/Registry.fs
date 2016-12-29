/// The registry is the composition root of Logary
namespace Logary

open Hopac
open Hopac.Infixes
open NodaTime
open System
open System.IO
open Logary.Message
open Logary.Target
open Logary.Internals

/// The promised logger is constructed through a the asynchronous call to
/// getLogger (i.e. the call to the Registry's getLogger channel). Every
/// call will return a job that is started on the global scheduler, which
/// assumes that the promise will be returned at some point in the (close)
/// future. If this assumption does not hold, we'll get an issue where all
/// of the log-method calls will put work on the global Hopac scheduler,
/// which in turn causes the 'unbounded queue' problem. However, it's
/// safe to assume that the promise will be completed shortly after the c'tor
/// of this type is called.
type internal PromisedLogger(name, requestedLogger : Job<Logger>) =
  let promised = memo requestedLogger

  /// Create a new `Logger` with the given name and `Job<Logger>` – nice to
  /// use for creating loggers that need to be immediately available.
  static member create (PointName contents as name) (logger : Job<Logger>) =
    if logger = null then nullArg "logger"
    if contents = null then nullArg "name"
    PromisedLogger(name, logger) :> Logger

  interface Logger with
    member x.name = name

    member x.logWithAck logLevel messageFactory =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck logLevel messageFactory)

    member x.log logLevel messageFactory =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck logLevel messageFactory)
      |> Alt.afterFun (fun _ -> ())

    member x.level =
      Verbose

module internal GlobalsService =
  open Globals

  let create (t : T) (ilogger : Logger) =
    let logger = ilogger |> Logger.apply (setSimpleName "Logary.Globals")
    let pauseCh, resumeCh, shutdownCh = Ch (), Ch (), Ch ()

    let rec init () =
      let prev = !config
      initialise t
      running t (fst prev)

    and running myself prev =
      Alt.choose [
        pauseCh ^=> fun (ack, nack) ->
          logger.debug (eventX "Pausing.")
          initialise prev
          ack *<= () >>=. running myself prev

        resumeCh ^=> fun (ack, nack) ->
          logger.debug (eventX "Resuming.")
          initialise myself
          ack *<= () >>=. running myself prev

        shutdownCh ^=> fun ack ->
          logger.debug (eventX "Shutting down.")
          initialise prev
          ack *<= ()
      ]

    let loop = Job.supervise logger Policy.terminate (init ())
    Service.create logger "globals" pauseCh resumeCh shutdownCh loop

  (* // cc: @oskarkarlsson ;)
  let scoped (globals : Service<Service.T>) (logger : Logger) =
    globals |> Service.pause >>-.
    { new IAsynDisposable with
        member x.AsyncDispose() =
          globals |> Service.resume
    }
  *)


module Engine =

  type T =
    private {
      heyday : bool
    }

  let create processing : Job<T> =
    Job.result { heyday = true }

/// When you validate the configuration, you get one of these.
///
/// This is the logary configuration structure having a memory of all
/// configured targets, metrics, healthchecks, middlewares, etc.
type LogaryConf =
  /// A map of the targets by name.
  abstract targets : Map<string, TargetConf>
  /// A map of metrics by name.
  abstract metrics : Map<string, MetricConf>
  /// A map of health checks by name.
  abstract healthChecks : Map<string, HealthCheckConf>
  /// Service metadata - what name etc.
  abstract runtimeInfo : RuntimeInfo
  /// Extra middleware added to every resolved logger.
  abstract middleware : Middleware[]
  /// Optional stream transformer.
  abstract processing : Processing

/// A data-structure that gives information about the outcome of a flush
/// operation on the Registry. This data structure is only relevant if the
/// flush operation had an associated timeout.
type FlushInfo = FlushInfo of acks:string list * timeouts:string list

/// A data-structure that gives information about the outcome of a shutdown
/// operation on the Registry. This data structure is only relevant if the
/// shutdown operation had an associated timeout.
type ShutdownInfo = ShutdownInfo of acks:string list * timeouts:string list

/// LogManager is the public interface to Logary and takes care of getting
/// loggers from names. It is also responsible for running Dispose at the
/// end of the application in order to run the target shutdown logic. That said,
/// the body of the software should be crash only, so even if you don't call dispose
/// terminating the application, it should continue working.
///
/// This is also a synchronous wrapper around the asynchronous actors that make
/// up logary
type LogManager =
  /// Gets the service name that is used to filter and process the logs further
  /// downstream. This property is configured at initialisation of Logary.
  abstract runtimeInfo : RuntimeInfo

  /// Get a logger denoted by the name passed as the parameter. This name can either be
  /// a specific name that you keep for a sub-component of your application or
  /// the name of the class. Also have a look at Logging.GetCurrentLogger().
  abstract getLogger : PointName -> Job<Logger>

  /// Awaits that all targets finish responding to a flush message
  /// so that we can be certain they have processed all previous messages.
  /// This function is useful together with unit tests for the targets.
  abstract flushPending : Duration -> Alt<FlushInfo>

  /// Shuts Logary down after flushing, given a timeout duration to wait before
  /// counting the target as timed out in responding. The duration is applied
  /// to each actor's communication. Does an ordered shutdown.
  ///
  /// First duration: flush duration
  /// Second duration: shutdown duration
  /// Returns the shutdown book keeping info
  abstract shutdown : flush:Duration -> shutdown:Duration -> Alt<FlushInfo * ShutdownInfo>

/// This is the main state container in Logary.
module Registry =
  /// The holder for the channels of communicating with the registry.
  type T =
    private {
      runtimeInfo : RuntimeInfo

      /// Get a logger for the given point name (the path of the logger). This
      /// operation should not fail, so there's no nack promise passed.
      getLoggerCh : Ch<PointName * Middleware option * IVar<Logger>>

      /// Flush all pending messages from the registry to await shutdown and
      /// ack on the `ackCh` when done. If the client nacks the request, the
      /// `nack` promise is filled with a unit value. Optional duration of how
      /// long the flush 'waits' for targets before returning a FlushInfo.
      flushCh : Ch<Ch<FlushInfo> * Promise<unit> * Duration option>

      /// Shutdown the registry in full.
      shutdownCh : Ch<Ch<ShutdownInfo> * Promise<unit> * Duration option>
    }

  /// Gets a logger from the registry, by name. This will always return a
  /// `Logger` value.
  let getLogger (t : T) name : Job<Logger> =
    t.getLoggerCh *<+=>- fun resCh -> name, None, resCh
    :> Job<_>

  /// Gets a logger from the registry, by name. This will always return a
  /// job with a `Logger` value.
  let getLoggerT (t : T) name : Logger =
    getLogger t name |> PromisedLogger.create name

  /// Gets a logger from the registry, by name, with attached middleware. This
  /// will always return a job with a `Logger` value.
  let getLoggerWithMiddleware (t : T) (name : PointName) (middleware : Middleware) : Job<Logger> =
    t.getLoggerCh *<+=>- fun resCh -> name, Some middleware, resCh
    :> Job<_>

  /// Gets a logger from the registry, by name, with attached middleware. This
  /// will always return a `Logger` value.
  let getLoggerWithMiddlewareT (t : T) name middleware : Logger =
    getLoggerWithMiddleware t name middleware |> PromisedLogger.create name

  /// Flush all pending messages for all targets. Flushes with no timeout; if
  /// this Alternative yields, all targets were flushed.
  let flush (t : T) : Alt<unit> =
    t.flushCh *<+->- fun flushCh nack -> flushCh, nack, None
    |> Alt.afterFun (fun _ -> ())

  /// Flush all pending messages for all targets. This Alternative always
  /// yields after waiting for the specified `timeout`; then giving back the
  /// `FlushInfo` data-structure that recorded what targets were successfully
  /// flushed and which ones timed out.
  let flushWithTimeout (t : T) (timeout : Duration) : Alt<FlushInfo> =
    t.flushCh *<+->- fun flushCh nack -> flushCh, nack, Some timeout

  /// Shutdown the registry and flush all targets before shutting it down. This
  /// function does not specify a timeout, neither for the flush nor for the
  /// shutting down of targets, and so it does not return a `ShutdownInfo`
  /// data-structure.
  let shutdown (t : T) : Alt<unit> =
    t.shutdownCh *<+->- fun shutdownCh nack -> shutdownCh, nack, None
    |> Alt.afterFun (fun _ -> ())

  /// Shutdown the registry and flush all targets before shutting it down. This
  /// function specifies both a timeout for the flushing of targets and the
  /// shutting down of the registry. The Alternative yields after a maximum of
  /// `shutdownTimeout` + `flushTimeout`, with information about the shutdown.
  let shutdownWithTimeouts (t : T) (flushTimeout : Duration) (shutdownTimeout : Duration) : Alt<FlushInfo * ShutdownInfo> =
    flushWithTimeout t flushTimeout ^=> fun flushInfo ->
    t.shutdownCh *<+->- fun shutdownCh nack -> shutdownCh, nack, Some shutdownTimeout
    |> Alt.afterFun (fun shutdownInfo -> flushInfo, shutdownInfo)

  let runtimeInfo (t : T) : RuntimeInfo =
    t.runtimeInfo

  module internal Impl =
    let createGlobals (conf : LogaryConf) (x : T) =
      let config =
        { Globals.defaultConfig with
            getLogger = getLoggerT x
            getLoggerWithMiddleware = getLoggerWithMiddlewareT x }
      GlobalsService.create config conf.runtimeInfo.logger

    let rec server (x : T) =
      Alt.choose [
        x.getLoggerCh ^=> fun (name, mid, repl) ->
          server x

        x.flushCh ^=> fun (ackCh, nack, timeout) ->
          server x

        x.shutdownCh ^=> fun (ackCh, nack, timeout) ->
          server x
      ]

  let create (conf : LogaryConf) : Job<T> =
    let logger = conf.runtimeInfo.logger |> Logger.apply (setSimpleName "Logary.Registry")
    let x = { runtimeInfo = conf.runtimeInfo; getLoggerCh = Ch (); flushCh = Ch (); shutdownCh = Ch () }
    let globals = Impl.createGlobals conf x
    let targets = conf.targets |> Map.map (fun _ -> Target.create conf.runtimeInfo)
    let metrics = conf.metrics |> Map.map (fun _ -> Metric.create conf.runtimeInfo)
    let hcs = conf.healthChecks |> Map.map (fun _ -> HealthCheck.create conf.runtimeInfo)
    let engine = Engine.create conf.processing
    Job.supervise logger (Policy.restartDelayed 500u) (Impl.server x) >>-.
    x

  let toLogManager (t : T) : LogManager =
    { new LogManager with
        member x.getLogger name =
          getLogger t name
        member x.runtimeInfo =
          t.runtimeInfo
        member x.flushPending dur =
          flushWithTimeout t dur
        member x.shutdown flushTO shutdownTO =
          shutdownWithTimeouts t flushTO shutdownTO
    }

[<AutoOpen>]
module LogManagerEx =

  type LogManager with
    /// Get a logger denoted by the name passed as the parameter. This name can either be
    /// a specific name that you keep for a sub-component of your application or
    /// the name of the class. Also have a look at Logging.GetCurrentLogger().
    member __.getLoggerT (x : LogManager) name : Logger =
      x.getLogger name
      |> PromisedLogger.create name