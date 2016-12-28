/// The registry is the composition root of Logary
namespace Logary

open Hopac
open Hopac.Infixes
open NodaTime
open System
open System.IO
open Logary.Supervisor
open Logary.Target
open Logary.Internals

/// See the docs on the funtions for descriptions on how Ack works in conjunction
/// with the promise.
type Logger =
  /// The PointName for this `Logger`: corresponds to the `name` field for the
  /// `Messages` produced from this instance.
  abstract name : PointName

  /// Write a message to the Logger. The returned value represents the commit
  /// point that Logary has acquired the message. The alternative is always
  /// selectable (through `Alt.always ()` if the logger filtered out the message
  /// due to a Rule).
  ///
  /// If the Message was not filtered through a Rule, but got sent onwards, the
  /// promise is there to denote the ack that all targets have successfully
  /// flushed the message. If you do not commit to the Alt then it will not be
  /// logged.
  ///
  /// If you choose to not await the Promise/Ack it makes no difference, since
  /// it will be garbage collected in the end, whether it's awaited or not.
  abstract logWithAck : LogLevel -> (LogLevel -> Message) -> Alt<Promise<unit>>

  /// Logs with the specified log level with backpressure via Logary's
  /// buffers.
  ///
  /// Calls to this function will block the caller only while executing the
  /// callback (if the level is active).
  ///
  /// The returned async value will yield when the message has been added to
  /// the buffers of Logary.
  ///
  /// You need to start the (cold) Alt value for the logging to happen.
  ///
  /// You should not do blocking/heavy operations in the callback.
  abstract log : LogLevel -> (LogLevel -> Message) -> Alt<unit>

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged.
  abstract level : LogLevel
  //abstract bisect : (LogLevel -> Message) -> Alt<Promise<unit>>


/// A disposable interface to use with `use` constructs and to create child-
/// contexts. Since it inherits Logger, you can pass this scope down into child
/// function calls. This interface should dovetail with how Zipkin/Dapper
/// manages parent/child spans.
type LoggerScope =
  inherit IDisposable
  inherit Logger

type TimeScope =
  inherit LoggerScope

  /// Gets the currently elapsed duration of this time scope scope.
  abstract elapsed : Duration

/// The type-signature for middleware; next:(Message -> Message) -> message:Message -> Message.
type Middleware =
  (Message -> Message) -> Message -> Message

/// A StringFormatter is the thing that takes a message and returns it as a
/// string that can be printed, sent or otherwise dealt with in a manner that
/// suits the target.
type MessageFormatter =
  abstract format : Message -> TextWriter -> unit

type ServiceState =
  | Initialising
  | Running
  | Paused
  | ShuttingDown
  | Shutdown
  | Faulted of unhandled:exn

/// A service is an abstract interface that encapsulates a service
/// implementation.
type internal Service<'t> =
  abstract name : string
  abstract instance : 't

type InitialisingService<'t> =
  inherit Service<'t>

and RunningService<'t> =
  inherit Service<'t>

and PausedService<'t> =
  inherit Service<'t>

and ShutdownService =
  /// Gets the list of service states and the duration that the service was in
  /// each of them.
  abstract transitions : (ServiceState * Duration) list

module Service =

  type T =
    private {
      name : PointName
      startIV : IVar<unit>
      pauseCh : Ch<IVar<unit> * Promise<unit>>
      resumeCh : Ch<IVar<unit> * Promise<unit>>
      shutdownCh : Ch<IVar<unit>>
      getStateCh : Ch<IVar<ServiceState>>
      serverLoop : Promise<Choice<unit, exn>>
      ilogger : Logger
    }
  with
    override x.ToString() =
      sprintf "Service(name=%O)" x.name

  let start (t : InitialisingService<T>) : Job<RunningService<T>> =
    t.instance.startIV *<= () >>-. 
    { new RunningService<T> with
        member x.name = t.name
        member x.instance = t.instance }

  let pause (t : RunningService<T>) : Alt<PausedService<T>> =
    Unchecked.defaultof<Alt<PausedService<T>>>

  let resume (t : PausedService<T>) : Alt<RunningService<T>> =
    Unchecked.defaultof<Alt<RunningService<T>>>

  let shutdown (t : #Service<T>) : Alt<ShutdownService> =
    Unchecked.defaultof<Alt<ShutdownService>>

  let getState (t : #Service<T>) : Job<ServiceState> =
    let hasExited =
      t.instance.serverLoop ^->
      (Choice.bimap (fun () -> Shutdown) Faulted >> Choice.get)

    t.instance.getStateCh *<+=>- id <|> hasExited
    :> Job<_>

  /// Create a new service from a supervised job. It's up to the caller to
  /// decide what sort of policy the supervised job should have.

  let create (internalLogger : Logger) name pauseCh resumeCh shutdownCh (server : SupervisedJob<unit>) : Job<InitialisingService<T>> =
    let ilogger = internalLogger |> Logger.apply (setSimpleName (sprintf "Logary.Service(%s)" name))
    let pn = PointName.ofSingle name
    let t =
      { name = pn
        startIV = IVar ()
        pauseCh = pauseCh
        resumeCh = resumeCh
        shutdownCh = shutdownCh
        getStateCh = Ch ()
        serverLoop = memo server
        ilogger = ilogger }

    Job.start (t.startIV >>-. t.serverLoop >>-. ()) >>-.
    { new InitialisingService<T> with
        member x.instance = t
        member x.name = name
    }

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
  static member create (PointName contents as name) logger =
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

  let create (t : T) (internalLogger : Logger) =
    let ilogger = internalLogger |> Logger.apply (setSimpleName "Logary.Globals")
    let pauseCh, resumeCh, shutdownCh = Ch (), Ch (), Ch ()

    let rec init () =
      let prev = !config
      initialise t
      running t (fst prev)

    and running myself prev =
      Alt.choose [
        pauseCh ^=> fun (ack, nack) ->
          ilogger.debug (eventX "Pausing")
          initialise prev
          ack *<= () >>=. running myself prev

        resumeCh ^=> fun (ack, nack) ->
          ilogger.debug (eventX "Resuming")
          initialise myself
          ack *<= () >>=. running myself prev

        shutdownCh ^=> fun ack ->
          ilogger.debug (eventX "Shutting down")
          initialise prev
          ack *<= ()
      ]

    let loop = Job.supervise Policy.terminate (init ())
    Service.create internalLogger "globals" pauseCh resumeCh shutdownCh loop

  (* // cc: @oskarkarlsson ;)
  let scoped (globals : Service<Service.T>) (logger : Logger) =
    globals |> Service.pause >>-.
    { new IAsynDisposable with
        member x.AsyncDispose() =
          globals |> Service.resume
    }
  *)


/// The low-water-mark for stream processing nodes (operators)
type LWM = EpochNanoSeconds

module Engine =

  type T =
    private {
      heyday : bool
    }

  let create processing : Job<T> =
    Job.result { heyday = true }

(* TODO:

 - Extract a stream of all messages
 - Run these as services:
   * Target
   * Engine
   * Metric
   * HealthCheck
   * Globals
*)

/// A function, that, given a stream of all events in the system, is allowed
/// to process them and can send new `Message` values to the channel.
/// Should return an Alt so that it can be cancelled. Hoisted into a metric
/// in the end.
type Processing = Processing of (Stream<Message> -> Ch<Message> -> Alt<unit>)

/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  /// Name of the service. Will show up as 'service' in e.g. Logstash/Kibana and
  /// is the basis for a lot of the sorting and health checking that Riemann
  /// does.
  abstract serviceName : string
  /// The host name of the machine that is running Logary. This is almost
  /// always required to coordinate logs in a distributed system and is
  /// also useful when reading logs from multiple machines at the same time.
  abstract host : string
  /// Gets the current timestamp
  abstract getTimestamp : unit -> EpochNanoSeconds
  /// Gets the console semaphore
  abstract getConsoleSemaphore : unit -> obj
  /// An internal logger for Logary's runtime, its {targets,metrics,...} to use.
  abstract logger : Logger

module RuntimeInfo =
  /// Create a new RuntimeInfo record from the passed parameters.
  ///
  /// This function gives you the ability to pass a custom clock to use within
  /// logary, as well as a host name that the logary source has.
  let create (serviceName : string) (host : string) (clock : NodaTime.IClock) =
    failwith "ensure using Globals"
    { new RuntimeInfo with
        member x.serviceName = serviceName
        member x.host = host
        member x.getTimestamp () = clock.Now.Ticks * Constants.NanosPerTick
        member x.getConsoleSemaphore () = obj ()
        member x.logger = NullLogger () :> _ }

/// Values that a node can take as input.
[<RequireQualifiedAccess>]
type NodeInput =
  | M of Message
  /// A low-water-mark signal that can be repeatedly committed to. (TBD: back
  /// with a MVar?)
  ///
  /// invariant: all Messages after a LWM have >= timestamp.
  | LWM of LWM

/// A node in a stream processing engine
type Node =
  /// An alternative that can be repeatedly committed to in order to receive
  /// messages in the node.
  abstract inputs : Alt<NodeInput>
  /// Gives you a way to perform internal logging and communicate with Logary.
  abstract runtimeInfo : RuntimeInfo

/// Logary's way to talk with Metrics.
type MetricAPI =
  inherit Node
  /// The metric can produce values through this function call.
  abstract produce : Message -> Alt<unit> // Stream.Src<Message>
  /// A channel that the metric needs to select on and then ACK once the target
  /// has fully shut down. 
  abstract shutdownCh : Ch<IVar<unit>>

// TODO: a way of composing these metrics via operators to filter them

type MetricConf =
  { name       : string
    bufferSize : uint32
    policy     : Policy
    server     : RuntimeInfo * MetricAPI -> Job<unit> }

  override x.ToString() =
    sprintf "MetricConf(name = %s)" x.name

module Metric =

  type T =
    private {
      shutdownCh : Ch<IVar<unit>>
      pauseCh : Ch<unit>
      resumeCh : Ch<unit>
      // etc...
    }

  let create () : Job<T> =
    Job.result 
      { shudownCh = Ch ()
        pauseCh = Ch ()
        resumeCh = Ch ()
      }

type Check =
  | Healthy of contents:string
  | Warning of contents:string
  | Critical of contents:string

type HealthCheckAPI =
  inherit Node
  /// Health checks should call this to produce their output as often as they
  /// like.
  abstract produce : Check -> Alt<unit>

/// When you validate the configuration, you get one of these.
///
/// This is the logary configuration structure having a memory of all
/// configured targets, metrics, healthchecks, middlewares, etc.
type LogaryConf =
  /// A list of rules that guide what targets are invoked for a given
  /// message.
  abstract rules : Rule[]
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
  abstract processing : Processing option

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
  inherit IAsyncDisposable

  /// Gets the service name that is used to filter and process the logs further
  /// downstream. This property is configured at initialisation of Logary.
  abstract runtimeInfo : RuntimeInfo

  /// Get a logger denoted by the name passed as the parameter. This name can either be
  /// a specific name that you keep for a sub-component of your application or
  /// the name of the class. Also have a look at Logging.GetCurrentLogger().
  abstract getLoggerAsync : PointName -> Job<Logger>

  /// Get a logger denoted by the name passed as the parameter. This name can either be
  /// a specific name that you keep for a sub-component of your application or
  /// the name of the class. Also have a look at Logging.GetCurrentLogger().
  abstract getLogger : PointName -> Logger

  /// Awaits that all targets finish responding to a flush message
  /// so that we can be certain they have processed all previous messages.
  /// This function is useful together with unit tests for the targets.
  abstract flushPending : Duration -> Alt<unit>

  /// Shuts Logary down after flushing, given a timeout duration to wait before
  /// counting the target as timed out in responding. The duration is applied
  /// to each actor's communication. Does an ordered shutdown.
  ///
  /// First duration: flush duration
  /// Second duration: shutdown duration
  /// Returns the shutdown book keeping info
  abstract shutdown : flush:Duration -> shutdown:Duration -> Job<FlushInfo * ShutdownInfo>

/// This is the main state container in Logary.
module Registry =

  module internal Impl =
    open System
    open Logging
    open Logary
    open Logary.Configuration
    open Logary.Rule
    open Logary.Target
    open Logary.HealthCheck
    open Logary.Internals

    /// Given a configuration and name to find all targets for, looks up all targets
    /// from the configuration matching the passed name and create a composite
    /// acceptor/filter (any matching acceptor).
    let getTargets conf (name : PointName) =
      let rules (rules, _, _) = rules

      let createFilter minLvl rules =
        let filters = Seq.map (fun (r: Rule) -> r.messageFilter) rules
        fun (msg : Message) ->
          msg.level >= minLvl
          && filters |> Seq.any (fun filter -> filter msg)

      conf.rules
      // first, filter by name
      |> matching name

      // map the target conf and target instance
      |> List.map (fun r ->
          let t, ti = Map.find r.targetName conf.targets
          r, t, (Option.get ti))

      // rules applying to the same target are grouped
      |> Seq.groupBy (fun (r, t, ti) -> t.name)

      // combine acceptors with Seq.any/combineAccept
      |> Seq.map (fun (_, ts) ->
          let _, t, ti = Seq.head ts
          let rs       = Seq.map rules ts
          let minLvl   = rs |> Seq.map (fun r -> r.level) |> Seq.min
          // find the min matching level from all rules for this target
          createFilter minLvl rs,
          t, ti,
          minLvl)

      // targets should be distinctly returned (deduplicated, so that doubly matching
      // rules don't duply log)
      |> Seq.distinctBy (fun (_, t, _, _) ->
          t.name)

      // project only the messageFilter and the target instance
      |> Seq.map (fun (messageFilter, _, ti, level) ->
          messageFilter, ti, level)

      // back to a list
      |> List.ofSeq

    let running (running : ResizeArray<RunningService<Service.T>>) =
      Alt.always ()

  /// The holder for the channels of communicating with the registry.
  type T =
    private {
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

  let create (conf : LogaryConf) : Job<T> =
    let t =
      { getLoggerCh = Ch ()
        flushCh     = Ch ()
        shutdownCh  = Ch () }
    let config = { Globals.defaultConfig with getLogger = getLoggerT t
                                              getLoggerWithMiddleware = getLoggerWithMiddlewareT t }
    let globals = Globals.create config conf.runtimeInfo.logger
    let logger = conf.runtimeInfo.logger |> Logger.apply (setSimpleName "Logary.Registry")
    let targets = conf.targets |> Map.map (Target.create)
    let metrics = conf.metrics |> Map.map (Metric.create)
    let hcs = conf.healthChecks |> Map.map (HealthCheck.create conf.runtimeInfo.logger)
    let engine = Engine.create conf.processing
    let services =
      let label label = Seq.map (fun (KeyValue (k, v)) -> sprintf "Logary.Services.%s(%s)" label k, v)
      Map [
        yield! targets |> Target.toService conf.runtimeInfo.logger |> label "Target"
        yield! metrics |> Metric.toService conf.runtimeInfo.logger |> label "Metric"
        yield! hcs |> HealthCheck.toService conf.runtimeInfo.logger |> label "HealthCheck"
        yield "Logary.Services.Engine", Engine.toService conf.runtimeInfo.logger engine
        yield "Logary.Services.Globals", globals
      ]

    let initialise =
      let running = services |> Seq.map (snd >> Service.start) |> Job.conCollect
      running >>= Impl.running

    Job.start initialise >>-. t