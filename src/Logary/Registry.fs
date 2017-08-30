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
open Hopac.Extensions

module internal GlobalService =
  open Global

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

    let shutdown : Alt<Promise<unit>> =
      shutdownCh *<-=>= fun repl -> repl
      |> Alt.afterFun (fun iv -> iv :> _)

    let loop = Job.supervise logger Policy.terminate (init ())
    Service.create logger "globals" pauseCh resumeCh shutdown loop

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
      subscriptions : HashMap<string, Message -> Job<unit>>
      inputCh : Ch<LogLevel * (LogLevel -> Message) * IVar<unit>>
      shutdownCh : Ch<IVar<unit>>
      subscriberCh : Ch<string * (Message->Job<unit>)> 
    }

  let processing msg subscribers pipe =
    let onNext = pipe.run <| (fun msg ->
          let targetName = Message.tryGetContext "target" msg
          match targetName with 
          | Some (String targetName) ->
            let subscriber = HashMap.tryFind targetName subscribers 
            match subscriber with
            | Some subscriber -> subscriber msg
            | _ -> Job.result ()
          | _ -> Job.result ())
    
    onNext msg

  let create (pipe:Processing) : Job<T> =
    let inputCh, shutdownCh, subscriberCh = Ch (), Ch (), Ch ()
    let engine = { 
                   subscriptions = HashMap.empty
                   inputCh = inputCh
                   shutdownCh = shutdownCh
                   subscriberCh = subscriberCh
                 }

    let rec loop ctss (subsribers:HashMap<string, Message -> Job<unit>>) =
      Alt.choose [
        inputCh ^=> fun (level, messageFactory, reply) -> 
          processing (messageFactory level) subsribers pipe
          ^=> fun _ -> reply *<= () 
          >>=. loop ctss subsribers

        subscriberCh ^=> fun (key, sink) ->
          subsribers
          |> HashMap.add key sink 
          |> loop ctss

        shutdownCh ^=> fun reply ->
          ctss 
          |> Seq.Con.iterJob Cancellation.cancel
          >>=. reply *<= ()
      ]

    Seq.Con.mapJob id pipe.tickTimerJobs
    >>= fun ctss -> Job.start (loop ctss engine.subscriptions)
    >>-. engine

  let subscribe (engine : T) (key:string) (sink : Message -> Job<unit>) : Job<unit> =
    engine.subscriberCh *<- (key, sink)
    :> Job<unit>

  let shutdown (engine : T) =
    engine.shutdownCh *<-=>- id

  let logWithAck (engine : T) (logLevel : LogLevel) (messageFactory : LogLevel -> Message) : Alt<Promise<unit>> =
    let reply = IVar ()
    engine.inputCh *<- (logLevel, messageFactory, reply)
    ^->. upcast reply

  let log (engine : T) (logLevel : LogLevel) (messageFactory : LogLevel -> Message) : Alt<unit> =
    logWithAck engine logLevel messageFactory ^-> ignore


/// When you validate the configuration, you get one of these.
///
/// This is the logary configuration structure having a memory of all
/// configured targets, middlewares, etc.
type LogaryConf =
  /// A map of the targets by name.
  abstract targets : HashMap<string, TargetConf>
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

      engine      : Engine.T

      /// Get a logger for the given point name (the path of the logger). This
      /// operation should not fail, so there's no nack promise passed.
      getLoggerCh : Ch<PointName * Middleware option * IVar<Logger>>

      /// Flush all pending messages from the registry to await shutdown and
      /// ack on the `ackCh` when done. If the client nacks the request, the
      /// `nack` promise is filled with a unit value. Optional duration of how
      /// long the flush 'waits' for targets before returning a FlushInfo.
      flushCh : Ch<Ch<FlushInfo> * Promise<unit> * Duration option>

      /// Shutdown the registry in full. This operation cannot be cancelled and
      /// so the caller is promised a ShutdownInfo.
      shutdownCh : Ch<IVar<ShutdownInfo> * Duration option>
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
    flush t ^=> fun _ ->
    t.shutdownCh *<-=>- fun shutdownCh -> shutdownCh, None
    ^-> ignore

  /// Shutdown the registry and flush all targets before shutting it down. This
  /// function specifies both a timeout for the flushing of targets and the
  /// shutting down of the registry. The Alternative yields after a maximum of
  /// `shutdownTimeout` + `flushTimeout`, with information about the shutdown.
  let shutdownWithTimeouts (t : T) (flushTimeout : Duration) (shutdownTimeout : Duration) : Alt<FlushInfo * ShutdownInfo> =
    flushWithTimeout t flushTimeout ^=> fun flushInfo ->
    t.shutdownCh *<-=>- fun shutdownCh -> shutdownCh, Some shutdownTimeout
    |> Alt.afterFun (fun shutdownInfo -> flushInfo, shutdownInfo)

  let runtimeInfo (t : T) : RuntimeInfo =
    t.runtimeInfo

  module internal Impl =

    let createLogger engine name mid =
      let logger =
        { new Logger with
            member x.name = name
            member x.level = Verbose // TOOD: ship back from engine?
            member x.log level messageFactory =
              Engine.log engine level messageFactory
            member x.logWithAck level messageFactory =
              Engine.logWithAck engine level messageFactory
        }

      Logger.apply mid logger

    let createGlobals (conf : LogaryConf) (x : T) =
      let config =
        { Global.defaultConfig with
            getLogger = getLoggerT x
            getLoggerWithMiddleware = getLoggerWithMiddlewareT x }
      GlobalService.create config conf.runtimeInfo.logger

    let spawnTarget (ri : RuntimeInfo) targets =
      targets
      |> List.ofSeq
      |> List.traverseJobA (fun (KeyValue (_,conf)) -> 
           Target.create ri conf >>= fun instance ->
           let (minionName,supervisedJob) = Target.toMinions instance conf.policy
           supervisedJob >>-. (minionName,instance))


    let inline generateProcessResult name processAlt (timeout:Duration option) = 
      match timeout with
        | None -> processAlt ^->. (name,true)
        | Some duration -> 
          timeOut (duration.ToTimeSpan ()) ^->. (name,false) 
          <|>
          processAlt ^->. (name,true)

    let shutdown targets (timeout:Duration option) : Job<ShutdownInfo> =
      let shutdownTarget (name,target) =
        generateProcessResult name (Target.shutdown target ^=> id) timeout

      targets |> Seq.Con.mapJob shutdownTarget
      >>- fun shutdownInfos ->
        shutdownInfos
        |> List.ofSeq
        |> List.partition snd
        |> (fun (acks, timeouts) ->
              let acks = List.map (fst >> PointName.format)  acks
              let timeouts = List.map (fst >> PointName.format) timeouts
              ShutdownInfo(acks,timeouts))
      

    let flushPending targets (timeout:Duration option) : Job<FlushInfo> =
      let flushTarget (name,target) =
        generateProcessResult name (Target.flush target) timeout

      targets |> Seq.Con.mapJob flushTarget
      >>- fun flushInfos ->
        flushInfos
        |> List.ofSeq
        |> List.partition snd
        |> (fun (acks, timeouts) ->
              let acks = List.map (fst >> PointName.format)  acks
              let timeouts = List.map (fst >> PointName.format) timeouts
              FlushInfo(acks,timeouts))

  open Impl

  // Middleware at:
  //  - LogaryConf (goes on all loggers) (composes here)
  //  - TargetConf (goes on specific target) (composes in engine)
  //  - individual loggers (composes at call-site, or in #create methods of services)

  let create (conf : LogaryConf) : Job<T> =
    let ri, rname, rmid =
      conf.runtimeInfo,
      PointName [| "Logary"; "Registry" |],
      List.ofArray conf.middleware
    let rlogger = ri.logger |> Logger.apply (setName rname)
   
    spawnTarget ri conf.targets >>= fun targets ->
    Engine.create Pipe.start >>= fun engine ->

    let getLoggerCh, flushCh, shutdownCh = Ch (), Ch (), Ch ()

    let rec running () =
      Alt.choose [
        getLoggerCh ^=> fun (name, lmid, repl) ->
          let cmid = Middleware.compose (lmid |> Option.fold (fun s t -> t :: s) rmid)
          repl *<= createLogger engine name cmid >>= running 

        flushCh ^=> fun (ackCh, nack, timeout) ->
          rlogger.infoWithAck (eventX "Start Flush")
          ^=> fun _ ->
            memo (flushPending targets timeout >>= fun flushInfo -> (ackCh *<- flushInfo))
            <|> 
            nack
          ^=> running 

        shutdownCh ^=> fun (res, timeout) ->
          rlogger.infoWithAck (eventX "Shutting down")
          ^=>. Engine.shutdown engine 
          ^=>. shutdown targets timeout
          >>= fun shutdownInfo -> res *<= shutdownInfo
      ]

    let state =
      { runtimeInfo = ri
        engine = engine
        getLoggerCh = getLoggerCh
        flushCh = flushCh
        shutdownCh = shutdownCh }

    createGlobals conf state
    >>= fun globals ->
      targets |> Seq.Con.iterJob (fun (name,target) -> 
        let logJob = fun msg -> Target.log target msg >>= id
        Engine.subscribe engine (PointName.format name) logJob)
    >>=. Job.supervise rlogger (Policy.restartDelayed 500u) (running ()) 
    >>-. state

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
    member x.getLoggerT name : Logger =
      x.getLogger name
      |> PromisedLogger.create name