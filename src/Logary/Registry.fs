/// The registry is the composition root of Logary
namespace Logary

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary.Message
open Logary.Internals
open Logary.EventsProcessing
open NodaTime
open System.Text.RegularExpressions


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
  abstract processing : Events.Processing
  /// each logger's min level
  abstract loggerLevels : (string * LogLevel) list

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
  abstract getLogger : PointName -> Logger

  abstract getLoggerWithMiddleware : PointName -> Middleware -> Logger

  /// Awaits that all targets finish responding to a flush message
  /// so that we can be certain they have processed all previous messages.
  /// This function is useful together with unit tests for the targets.
  abstract flushPending : Duration -> Alt<FlushInfo>
  abstract flushPending : unit -> Alt<unit>

  /// Shuts Logary down after flushing, given a timeout duration to wait before
  /// counting the target as timed out in responding. The duration is applied
  /// to each actor's communication. Does an ordered shutdown.
  ///
  /// First duration: flush duration
  /// Second duration: shutdown duration
  /// Returns the shutdown book keeping info
  abstract shutdown : flush:Duration * shutdown:Duration -> Alt<FlushInfo * ShutdownInfo>
  abstract shutdown : unit -> Alt<unit>

  /// Dynamically controls logger min level, 
  /// this will only affect the loggers (its name, not its instance) which have been created beafore
  abstract switchLoggerLevel : string * LogLevel -> unit

/// This is the main state container in Logary.
module Registry =
  /// The holder for the channels of communicating with the registry.
  type T =
    private {
      runtimeInfo : RuntimeInfo
      msgProcessing : Message -> Middleware option -> Alt<Promise<unit>>

      /// to show whether or not registry is shutdown, used contorl message communication channel
      /// avoid blocking endless case
      isClosed : IVar<unit>

      /// Flush all pending messages from the registry to await shutdown and
      /// ack on the `ackCh` when done. If the client nacks the request, the
      /// `nack` promise is filled with a unit value. Optional duration of how
      /// long the flush 'waits' for targets before returning a FlushInfo.
      flushCh : Ch<Ch<FlushInfo> * Promise<unit> * Duration option>

      /// Shutdown the registry in full. This operation cannot be cancelled and
      /// so the caller is promised a ShutdownInfo.
      shutdownCh : Ch<IVar<ShutdownInfo> * Duration option>

      /// each logger's minLevel stores here, can be dynamically change
      loggerLevelDic : System.Collections.Concurrent.ConcurrentDictionary<string,LogLevel>

      /// default logger rules from logary conf, will be applied when create new logger
      defaultLoggerLevelRules : (string * LogLevel) list
    }

  /// Flush all pending messages for all targets. Flushes with no timeout; if
  /// this Alternative yields, all targets were flushed.
  let flush (t : T) : Alt<unit> =
    t.isClosed
    <|>
    ((t.flushCh *<+->- fun flushCh nack -> flushCh, nack, None) ^-> ignore)


  /// Flush all pending messages for all targets. This Alternative always
  /// yields after waiting for the specified `timeout`; then giving back the
  /// `FlushInfo` data-structure that recorded what targets were successfully
  /// flushed and which ones timed out.
  let flushWithTimeout (t : T) (timeout : Duration) : Alt<FlushInfo> =
    (t.isClosed ^->. (FlushInfo(["registry is closed"],[])))
    <|>
    (t.flushCh *<+->- fun flushCh nack -> flushCh, nack, Some timeout)


  /// Shutdown the registry and flush all targets before shutting it down. This
  /// function does not specify a timeout, neither for the flush nor for the
  /// shutting down of targets, and so it does not return a `ShutdownInfo`
  /// data-structure.
  let shutdown (t : T) : Alt<unit> =
    t.isClosed
    <|>
    ((t.flushCh *<+->- fun flushCh nack -> flushCh, nack, None) ^=> fun _ ->
      (t.shutdownCh *<+=>- fun shutdownCh -> shutdownCh, None) ^-> ignore)


  /// Shutdown the registry and flush all targets before shutting it down. This
  /// function specifies both a timeout for the flushing of targets and the
  /// shutting down of the registry. The Alternative yields after a maximum of
  /// `shutdownTimeout` + `flushTimeout`, with information about the shutdown.
  let shutdownWithTimeouts (t : T) (flushTimeout : Duration) (shutdownTimeout : Duration) : Alt<FlushInfo * ShutdownInfo> =
    (t.isClosed ^->. (FlushInfo(["registry is closed"],[]), ShutdownInfo(["registry is closed"],[])))
    <|>
    ((t.flushCh *<+->- fun flushCh nack -> flushCh, nack, Some flushTimeout) ^=> (fun flushInfo ->
      (t.shutdownCh *<+=>- fun shutdownCh -> shutdownCh, Some shutdownTimeout) ^-> (fun shutdownInfo ->
        flushInfo, shutdownInfo)))


  module private Impl =
    let inline ensureName name (m: Message) =
      if m.name.isEmpty then { m with name = name } else m

    let inline getLogger (t : T) name mid =
      let nameStr = name.ToString ()
      match t.loggerLevelDic.TryGetValue nameStr with
      | true, _ -> do ()
      | false, _  ->
        t.defaultLoggerLevelRules
        |> List.tryFind (fun (path, _) -> path = nameStr || Regex.IsMatch(nameStr, path))
        |> function
           | Some (_, minLevel) -> t.loggerLevelDic.[nameStr] <- minLevel
           | None -> t.loggerLevelDic.[nameStr] <- LogLevel.Info

      { new Logger with
          member x.name = name

          /// support no blocking endless, when target/registry is shutdown
          member x.logWithAck level messageFactory =
            if level >= x.level then
              (t.isClosed ^->. Promise (()))
              <|>
              (Alt.prepareFun (fun _ ->
                let msg = level |> messageFactory |> ensureName name
                t.msgProcessing msg mid))
            else Promise.instaPromise

          member x.level =
            match t.loggerLevelDic.TryGetValue nameStr with
            | true, minLevel -> minLevel
            | false, _  -> LogLevel.Info
      }

    let inline switchLoggerLevel (t : T) path minLevel =
      // maybe use msg passing style, if we need support affect loggers create after this switch.
      let regPath = Regex (path, RegexOptions.Compiled)
      let dic = t.loggerLevelDic
      dic |> Seq.iter (fun (KeyValue (name, _)) ->
        if name = path || regPath.IsMatch (name) then
          do dic.[name] <- minLevel)

    let inline spawnTarget (ri : RuntimeInfo) targets =
      targets
      |> HashMap.toList
      |> List.traverseJobA (fun (_,conf) -> Target.create ri conf)

    let inline generateProcessResult name processAlt (timeout:Duration option) =
      match timeout with
        | None -> processAlt ^->. (name,true)
        | Some duration ->
          timeOut (duration.ToTimeSpan ()) ^->. (name,false)
          <|>
          processAlt ^->. (name,true)

    let inline partitionResults results =
      results
      |> List.ofSeq
      |> List.partition snd
      |> (fun (acks, timeouts) ->
            let acks = List.map fst acks
            let timeouts = List.map fst timeouts
            (acks, timeouts))

    let inline shutdown (targets: Target.T list) (timeout: Duration option) : Job<ShutdownInfo> =
      let shutdownTarget (target: Target.T) =
        Target.shutdown target ^=> fun ack -> generateProcessResult target.Name ack timeout

      (targets |> Seq.Con.mapJob shutdownTarget)
      >>- (partitionResults >> ShutdownInfo)

    let inline flushPending (targets: Target.T list) (timeout: Duration option) : Job<FlushInfo> =
      let flushTarget (target: Target.T) =
        generateProcessResult target.Name (Target.flush target) timeout

      (targets |>  Seq.Con.mapJob flushTarget)
      >>- (partitionResults >> FlushInfo)


  // Middleware at:
  //  - LogaryConf (goes on all loggers) (through engine,and compose at call-site)
  //  - TargetConf (goes on specific target) (composes in engine when sending msg to target)
  //  - individual loggers (through engine,and compose at call-site)

  let create (conf : LogaryConf) : Job<T> =
    let ri, rname, rmid =
      conf.runtimeInfo,
      PointName [| "Logary"; "Registry" |],
      List.ofArray conf.middleware
    let rlogger = ri.logger |> Logger.apply (setName rname)

    Impl.spawnTarget ri conf.targets >>= fun targets ->
    let targetsMap = targets |> List.map (fun t -> t.Name, t) |> HashMap.ofList

    let wrapper sendMsg msg mid =
      msg
      |> Middleware.compose (mid |> Option.fold (fun s t -> t :: s) rmid)
      |> sendMsg |> PipeResult.orDefault Promise.instaPromise

    let flushCh, shutdownCh, isClosed = Ch (), Ch (), IVar ()

    let rec running ctss =
      Alt.choose [
        flushCh ^=> fun (ackCh, nack, timeout) ->
          rlogger.infoWithAck (eventX "Start Flush")
          ^=> fun _ ->
              memo (Impl.flushPending targets timeout) ^=> fun flushInfo -> (ackCh *<- flushInfo)
              <|>
              nack
          >>=. running ctss

        shutdownCh ^=> fun (res, timeout) ->
          rlogger.infoWithAck (eventX "Shutting down")
          ^=>. Seq.Con.iterJob Cancellation.cancel ctss
          >>=. Impl.shutdown targets timeout
          >>= fun shutdownInfo ->
              InternalLogger.shutdown ri.logger ^=>. res *<= shutdownInfo
          >>= IVar.fill isClosed
      ]

    // pipe.run should only be invoke once, because state in pipes is captured when pipe.run
    let runningPipe =
      conf.processing
      |> Pipe.run (fun msg ->
         let msgSinks =  msg |> Message.getAllSinks
         let sinks =
           if Set.isEmpty msgSinks then targets
           else msgSinks |> Set.toList |> List.choose (fun name -> HashMap.tryFind name targetsMap)
         if sinks.IsEmpty then NoResult
         else msg |> Target.logAll sinks |> HasResult)

    runningPipe
    >>= fun (sendMsg, ctss) ->
        let state =
          { runtimeInfo = ri
            msgProcessing = wrapper sendMsg
            isClosed = isClosed
            flushCh = flushCh
            shutdownCh = shutdownCh
            loggerLevelDic = new System.Collections.Concurrent.ConcurrentDictionary<string,LogLevel> ()
            defaultLoggerLevelRules = conf.loggerLevels }

        Job.supervise rlogger (Policy.restartDelayed 512u) (running ctss)
        |> Job.startIgnore
        >>-. state

  let toLogManager (t : T) : LogManager =
    { new LogManager with
        member x.getLogger name =
          Impl.getLogger t name None
        member x.getLoggerWithMiddleware name mid =
          Impl.getLogger t name (Some mid)
        member x.runtimeInfo =
          t.runtimeInfo
        member x.flushPending dur =
          flushWithTimeout t dur
        member x.flushPending () = flush t
        member x.shutdown (flushTO, shutdownTO) =
          shutdownWithTimeouts t flushTO shutdownTO
        member x.shutdown () = shutdown t
        member x.switchLoggerLevel (path, logLevel) =
          Impl.switchLoggerLevel t path logLevel

    }