/// The registry is the composition root of Logary
namespace Logary

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary.Message
open Logary.Internals
open Logary.Configuration
open NodaTime
open System.Text.RegularExpressions
open System.Collections.Concurrent

/// This is the logary configuration structure having a memory of all
/// configured targets, middlewares, etc.
type LogaryConf =
  /// A map of the targets by name.
  abstract targets: HashMap<string, TargetConf>
  /// Service metadata - what name etc.
  abstract runtimeInfo: RuntimeInfo
  /// Extra middleware added to every resolved logger.
  abstract middleware: Middleware[]
  /// Optional stream transformer.
  abstract processing: Processing
  /// each logger's min level
  abstract loggerLevels : (string * LogLevel) list

/// This is the main state container in Logary.
module Registry =
  /// The holder for the channels of communicating with the registry.
  type T =
    private {
      runtimeInfo: RuntimeInfo

      runPipeline: Middleware option -> Message -> LogResult

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
    }

  /// Flush all pending messages for all targets. Flushes with no timeout; if
  /// this Alternative yields, all targets were flushed.
  let flush (t: T): Alt<unit> =
    t.isClosed
    <|>
    ((t.flushCh *<+->- fun flushCh nack -> flushCh, nack, None) ^-> ignore)


  let private alreadyClosed = FlushInfo (["registry is closed"], [])

  /// Flush all pending messages for all targets. This Alternative always
  /// yields after waiting for the specified `timeout`; then giving back the
  /// `FlushInfo` data-structure that recorded what targets were successfully
  /// flushed and which ones timed out.
  let flushWithTimeout (t: T) (timeout: Duration): Alt<FlushInfo> =
    (t.isClosed ^->. alreadyClosed)
    <|>
    (t.flushCh *<+->- fun flushCh nack -> flushCh, nack, Some timeout)


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
    let inline ensureName name (m: Message) =
      if m.name.isEmpty then { m with name = name } else m

    let inline getLogger (t: T) name mid =
      let nameStr = name.ToString ()
      match t.loggerLevels.TryGetValue nameStr with
      | true, _ ->
        ()
      | false, _  ->
        let foundDefault =
          t.defaultLoggerLevelRules
          |> List.tryFind (fun (path, _) -> path = nameStr || Regex.IsMatch(nameStr, path))
        match foundDefault with
        | Some (_, minLevel) ->
          t.loggerLevels.[nameStr] <- minLevel
        | None ->
          t.loggerLevels.[nameStr] <- LogLevel.Info

      { new Logger with
          member x.name = name

          member x.logWithAck (waitForBuffers, level) messageFactory =
            if level >= x.level then
              // When the registry is shut down, reject the log message.
              let rejection = t.isClosed ^->. Result.Error Rejected
              let logMessage = Alt.prepareFun (fun () -> messageFactory level |> ensureName name |> t.runPipeline mid)
              rejection <|> logMessage
            else
              LogResult.success

          member x.level =
            match t.loggerLevels.TryGetValue nameStr with
            | true, minLevel ->
              minLevel
            | false, _  ->
              Info
      }

    let switchLoggerLevel (t: T) path minLevel =
      // maybe use msg passing style, if we need support affect loggers create after this switch.
      let regPath = Regex (path, RegexOptions.Compiled)
      t.loggerLevels
      |> Seq.iter (fun (KeyValue (name, _)) ->
        if name = path || regPath.IsMatch name then t.loggerLevels.[name] <- minLevel)

    let spawnTargets (ri: RuntimeInfo) targets =
      targets
      |> HashMap.toList
      |> List.traverseJobA (fun (_, conf) -> Target.create ri conf)
      |> Job.map List.toArray

    let generateProcessResult name processAlt (timeout:Duration option) =
      match timeout with
        | None -> processAlt ^->. (name,true)
        | Some duration ->
          timeOut (duration.ToTimeSpan ()) ^->. (name,false)
          <|>
          processAlt ^->. (name,true)

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

      (targets |> Seq.Con.mapJob flushTarget)
      >>- (partitionResults >> FlushInfo)

    let internal onKestrel =
      lazy (null <> System.Type.GetType "Microsoft.AspNetCore.Server.Kestrel.Core.KestrelServer, Microsoft.AspNetCore.Server.Kestrel.Core")
    let internal onIIS =
      lazy (let he = System.Type.GetType "System.Web.Hosting.HostingEnvironment, System.Web"
            if isNull he then false else he.GetProperty("IsHosted").GetValue(null) :?> bool)
    let internal lc () =
      if (onKestrel.Value || onIIS.Value)
         && not ("true" = Env.varDefault "LOGARY_I_PROMISE_I_HAVE_PURCHASED_LICENSE" (fun () -> "false")) then
         failwith "You must purchase a license for Logary to run it on or with IIS or Kestrel."

  // Middleware at:
  //  - LogaryConf (goes on all loggers) (through engine,and compose at call-site)
  //  - TargetConf (goes on specific target) (composes in engine when sending msg to target)
  //  - individual loggers (through engine,and compose at call-site)

  let create (conf: LogaryConf): Job<T> =
    Impl.lc ()

    let ri, rname, rmid =
      conf.runtimeInfo,
      PointName [| "Logary"; "Registry" |],
      List.ofArray conf.middleware

    let rlogger = ri.logger |> Logger.apply (setName rname)

    let wrapper sendMsg mid msg =
      msg
      |> Middleware.compose (mid |> Option.fold (fun s t -> t :: s) rmid)
      |> sendMsg
      |> PipeResult.orDefault LogResult.rejected

    let flushCh, shutdownCh, isClosed = Ch (), Ch (), IVar ()

    Impl.spawnTargets ri conf.targets >>= fun targets ->
    let byName = targets |> Array.map (fun t -> t.name, t) |> HashMap.ofArray

    let rec running ctss =
      Alt.choose [
        flushCh ^=> fun (ackCh, nack, timeout) ->
          let flushOrAbort =
            memo (Impl.flushPending targets timeout) ^=> Ch.give ackCh
            <|> nack

          rlogger.timeAlt (flushOrAbort, "flushOrAbort")
          >>=. running ctss

        shutdownCh ^=> fun (res, timeout) ->
          //printfn "SHUTTING DOWN"
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
        let sinks = Message.getAllSinks msg
        let targets =
          if Set.isEmpty sinks then
            targets
          else
            sinks |> Seq.choose (fun name -> HashMap.tryFind name byName) |> Array.ofSeq

        if Array.isEmpty targets then
          NoResult
        else
          msg |> Target.tryLogAllReduce targets |> HasResult)

    runningPipe >>= fun (sendMsg, ctss) ->
    let state =
      { runtimeInfo = ri
        runPipeline = wrapper sendMsg
        isClosed = isClosed
        flushCh = flushCh
        shutdownCh = shutdownCh
        loggerLevels = new ConcurrentDictionary<string,LogLevel> ()
        defaultLoggerLevelRules = conf.loggerLevels }

    Job.supervise rlogger (Policy.restartDelayed 512u) (running ctss)
    |> Job.startIgnore
    >>-. state

  let toLogManager (t: T): LogManager =
    { new LogManager with
        member x.getLogger name =
          Impl.getLogger t name None
        member x.getLoggerWithMiddleware name mid =
          Impl.getLogger t name (Some mid)
        member x.runtimeInfo =
          t.runtimeInfo
        member x.flushPending dur =
          flushWithTimeout t dur
        member x.flushPending () =
          flush t
        member x.shutdown (flushTO, shutdownTO) =
          shutdownWithTimeouts t flushTO shutdownTO
        member x.shutdown () =
          shutdown t
        member x.switchLoggerLevel (path, logLevel) =
          Impl.switchLoggerLevel t path logLevel
    }