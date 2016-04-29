/// The registry is the composition root of Logary
module Logary.Registry

open Hopac
open Hopac.Infixes
open NodaTime
open Logary.Internals
open Logary.Internals.Scheduling

/// Given the registry actor, and a name for the logger, get the logger from the registry.
let getLogger (registry : RegistryInstance) name : Job<Logger> =
  registry.reqCh *<+=>- fun resCh -> GetLogger (name, None, resCh)
  :> Job<_>

let getLoggerWithMiddleware (middleware : Middleware.Mid) registry name : Job<Logger> =
  registry.reqCh *<+=>- fun resCh -> GetLogger (name, Some middleware, resCh)
  :> Job<_>

/// handling flyweights
module internal Logging =
  open Hopac.Infixes
  open System
  open Logary.Target
  open Logary.Internals
  open Logary.Internals.Date

  /// Flyweight logger impl - reconfigures to work when it is configured, and
  /// until then throws away all log lines.
  type Flyweight(name) =
    let state = IVar ()

    let logger (f : Logger -> Alt<_>) def =
      if IVar.Now.isFull state then
        IVar.Now.get state |> (snd >> f)
      else
        Alt.always def

    interface Named with
      member x.name = name

    interface FlyweightLogger with
      member x.configured lm =
        getLogger lm.registry name >>= fun logger ->
        state *<= (lm, logger)

    interface Logger with
      member x.logVerboseWithAck fMessage =
        logger (flip Logger.logVerboseWithAck fMessage) (Promise.Now.withValue ())

      member x.logDebugWithAck fMessage =
        logger (flip Logger.logDebugWithAck fMessage) (Promise.Now.withValue ())

      member x.logWithAck message =
        logger (flip Logger.logWithAck message) (Promise.Now.withValue ())

      member x.level =
        Verbose

  /// Singleton configuration entry point: call from the runLogary code.
  let startFlyweights inst : Job<unit> =
    // Iterate through all flywieghts and set the current LogManager for them
    Globals.withFlyweights <| fun flyweights ->
      flyweights
      |> List.traverseJobA (fun f -> f.configured inst)
      >>- fun (_ : unit list) ->
        Globals.singleton := Some inst
        Globals.clearFlywieghts ()

  /// Singleton configuration exit point: call from shutdownLogary code
  let shutdownFlyweights () =
    Globals.singleton := None
    Globals.clearFlywieghts ()

module Advanced =
  open System
  open Logging
  open Logary
  open Logary.Configuration
  open Logary.Rule
  open Logary.Target
  open Logary.HealthCheck
  open Logary.Internals
  open Logary.Supervisor

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
        let t, ti = Map.find r.target conf.targets
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

  /// Flush all pending log lines for all targets. Returns a Nack/Ack structure describing
  /// how that went. E.g. if there's a target is has a backlog, it might not be able to
  /// flush all its pending entries within the allotted timeout (200 ms at the time of writing)
  /// but will then instead return Nack/failed RPC.
  let flushPending (registry : RegistryInstance) : Alt<unit> =
    registry.reqCh *<+->- fun flushCh nack -> FlushPending (flushCh, nack)

  /// Shutdown the registry. This will first flush all pending entries for all targets inside the
  /// registry, and then proceed to sending ShutdownTarget to all of them. If this doesn't
  /// complete within the allotted timeout, (200 ms for flush, 200 ms for shutdown), it will
  /// return Nack to the caller (of shutdown).
  let shutdown (registry : RegistryInstance) : Alt<unit> =
    registry.reqCh *<+->- fun shutdownCh nack -> ShutdownLogary (shutdownCh, nack)

  // TODO: use Alt<..> instead:

  /// Flush all registry targets, then shut it down -- the registry internals
  /// take care of timeouts, not these methods
  let flushAndShutdown (durFlush : Duration) (durShutdown : Duration) (registry : RegistryInstance) = job {
    let! fs =
      flushPending registry ^->. Ack <|>
      timeOut (durFlush.ToTimeSpan ()) ^->. Nack "Flush timed out"
    let! ss =
      shutdown registry ^->. Ack <|>
      timeOut (durShutdown.ToTimeSpan ()) ^->. Nack "Shutdown timed out"

    // TODO: granular Ack handling for each target
    return { flushed = fs
             stopped = ss
             timedOut = Seq.any (function Nack _ -> true | _ -> false) [ fs; ss ] }
  }

  module private Impl =

    type MiddlewareFacadeLogger(middleware : Message -> Alt<Promise<unit>>, name, level) as x =
      let ifLevel level otherwise f =
        let xLogger = x :> Logger
        if xLogger.level <= level then
          f ()
        else
          otherwise

      interface Named with
        member x.name = name

      interface Logger with
        member x.logVerboseWithAck fMessage =
          ifLevel Verbose (Alt.always (Promise.Now.withValue ())) <| fun _ ->
            let msg = fMessage ()
            middleware msg

        member x.logDebugWithAck fMessage =
          ifLevel Debug (Alt.always (Promise.Now.withValue ())) <| fun _ ->
            let msg = fMessage ()
            middleware msg

        member x.logWithAck message =
          middleware message

        member x.level =
          level

    let applyMiddleware (middleware : Message -> Message) logger =
      let target msg =
        middleware msg |> Logger.logWithAck logger

      MiddlewareFacadeLogger(target, logger.name, logger.level) :> Logger

    /// Create a new Logger from the targets passed, with a given name.
    let fromTargets name ilogger (targets : (MessageFilter * TargetInstance * LogLevel) list) =
      { name    = name
        targets = targets |> List.map (fun (mf, ti, _) -> mf, ti)
        level   = targets |> List.map (fun (_, _, level) -> level) |> List.min
        ilogger = ilogger }
      :> Logger

    let wasSuccessful = function
      | Success Ack      -> 0
      | TimedOut         -> 1
      | Success (Nack _) -> 1

    let bisectSuccesses = // see wasSuccessful f-n above
      fun groups -> groups |> Seq.filter (fun (k, _) -> k = 0) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList),
                    groups |> Seq.filter (fun (k, _) -> k = 1) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList)

    let nl = System.Environment.NewLine

    let toTextList s =
      let sb = new Text.StringBuilder()
      for x in s do sb.AppendLine(sprintf " * %A" x) |> ignore
      sb.ToString()

    /// The state for the Registry
    type RegistryState =
      { /// the supervisor, not of the registry actor, but of the targets and probes and health checks.
        supervisor  : Supervisor.Instance
        schedules   : (PointName * Cancellation) list }

    /// The registry function that works as a supervisor and runtime state holder
    /// for Logary.
    let rec registry conf sup sched (inbox : Ch<RegistryMessage>) : Job<_> =
      let targets = conf.targets |> Seq.map (fun kv -> kv.Value |> snd |> Option.get)
      let regPath = PointName [| "Logary"; "Registry"; "registry" |]
      let log = Message.setName regPath >> Logger.log conf.runtimeInfo.logger

      /// In the shutdown state, the registry doesn't respond to messages, but rather tries to
      /// flush and shut down all targets, doing internal logging as it goes.
      let shutdown state (ackCh : Ch<unit>) (nack : Promise<unit>) = job {
        do! Message.eventInfo "shutdown metrics polling" |> log
        do! state.schedules |> List.traverseJobA (snd >> Cancellation.cancel) |> Job.Ignore

        do! Message.eventInfo "cancel schedules" |> log
        for (_, x) in state.schedules do
          do! Cancellation.cancel x

        do! Message.eventInfo "shutdown targets" |> log
        let! allShutdown =
          targets
          |> Seq.pjmap (fun t -> Target.shutdown t  ^->. Success Ack
                                 <|> timeOutMillis 200 ^->. TimedOut)

        let stopped, failed =
          allShutdown
          |> Seq.groupBy wasSuccessful
          |> bisectSuccesses

        if List.isEmpty failed then
          do! Message.eventInfo "shutdown Ack" |> log
        else
          let msg = sprintf "failed target shutdowns:%s%s" nl (toTextList failed)
          do! Message.eventErrorf "shutdown Nack%s%s" nl msg
              |> Message.setField "failed" (Value.ofObject failed)
              |> log

        do! Ch.give ackCh () <|> nack
        return! Message.eventInfo "shutting down immediately" |> log
      }

      let rec init state = job {
        let! ctss =
          conf.metrics
          |> Seq.toList
          |> List.traverseJobA (function
            | KeyValue (name, mconf) -> job {
              let! instance = mconf.initialise name
              do! Message.eventDebugf "will poll %O every %O" name mconf.tickInterval |> log

              let! tickCts =
                Scheduling.schedule sched Metric.tick instance mconf.tickInterval
                                    (Some mconf.tickInterval)
              do! Message.eventDebugf "getting logger for metric %O" name |> log
              let logger = name |> getTargets conf |> fromTargets name conf.runtimeInfo.logger
              Metric.tapMessages instance |> Stream.consumeJob (Logger.log logger)
              return name, tickCts
            })

        return! running { state with schedules = ctss }
        }

      /// In the running state, the registry takes queries for loggers, gauges, etc...
      and running state : Job<_> = job {
        let! msg = Ch.take inbox
        match msg with
        | GetLogger (name, extraMiddleware, chan) ->

          let middleware =
            match extraMiddleware with
            | None -> conf.middleware
            | Some mid -> mid :: conf.middleware
                       
          do! Message.eventDebugf "composing %i middlewares" (List.length middleware) |> log
          let composedMiddleware = Middleware.compose middleware
          
          let logger =
            name
            |> getTargets conf
            |> fromTargets name conf.runtimeInfo.logger
            |> applyMiddleware (composedMiddleware)

          do! IVar.fill chan logger
          return! running state

        | FlushPending(ackCh, nack) ->
          do! Message.eventInfo "flush start" |> log

          let! allFlushed =
            targets
            |> Seq.pjmap (fun t -> Target.flush t    ^->. Success Ack <|>
                                   timeOutMillis 200 ^->. TimedOut)

          let flushed, notFlushed =
            allFlushed
            |> Seq.groupBy wasSuccessful
            |> bisectSuccesses

          if List.isEmpty notFlushed then
            do! Message.eventInfo "flush Ack" |> log
          else
            let msg = sprintf "Failed target flushes:%s%s" nl (toTextList notFlushed)
            do! Message.eventErrorf "flush Nack - %s" msg |> log

          do! Ch.give ackCh () <|> nack

          return! running state

        | ShutdownLogary(ackCh, nack) ->
          return! shutdown state ackCh nack }

      init { supervisor = sup; schedules = [] }

    let create (conf : LogaryConf) (sup : Supervisor.Instance)
               (sched : Ch<ScheduleMsg>)
               : RegistryInstance =
      let regCh = Ch ()
      Job.Global.start (registry conf sup sched regCh)
      { reqCh = regCh }

  /// Start a new registry with the given configuration. Will also launch/start
  /// all targets that are to run inside the registry. Returns a newly
  /// configured LogaryInstance.
  ///
  /// It is not until runRegistry is called, that each target gets its service
  /// metadata, so it's no need to pass the metadata to the target before this.
  let create (conf : LogaryConf) =
    let log =
      Message.setName (PointName [| "Logary"; "Registry"; "create" |])
      >> Logger.log conf.runtimeInfo.logger

    let sopts = Supervisor.Options.create (conf.runtimeInfo.logger, 10u, Duration.FromMilliseconds 500L)
    let sup = Supervisor.create sopts

    // Supervisor must be running so we can register new jobs
    Supervisor.start sup

    let targets =
      conf.targets
      |> Map.map (fun _ (tconf, _) ->
        tconf,
        // TODO: consider this blocking call?
        Some (Target.init conf.runtimeInfo tconf |> run))

    let confNext = { conf with targets = targets }

    let listen =
      targets
      |> Seq.map (fun kv -> kv.Value |> snd |> Option.get)
      |> Seq.map (fun ti ->
        let namedJob = NamedJob.create (PointName.format ti.name) ti.server
        Supervisor.register sup namedJob)
      |> Job.conIgnore

    let sched = Scheduling.create ()
    let reg = Impl.create confNext sup sched

    listen |> Job.map (fun _ ->
    { supervisor  = sup
      registry    = reg
      scheduler   = sched
      runtimeInfo = confNext.runtimeInfo
      middleware  = confNext.middleware })