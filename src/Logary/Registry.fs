/// The registry is the composition root of Logary
module Logary.Registry

open Hopac
open Hopac.Infixes
open NodaTime
open Logary.Message
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
    let instaPromise = Alt.always (Promise (()))
    let insta = Alt.always ()

    let logger (f : Logger -> _) defaul =
      if IVar.Now.isFull state then
        let _, logger = IVar.Now.get state
        f logger
      else
        defaul

    interface Named with
      member x.name = name

    interface FlyweightLogger with
      member x.configured lm =
        getLogger lm.registry name >>= fun logger ->
        state *<= (lm, logger)

    interface Logger with
      member x.logWithAck logLevel messageFactory =
        logger (fun logger -> logger.logWithAck logLevel messageFactory) instaPromise

      member x.log logLevel messageFactory =
        logger (fun logger -> logger.log logLevel messageFactory) insta

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
    /// Create a new Logger from the targets passed, with a given name.
    let fromTargets name ilogger (targets : (MessageFilter * TargetInstance * LogLevel) list) =
      let minLevel =
        match targets |> List.map (fun (_, _, level) -> level) with
        | [] -> Fatal
        | levels -> List.min levels
      { name    = name
        targets = targets |> List.map (fun (mf, ti, _) -> mf, ti)
        level   = minLevel
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
      let logger = conf.runtimeInfo.logger |> Logger.apply (Message.setName regPath)

      /// In the shutdown state, the registry doesn't respond to messages, but rather tries to
      /// flush and shut down all targets, doing internal logging as it goes.
      let shutdown state (ackCh : Ch<unit>) (nack : Promise<unit>) = job {
        do! logger.info (eventX "Shutting down polling of the metrics")
        do! state.schedules |> List.traverseJobA (snd >> Cancellation.cancel) |> Job.Ignore

        do! logger.info (eventX "Cancelling schedules")
        for (_, x) in state.schedules do
          do! Cancellation.cancel x

        do! logger.info (eventX "Shutting down targets")
        let! allShutdown =
          targets
          |> Seq.pjmap (fun t -> Target.shutdown t  ^->. Success Ack
                                 <|> timeOutMillis 200 ^->. TimedOut)

        let stopped, failed =
          allShutdown
          |> Seq.groupBy wasSuccessful
          |> bisectSuccesses

        if List.isEmpty failed then
          do! logger.verbose (eventX "Shutdown Ack")
        else
          let msg = sprintf "Failed target shutdowns:%s%s" nl (toTextList failed)
          do! logger.error (eventX "Shutdown Nack with {message}"
                            >> setField "message" msg
                            >> setField "failed" (Value.ofObject failed))

        do! Ch.give ackCh () <|> nack
        return! logger.verbose (eventX "Shutting down immediately")
      }

      let rec init state = job {
        let! ctss =
          conf.metrics
          |> Seq.toList
          |> List.traverseJobA (function
            | KeyValue (name, mconf) -> job {
              let! instance = mconf.initialise name
              do! logger.verbose (eventX "Will poll {name} every {interval}"
                                  >> setField "name" name
                                  >> setFieldFromObject "interval" mconf.tickInterval)

              let! tickCts =
                Scheduling.schedule Metric.tick instance mconf.tickInterval
                                    (Some mconf.tickInterval)
                                    sched
              do! logger.verbose (eventX "Getting logger for metric {name}" >> setField "name" name)
              let logger = name |> getTargets conf |> fromTargets name conf.runtimeInfo.logger
              Metric.tapMessages instance |> Stream.consumeJob (fun msg ->
                logger.logWithAck msg.level (fun _ -> msg)
                |> Job.Ignore
              )
              return name, tickCts
            })

        return! running { state with schedules = ctss }
        }

      /// In the running state, the registry takes queries for loggers, gauges, etc...
      and running state : Job<_> = job {
        let! msg = Ch.take inbox
        match msg with
        | GetLogger (name, extraMiddleware, replCh) ->
          let middleware =
            match extraMiddleware with
            | None -> conf.middleware
            | Some mid -> mid :: conf.middleware

          do! logger.verbose (
                eventX "Composing {length} middlewares"
                >> setField "length" (List.length middleware))

          let composedMiddleware = Middleware.compose middleware

          let logger =
            name
            |> getTargets conf
            |> fromTargets name conf.runtimeInfo.logger
            |> Logger.apply composedMiddleware

          do! IVar.fill replCh logger
          return! running state

        | FlushPending(ackCh, nack) ->
          do! logger.debug (eventX "Starting to flush")

          let! allFlushed =
            targets
            |> Seq.pjmap (fun t -> Target.flush t    ^->. Success Ack <|>
                                   timeOutMillis 200 ^->. TimedOut)

          let flushed, notFlushed =
            allFlushed
            |> Seq.groupBy wasSuccessful
            |> bisectSuccesses

          if List.isEmpty notFlushed then
            do! logger.verbose (eventX "Completed flush")
          else
            let msg = sprintf "Failed target flushes:%s%s" nl (toTextList notFlushed)
            do! logger.error (eventX "Flush failed with {nack}." >> setField "nack" msg)

          do! Ch.give ackCh () <|> nack

          return! running state

        | ShutdownLogary(ackCh, nack) ->
          return! shutdown state ackCh nack }

      init { supervisor = sup; schedules = [] }

    let create (conf : LogaryConf) (sup : Supervisor.Instance)
               (sched : Ch<ScheduleMsg>)
               : RegistryInstance =
      let regCh = Ch ()
      start (registry conf sup sched regCh)
      { reqCh = regCh }

  /// Start a new registry with the given configuration. Will also launch/start
  /// all targets that are to run inside the registry. Returns a newly
  /// configured LogaryInstance.
  ///
  /// It is not until runRegistry is called, that each target gets its service
  /// metadata, so it's no need to pass the metadata to the target before this.
  let create (conf : LogaryConf) =
    let logger =
      let pn = PointName.parse "Logary.Registry.create"
      conf.runtimeInfo.logger |> Logger.apply (setName pn)

    let init = Target.init conf.runtimeInfo
    let supervisor = Supervisor.create conf.runtimeInfo.logger
    let sched = Scheduling.create ()

    /// Registers the target instance with the supervisor.
    let register (sup : Supervisor.Instance) (inst : TargetInstance) =
      sup.register *<-
        { name     = inst.name
          policy   = Supervisor.Delayed (Duration.FromSeconds 1L)
          job      = inst.server
          shutdown = inst.shutdown }

    /// Builds the new Logary instance
    let build conf =
      { supervisor  = supervisor
        registry    = Impl.create conf supervisor sched
        scheduler   = sched
        runtimeInfo = conf.runtimeInfo
        middleware  = conf.middleware }

    conf.targets
    // init all targets
    |> Map.map (fun name (tconf, _) -> init tconf >>- (fun inst -> tconf, inst))
    // register all targets with the supervisor
    |> Seq.map (fun (KeyValue (pn, jb)) ->
      jb |> Job.bind (fun (tconf, inst) ->
      register supervisor inst >>-.
      (pn, (tconf, Some inst)))
    )
    |> Job.conCollect
    |> Job.map Map.ofSeq
    |> Job.map (fun targets -> { conf with targets = targets })
    |> Job.map build
