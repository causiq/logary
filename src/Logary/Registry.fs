/// The registry is the composition root of Logary
module Logary.Registry

open System.Threading

open Hopac
open Hopac.Infixes
open NodaTime
open Logary.Utils.Aether
open Logary.Internals
open Logary.Internals.Scheduling
open Logary.Target
open Logary.HealthCheck

/// Given the registry actor, and a name for the logger, get the logger from the registry.
let getLogger (registry : RegistryInstance) name : Job<Logger> =
  let resCh = Ch ()
  registry.reqCh *<+ GetLogger (name, resCh) >>=.
  resCh

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
      member x.logVerbose fLine =
        logger (flip Logger.logVerbose fLine) ()

      member x.logVerboseWithAck fLine =
        logger (flip Logger.logVerboseWithAck fLine) (Promise.Now.withValue ())

      member x.logDebug fLine =
        logger (flip Logger.logDebug fLine) ()

      member x.logDebugWithAck fLine =
        logger (flip Logger.logDebugWithAck fLine) (Promise.Now.withValue ())

      member x.log l =
        logger (flip Logger.log l) ()

      member x.logWithAck l =
        logger (flip Logger.logWithAck l) (Promise.Now.withValue ())

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
  let flushPending (registry : RegistryInstance) : Alt<unit> = Alt.withNackJob <| fun nack -> job {
    let! flushCh = Ch.create ()
    do! registry.reqCh *<+ (FlushPending (flushCh, nack))
    return flushCh
  }

  /// Shutdown the registry. This will first flush all pending entries for all targets inside the
  /// registry, and then proceed to sending ShutdownTarget to all of them. If this doesn't
  /// complete within the allotted timeout, (200 ms for flush, 200 ms for shutdown), it will
  /// return Nack to the caller (of shutdown).
  let shutdown (registry : RegistryInstance) : Alt<unit> = Alt.withNackJob <| fun nack -> job {
    let! shutdownCh = Ch.create ()
    do! registry.reqCh *<+ (ShutdownLogary (shutdownCh, nack))
    return shutdownCh
  }

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
    [<CompiledName "FromTargets">]
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

    let pollMetrics (registry: Ch<RegistryMessage>) =
      Job.start (Ch.send registry PollMetrics)

    /// The state for the Registry
    type RegistryState =
      { /// the supervisor, not of the registry actor, but of the targets and probes and health checks.
        supervisor  : Supervisor.Instance
        schedules   : (PointName * Cancellation) list
        pollMetrics : Cancellation option }

    /// The registry function that works as a supervisor and runtime state holder
    /// for Logary.
    let rec registry conf sup sched (inbox : Ch<RegistryMessage>) : Job<_> =
      let targets = conf.targets |> Seq.map (fun kv -> kv.Value |> snd |> Option.get)
      let regPath = PointName ["Logary"; "Registry"; "registry" ]
      let log = Message.setName regPath >> Logger.log conf.runtimeInfo.logger

      /// In the shutdown state, the registry doesn't respond to messages, but rather tries to
      /// flush and shut down all targets, doing internal logging as it goes.
      let shutdown state (ackCh : Ch<unit>) (nack : Promise<unit>) = job {
        do! Message.info "shutdown metrics polling" |> log
        do! defaultArg (Option.map Cancellation.cancel state.pollMetrics) (Job.unit ())

        do! Message.info "cancel schedules" |> log
        for (_, x) in state.schedules do
          do! Cancellation.cancel x

        do! Message.info "shutdown targets" |> log
        let! allShutdown =
          targets
          |> Seq.pjmap (fun t -> Target.shutdown t  ^->. Success Ack
                                 <|> timeOutMillis 200 ^->. TimedOut)

        let stopped, failed =
          allShutdown
          |> Seq.groupBy wasSuccessful
          |> bisectSuccesses

        if List.isEmpty failed then
          do! Message.info "shutdown Ack" |> log
        else
          let msg = sprintf "failed target shutdowns:%s%s" nl (toTextList failed)
          do! Message.errorf "shutdown Nack%s%s" nl msg
              |> Message.field "failed" (Value.fromObject failed)
              |> log

        do! Ch.give ackCh () <|> nack
        return! Message.info "shutting down immediately" |> log
      }

      let rec init state = job {
        let! ctss =
          // init sampling of metrics
          conf.metrics
          |> Seq.map (fun (KeyValue (key, (conf, mi))) ->
            key,
            mi |> Option.map (fun mi ->
              let initialDelay, interval = conf.sampling, Some conf.sampling
              Scheduling.schedule sched Metric.sample mi initialDelay interval
            )
          )
          |> Seq.filter (snd >> Option.isSome)
          |> Seq.map (fun (a, b) -> a, Option.get b)
          |> List.ofSeq
          |> List.traverseJobA (fun (key, cancellation : Job<Cancellation>) ->
            cancellation >>- (fun ct -> key, ct))

        // init getting metrics' values
        do! Message.debugf "will poll metrics every %O" conf.pollPeriod |> log
        let! pollCts =
          Scheduling.schedule sched pollMetrics inbox
                              (Duration.FromMilliseconds 200L)
                              (Some conf.pollPeriod)

        return! running { state with schedules = List.ofSeq ctss
                                     pollMetrics = Some pollCts }
        }

      /// In the running state, the registry takes queries for loggers, gauges, etc...
      and running state : Job<_> = job {
        let! msg = Ch.take inbox
        match msg with
        | GetLogger (name, chan) ->
          let logger =
            name
            |> getTargets conf
            |> fromTargets name conf.runtimeInfo.logger

          do! Ch.give chan logger
          return! running state

        // CONSIDER: move away from RPC (even though we're just reading memory
        // in this case) towards fully asynchronous messaging and correlations
        // between messages to create conversations
        | PollMetrics ->
          // CONSIDER: a logMaybe function that checks level before creating LogLine
          do! Message.debug "polling metrics" |> log
          // CONSIDER: can parallelise this if it helps (as they are all disjunct)
          for mtr in conf.metrics do
            let getMetricInst name = fun x -> x.metrics |> Map.find name |> (snd >> Option.get)
            let metricInstance = getMetricInst mtr.Key conf

            // CONSIDER: can memoize get data points of it helps (this one will probably not change very often)
            do! Message.debugf "get dps from %O" metricInstance |> log
            let! dps = metricInstance |> Metric.getDataPoints

            do! Message.debug "get logger" |> log
            let mtrLgr = mtr.Key |> getTargets conf |> fromTargets mtr.Key conf.runtimeInfo.logger

            do! Message.debug "getting value from metrics" |> log
            let! msrs = metricInstance |> Metric.getValue dps

            // CONSIDER: how to log each of these data points/measures? And what path to give them?
            for m in msrs do
              do! Logger.log mtrLgr m

          return! running state

        | FlushPending(ackCh, nack) ->
          do! Message.info "flush start" |> log

          let! allFlushed =
            targets
            |> Seq.pjmap (fun t -> Target.flush t    ^->. Success Ack <|>
                                   timeOutMillis 200 ^->. TimedOut)

          let flushed, notFlushed =
            allFlushed
            |> Seq.groupBy wasSuccessful
            |> bisectSuccesses

          if List.isEmpty notFlushed then
            do! Message.info "flush Ack" |> log
          else
            let msg = sprintf "Failed target flushes:%s%s" nl (toTextList notFlushed)
            do! Message.errorf "flush Nack - %s" msg |> log

          do! Ch.give ackCh () <|> nack

          return! running state

        | ShutdownLogary(ackCh, nack) ->
          return! shutdown state ackCh nack }

      init { supervisor = sup; schedules = []; pollMetrics = None }

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
      Message.setName (PointName [ "Logary"; "Registry"; "create" ])
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

    let metrics =
      conf.metrics
      |> Map.map (fun _ (mconf, _) ->
        mconf,
        // TODO: consider this blocking call?
        Some (Metric.init conf.runtimeInfo mconf |> run))

    let confNext = { conf with targets = targets
                               metrics = metrics }

    let listen =
      targets
      |> Seq.map (fun kv -> kv.Value |> snd |> Option.get)
      |> List.ofSeq
      |> List.traverseJobA (fun ti ->
        let (PointName name) = ti.name
        let namedJob = NamedJob.create name ti.server
        Supervisor.register sup namedJob :> Job<_>)
      >>-. ()
      |> run // TODO: consider this blocking call?

    let sched = Scheduling.create ()
    let reg = Impl.create confNext sup sched
    { supervisor  = sup
      registry    = reg
      scheduler   = sched
      runtimeInfo = confNext.runtimeInfo }