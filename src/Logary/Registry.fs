/// The registry is the composition root of Logary
module Logary.Registry

open System.Threading

open FSharp.Actor

open NodaTime

open Logary.Internals
open Logary.Target
open Logary.HealthCheck

/// The messages that can be sent to the registry to interact with it and its
/// running targets.
type RegistryMessage =
  | GetLogger             of string * Logger ReplyChannel
  | PollMetrics
  /// flush all pending messages from the registry to await shutdown
  | FlushPending          of Duration * Acks ReplyChannel
  /// shutdown the registry in full
  | ShutdownLogary        of Duration * Acks ReplyChannel

/// Given the registry actor, and a name for the logger, get the logger from the registry.
let getLogger registry name =
  registry |> Actor.reqReply (fun ret -> GetLogger (name, ret)) Infinite

/// handling flyweights
module internal Logging =
  open System

  open Logary.Target
  open Logary.Internals
  open Logary.Internals.Date

  /// Flyweight logger impl - reconfigures to work when it is configured, and until then
  /// throws away all log lines.
  type FWL(name) =
    let locker = obj()
    let logManager = ref None
    let logger = ref None
    interface FlyweightLogger with
      member x.Configured lm =
        lock locker (fun () ->
          logManager := Some lm
          logger := Some <| Async.RunSynchronously(getLogger lm.registry name))
    interface Logger with
      member x.Name = name
      member x.Log l = (!logger) |> Option.iter (fun logger -> logger.Log l)
      member x.Measure m = (!logger) |> Option.iter (fun logger -> logger.Measure m)
      member x.Level = Verbose

  /// Iterate through all flywieghts and set the current LogManager for them
  let goFish () =
    lock Globals.criticalSection <| fun () ->
      match !Globals.singleton with
      | None -> ()
      | Some lm ->
        !Globals.flyweights |> List.iter (fun f -> f.Configured lm)
        Globals.flyweights := []

  /// Singleton configuration entry point: call from the runLogary code.
  let startFlyweights inst =
    lock Globals.criticalSection <| fun () ->
      Globals.singleton := Some inst
      goFish ()

  /// Singleton configuration exit point: call from shutdownLogary code
  let shutdownFlyweights _ =
    lock Globals.criticalSection <| fun () ->
      Globals.singleton := None
      Globals.flyweights := []

module Advanced =
  open System

  open Logging

  open Logary
  open Logary.Configuration
  open Logary.Configuration.LogaryConfLenses
  open Logary.Measure
  open Logary.Rule
  open Logary.Target
  open Logary.HealthCheck
  open Logary.Internals

  /// Given a configuration and name to find all targets for, looks up all targets
  /// from the configuration matching the passed name and create a composite
  /// acceptor/filter (any matching acceptor).
  let getTargets conf name =
    let rules (rules, _, _) = rules

    let createLineFilter minLvl rules =
      let filters = Seq.map (fun r -> r.lineFilter) rules
      fun (line : LogLine) ->
        line.level >= minLvl
        && filters |> Seq.any (fun filter -> filter line)

    let createMeasureFilter minLvl rules =
      let filters = Seq.map (fun r -> r.measureFilter) rules
      fun (msr : Measure) ->
        msr.m_level >= minLvl
        && filters |> Seq.any (fun filter -> filter msr)

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
        createLineFilter minLvl rs,
        createMeasureFilter minLvl rs,
        t, ti,
        minLvl)

    // targets should be distinctly returned (deduplicated, so that doubly matching
    // rules don't duply log)
    |> Seq.distinctBy (fun (_, _, t, _, _) ->
        t.name)

    // project only the lineFilter, measureFilter and the target instance
    |> Seq.map (fun (lineFilter, measureFilter, _, ti, level) ->
        lineFilter, measureFilter, ti, level)

    // back to a list
    |> List.ofSeq

  /// Flush all pending log lines for all targets. Returns a Nack/Ack structure describing
  /// how that went. E.g. if there's a target is has a backlog, it might not be able to
  /// flush all its pending entries within the allotted timeout (200 ms at the time of writing)
  /// but will then instead return Nack/failed RPC.
  let flushPending dur (registry : IActor) =
    if registry.Status <> ActorStatus.Running then async { return Nack "registry not running" }
    /// Timeout=Infinite below is for THIS call, internally registry has a timeout per target.
    else registry |> Actor.reqReply (fun c -> FlushPending(dur, c)) Infinite

  /// Shutdown the registry. This will first flush all pending entries for all targets inside the
  /// registry, and then proceed to sending ShutdownTarget to all of them. If this doesn't
  /// complete within the allotted timeout, (200 ms for flush, 200 ms for shutdown), it will
  /// return Nack to the caller (of shutdown).
  let shutdown dur (registry : IActor) =
    if registry.Status <> ActorStatus.Running then async { return Nack "registry not running" }
    /// Timeout=Infinite below is for THIS call, internally registry has a timeout per target.
    else registry |> Actor.reqReply (fun c -> ShutdownLogary(dur, c)) Infinite

  /// Flush all registry targets, then shut it down -- the registry internals
  /// take care of timeouts, not these methods
  let flushAndShutdown durFlush durShutdown (registry : IActor) = async {
    if registry.Status <> ActorStatus.Running then
      return { flushed = Nack "registry not running"
               stopped = Nack "registry not running"
               timedOut = false }
    else
      let! fs = flushPending durFlush registry
      let! ss = shutdown durShutdown registry
      // TODO: granular Ack handling for each target
      return { flushed = fs
               stopped = ss
               timedOut = Seq.any (function Nack _ -> true | _ -> false) [ fs; ss ] }
    }

  module private Impl =

    /// Create a new Logger from the targets passed, with a given name.
    [<CompiledName "FromTargets">]
    let fromTargets name ilogger (targets : (LineFilter * MeasureFilter * TargetInstance * LogLevel) list) =
      { name    = name
        targets = targets |> List.map (fun (lf, mf, ti, _) -> lf, mf, ti.actor)
        level   = targets |> List.map (fun (_, _, _, level) -> level) |> List.min
        ilogger = ilogger }
      :> Logger

    let wasSuccessful = function
      | SuccessWith(Ack, _)     -> 0
      | ExperiencedTimeout _    -> 1
      | SuccessWith(Nack(_), _) -> 1

    let bisectSuccesses = // see wasSuccessful f-n above
      fun groups -> groups |> Seq.filter (fun (k, _) -> k = 0) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList),
                    groups |> Seq.filter (fun (k, _) -> k = 1) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList)

    let nl = System.Environment.NewLine

    let toTextList s =
      let sb = new Text.StringBuilder()
      for x in s do sb.AppendLine(sprintf " * %A" x) |> ignore
      sb.ToString()

    let pollMetrics registry =
      registry <-- PollMetrics

    /// The state for the Registry
    type RegistryState =
      { /// the supervisor, not of the registry actor, but of the targets and probes and health checks.
        supervisor     : IActor
        schedules      : (string * CancellationTokenSource) list
        pollMetrics    : CancellationTokenSource option }

    /// The registry function that works as an actor.
    let rec registry conf sup sched (inbox : IActor<_>) =
      let targets = conf.targets |> LogaryConfLenses.targetActors_.get
      let lgr = conf.metadata.logger
      let regPath = "Logary.Registry.registry"
      let log = LogLine.setPath regPath >> Logger.log lgr

      let rec init state = async {
        let ctss = System.Collections.Generic.List<_>()
        // init sampling of metrics
        for m in conf.metrics do
          let (conf, actor) = m.Value
          match actor with
          | Some actor ->
            let! cts = Scheduling.schedule sched Metric.sample actor conf.sampling (Some conf.sampling)
            ctss.Add( m.Key, cts )
          | None -> ()

        // init getting metrics' values
        LogLine.debugf "will poll metrics every %O" conf.pollPeriod |> log
        let! pollCts =
          Scheduling.schedule sched pollMetrics inbox
            (Duration.FromMilliseconds 200L)
            (Some conf.pollPeriod)

        return! running { state with schedules = ctss |> List.ofSeq
                                     pollMetrics = Some pollCts }
        }

      /// In the running state, the registry takes queries for loggers, gauges, etc...
      and running state = async {
        let! msg, _ = inbox.Receive()
        match msg with
        | GetLogger (name, chan) ->
          chan.Reply( name |> getTargets conf |> fromTargets name lgr)
          return! running state

        // CONSIDER: move away from RPC (even though we're just reading memory
        // in this case) towards fully asynchronous messaging and correlations
        // between messages to create conversations
        | PollMetrics ->
          // CONSIDER: a logMaybe function that checks level before creating LogLine
          LogLine.debug "polling metrics" |> log
          // CONSIDER: can parallelise this if it helps (as they are all disjunct)
          for mtr in conf.metrics do
            let mtrActor = (LogaryConfLenses.metricActor_ mtr.Key).get conf

            // CONSIDER: can memoize get data points of it helps (this one will probably not change very often)
            LogLine.debugf "get dps from %O" mtrActor |> log
            let! dps = mtrActor |> Metric.getDataPoints

            LogLine.debug "get logger" |> log
            let mtrLgr  = mtr.Key |> getTargets conf |> fromTargets mtr.Key lgr

            LogLine.debug "getting value from metrics" |> log
            let! msrs = mtrActor |> Metric.getValue dps

            // CONSIDER: how to log each of these data points/measures? And what path to give them?
            msrs |> List.iter (Logger.``measure`` mtrLgr)

          return! running state

        | FlushPending(dur, chan) ->
          LogLine.info "flush start" |> log
          let! allFlushed =
            targets
            // wait for flushes across all targets
            |> Seq.pmap (Actor.makeRpc Flush (Timeout(dur.ToTimeSpan ())))

          let flushed, notFlushed =
            allFlushed
            |> Seq.groupBy wasSuccessful
            |> bisectSuccesses

          chan.Reply <|
            if List.isEmpty notFlushed then
              LogLine.info "flush Ack" |> log
              Ack
            else
              let msg = sprintf "Failed target flushes:%s%s" nl (toTextList notFlushed)
              LogLine.errorf "flush Nack - %s" msg |> log
              Nack msg

          return! running state

        | ShutdownLogary(dur, ackChan) ->
          return! shutdown state dur ackChan }

      /// In the shutdown state, the registry doesn't respond to messages, but rather tries to
      /// flush and shut down all targets, doing internal logging as it goes.
      and shutdown state (dur : Duration) (ackChan : Acks ReplyChannel) = async {
        LogLine.info "shutdown metrics polling" |> log
        state.pollMetrics |> Option.iter (fun x -> x.Dispose())

        LogLine.info "shutdown schedules" |> log
        state.schedules |> List.iter (fun (_, x) -> x.Dispose())

        LogLine.info "shutdown targets" |> log
        let! allShutdown =
          targets
          |> Seq.pmap (Actor.makeRpc Target.Shutdown (Timeout(dur.ToTimeSpan ())))

        let stopped, failed =
          allShutdown
          |> Seq.groupBy wasSuccessful
          |> bisectSuccesses

        ackChan.Reply <|
          if List.isEmpty failed then
            LogLine.info "shutdown Ack" |> log
            Ack
          else
            let msg = sprintf "failed target shutdowns:%s%s" nl (toTextList failed)
            LogLine.errorf "shutdown Nack%s%s" nl msg
            |> LogLine.setData "failed" failed
            |> log
            Nack msg

        LogLine.info "shutting down immediately" |> log
        return () }

      init { supervisor = sup; schedules = []; pollMetrics = None }

    let create (conf : _) (sup : #IActor) (sched : #IActor) =
      Actor.spawn (Ns.create "registry") (registry conf sup sched)

  /// Start a new registry with the given configuration. Will also launch/start
  /// all targets that are to run inside the registry. Returns a newly
  /// configured LogaryInstance.
  ///
  /// It is not until runRegistry is called, that each target gets its service
  /// metadata, so it's no need to pass the metadata to the target before this.
  let create ({ metadata = { logger = lgr } } as conf : LogaryConf) =
    let log = LogLine.setPath "Logary.Registry.runRegistry" >> Logger.log lgr

    let targets = conf.targets |> Map.map (fun _ (tconf, _) -> tconf, Some(Target.init conf.metadata tconf))
    let metrics = conf.metrics |> Map.map (fun _ (mconf, _) -> mconf, Some(Metric.init conf.metadata mconf))
    let conf' = { conf with targets    = targets
                            metrics    = metrics }

    let strategy = (fun err (supervisor:IActor) (target:IActor) ->
      System.Threading.Thread.Sleep 500 // or we'll crash with out of mem from gc
      Supervisor.Strategy.OneForOne err supervisor target
    )
    let sopts = Supervisor.Options.Create(None, strategy,
                                          Ns.create "supervisor")
    let sup   = Supervisor.spawn sopts |> Supervisor.superviseAll (targetActors_.get targets)
    LogLine.debugf "supervisor status: %A" sup.Status |> log

    let sched = Scheduling.create ()
    LogLine.debugf "scheduler status: %A" sched.Status |> log

    let reg = Impl.create conf' sup sched
    LogLine.debugf "registry status: %A" reg.Status |> log

    { supervisor = sup
      registry   = reg
      scheduler  = sched
      metadata   = conf'.metadata }
