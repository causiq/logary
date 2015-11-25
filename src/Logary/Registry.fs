/// The registry is the composition root of Logary
module Logary.Registry

open System.Threading

open Hopac
open Hopac.Infixes

open NodaTime

open Logary.Internals
open Logary.Internals.Scheduling
open Logary.Target
open Logary.HealthCheck

/// Given the registry actor, and a name for the logger, get the logger from the registry.
let getLogger (registry : RegistryInstance) name = job {
    let! resultCh = Ch.create ()
    do! Ch.give registry.reqCh (GetLogger (name, resultCh))
    return! Ch.take resultCh
  }

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
    interface Named with
      member x.Name = name
    interface FlyweightLogger with
      member x.Configured lm =
        lock locker (fun () ->
          logManager := Some lm
          logger := Some (Job.Global.run (getLogger lm.registry name)))
    interface Logger with
      member x.LogVerbose fLine = (!logger) |> Option.iter (fun logger -> logger.LogVerbose fLine)
      member x.LogDebug fLine = (!logger) |> Option.iter (fun logger -> logger.LogDebug fLine)
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
  open Logary.Rule
  open Logary.Target
  open Logary.HealthCheck
  open Logary.DataModel
  open Logary.Internals

  /// Given a configuration and name to find all targets for, looks up all targets
  /// from the configuration matching the passed name and create a composite
  /// acceptor/filter (any matching acceptor).
  let getTargets conf name =
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
  let flushPending dur (registry : RegistryInstance) =
    let flushJob = job {
      let! ack = IVar.create ()
      do! Ch.give registry.reqCh (FlushPending (dur, ack))
      return! ack }

    Job.Global.startIgnore flushJob
    flushJob

  /// Shutdown the registry. This will first flush all pending entries for all targets inside the
  /// registry, and then proceed to sending ShutdownTarget to all of them. If this doesn't
  /// complete within the allotted timeout, (200 ms for flush, 200 ms for shutdown), it will
  /// return Nack to the caller (of shutdown).
  let shutdown dur (registry : RegistryInstance) =
    let shutdownJob = job {
      let! ack = IVar.create ()
      do! Ch.give registry.reqCh (ShutdownLogary (dur, ack))
      return! IVar.read ack }

    Job.Global.startIgnore shutdownJob
    shutdownJob

  /// Flush all registry targets, then shut it down -- the registry internals
  /// take care of timeouts, not these methods
  let flushAndShutdown durFlush durShutdown (registry : RegistryInstance) = job {
    let! fs = flushPending durFlush registry
    let! ss = shutdown durShutdown registry
    // TODO: granular Ack handling for each target
    return { flushed = fs
             stopped = ss
             timedOut = Seq.any (function Nack _ -> true | _ -> false) [ fs; ss ] }
    }

  module private Impl =
    open Logary.DataModel

    /// Create a new Logger from the targets passed, with a given name.
    [<CompiledName "FromTargets">]
    let fromTargets name ilogger (targets : (MessageFilter * TargetInstance * LogLevel) list) =
      { name    = name
        targets = targets |> List.map (fun (mf, ti, _) -> mf, ti)
        level   = targets |> List.map (fun (_, _, level) -> level) |> List.min
        ilogger = ilogger }
      :> Logger

    let wasSuccessful = function
      | HopacSuccess Ack      -> 0
      | HopacTimedOut         -> 1
      | HopacSuccess (Nack _) -> 1

    let bisectSuccesses = // see wasSuccessful f-n above
      fun groups -> groups |> Seq.filter (fun (k, _) -> k = 0) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList),
                    groups |> Seq.filter (fun (k, _) -> k = 1) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList)

    let nl = System.Environment.NewLine

    let toTextList s =
      let sb = new Text.StringBuilder()
      for x in s do sb.AppendLine(sprintf " * %A" x) |> ignore
      sb.ToString()

    let pollMetrics (registry: Ch<RegistryMessage>) =
      Job.Global.run <| Ch.give registry PollMetrics

    /// The state for the Registry
    type RegistryState =
      { /// the supervisor, not of the registry actor, but of the targets and probes and health checks.
        supervisor     : unit
        schedules      : (string * Cancellation) list
        pollMetrics    : Cancellation option }

    /// The registry function that works as an actor.
    let rec registry conf sup sched (inbox : Ch<RegistryMessage>) =
      let targets = conf.targets |> LogaryConfLenses.targetActors_.get
      let lgr = conf.metadata.logger
      let regPath = "Logary.Registry.registry"
      let log = Message.Context.serviceSet regPath >> Logger.log lgr

      /// In the shutdown state, the registry doesn't respond to messages, but rather tries to
      /// flush and shut down all targets, doing internal logging as it goes.
      let shutdown state (dur : Duration) (ack : IVar<Acks>) = job {
        Message.info "shutdown metrics polling" |> log

        do! defaultArg (Option.map (Cancellation.cancel) state.pollMetrics) (Job.unit ())

        Message.info "shutdown schedules" |> log
        for (_, x) in state.schedules do do! Cancellation.cancel x

        Message.info "shutdown targets" |> log
        let! allShutdown =
          targets
          |> Seq.pjmap (Target.shutdown >> Job.withTimeout (HopacTimeout (dur.ToTimeSpan ())))

        let stopped, failed =
          allShutdown
          |> Seq.groupBy wasSuccessful
          |> bisectSuccesses

        do! IVar.fill ack <|
          if List.isEmpty failed then
            Message.info "shutdown Ack" |> log
            Ack
          else
            let msg = sprintf "failed target shutdowns:%s%s" nl (toTextList failed)
            Message.errorf "shutdown Nack%s%s" nl msg
            |> Message.field "failed" (Value.fromObject failed)
            |> log
            Nack msg

        Message.info "shutting down immediately" |> log
        return () }

      let rec init state = job {
        let ctss = System.Collections.Generic.List<_>()
        // init sampling of metrics
        for m in conf.metrics do
          let (conf, mi) = m.Value
          match mi with
          | Some mi ->
            let cts = Scheduling.schedule sched Metric.sample mi conf.sampling (Some conf.sampling)
            ctss.Add( m.Key, cts )
          | None -> ()

        // init getting metrics' values
        Message.debugf "will poll metrics every %O" conf.pollPeriod |> log
        let pollCts =
          Scheduling.schedule sched pollMetrics inbox
            (Duration.FromMilliseconds 200L)
            (Some conf.pollPeriod)

        return! running { state with schedules = ctss |> List.ofSeq
                                     pollMetrics = Some pollCts }
        }

      /// In the running state, the registry takes queries for loggers, gauges, etc...
      and running state : Job<unit> = job {
        let! msg = Ch.take inbox
        printfn "%A" msg
        match msg with
        | GetLogger (name, chan) ->
          do! Ch.give chan (name |> getTargets conf |> fromTargets name lgr)
          return! running state

        // CONSIDER: move away from RPC (even though we're just reading memory
        // in this case) towards fully asynchronous messaging and correlations
        // between messages to create conversations
        | PollMetrics ->
          // CONSIDER: a logMaybe function that checks level before creating LogLine
          Message.debug "polling metrics" |> log
          // CONSIDER: can parallelise this if it helps (as they are all disjunct)
          for mtr in conf.metrics do
            let metricInstance = (LogaryConfLenses.metricActor_ mtr.Key).get conf

            // CONSIDER: can memoize get data points of it helps (this one will probably not change very often)
            Message.debugf "get dps from %O" metricInstance |> log
            let! dps = metricInstance |> Metric.getDataPoints

            Message.debug "get logger" |> log
            let mtrLgr  = mtr.Key |> getTargets conf |> fromTargets mtr.Key lgr

            Message.debug "getting value from metrics" |> log
            let! msrs = metricInstance |> Metric.getValue dps

            // CONSIDER: how to log each of these data points/measures? And what path to give them?
            msrs |> List.iter (Logger.``measure`` mtrLgr)

          return! running state

        | FlushPending(dur, ack) ->
          Message.info "flush start" |> log

          let! allFlushed =
            targets
            |> Seq.pjmap (Target.flush >> Job.withTimeout (HopacTimeout (dur.ToTimeSpan ())))

          let flushed, notFlushed =
            allFlushed
            |> Seq.groupBy wasSuccessful
            |> bisectSuccesses

          do! IVar.fill ack <|
            if List.isEmpty notFlushed then
              Message.info "flush Ack" |> log
              Ack
            else
              let msg = sprintf "Failed target flushes:%s%s" nl (toTextList notFlushed)
              Message.errorf "flush Nack - %s" msg |> log
              Nack msg

          return! running state

        | ShutdownLogary(dur, ack) ->
          do! shutdown state dur ack
          return () }

      init { supervisor = sup; schedules = []; pollMetrics = None }

    let create (conf : LogaryConf) (sup : _) (sched : Ch<ScheduleMsg>) : RegistryInstance =
      let regCh = Ch.Now.create ()
      Job.Global.start (registry conf sup sched regCh)
      {reqCh = regCh}

  /// Start a new registry with the given configuration. Will also launch/start
  /// all targets that are to run inside the registry. Returns a newly
  /// configured LogaryInstance.
  ///
  /// It is not until runRegistry is called, that each target gets its service
  /// metadata, so it's no need to pass the metadata to the target before this.
  let create ({ metadata = { logger = lgr } } as conf : LogaryConf) =
    let log = Message.Context.serviceSet "Logary.Registry.runRegistry" >> Logger.log lgr

    let targets = conf.targets |> Map.map (fun _ (tconf, _) -> tconf, Some(Target.init conf.metadata tconf))
    let metrics = conf.metrics |> Map.map (fun _ (mconf, _) -> mconf, Some(Metric.init conf.metadata mconf))
    let conf' = { conf with targets    = targets
                            metrics    = metrics }

    let sched = Scheduling.create ()
    //Message.debugf "scheduler status: %A" sched.Status |> log

    let reg = Impl.create conf' () sched

    { supervisor = Job.unit ()
      registry = reg
      scheduler = sched
      metadata = conf'.metadata}


    (*let strategy = (fun err (supervisor:IActor) (target:IActor) ->
      System.Threading.Thread.Sleep 500 // or we'll crash with out of mem from gc
      Supervisor.Strategy.OneForOne err supervisor target
    )
    let sopts = Supervisor.Options.Create(None, strategy,
                                          Ns.create "supervisor")
    let sup   = Supervisor.spawn sopts |> Supervisor.superviseAll (targetActors_.get targets)
    Message.debugf "supervisor status: %A" sup.Status |> log

    let sched = Scheduling.create ()
    Message.debugf "scheduler status: %A" sched.Status |> log

    let reg = Impl.create conf' sup sched
    Message.debugf "registry status: %A" reg.Status |> log

    { supervisor = sup
      registry   = reg
      scheduler  = sched
      metadata   = conf'.metadata }*)


