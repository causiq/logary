/// The registry is the composition root of Logary
module Logary.Registry

open FSharp.Actor

open NodaTime

let private getSome (thing : string * ReplyChannel<_> -> _) registry name =
  registry |> Actor.reqReply (fun ret -> thing (name, ret)) Infinite

/// Given the registry actor, and a name for the logger, get the logger from the registry.
let getLogger =
  getSome GetLogger

/// Given the registry actor, and a name for the gauge, get the gauge from the registry.
let getGauge : IActor -> string -> Async<_> =
  getSome GetGauge

/// Given the registry actor, and a name for the counter, get the counter from the registry.
let getCounter : IActor -> string -> Async<_> =
  getSome GetCounter

// TODO: move things out here as they become 'easy' to use

/// handling flyweights
module internal Logging =
  open System

  open Logary.Targets
  open Logary.Internals
  open Logary.Internals.InternalLogger
  open Logary.Internals.Date

  /// A logger instance that keeps a reference to the actor targets that it
  /// logs to, as well as its name.
  type LoggerInstance =
    { name    : string
    ; targets : (Acceptor * IActor) list
    ; level   : LogLevel }
    with
      interface Named with
        member x.Name = x.name

      interface Logger with
        member x.Log line =
          // TODO: make functional:
          for accept, t in x.targets do
            try
              if accept line then
                t <-- Log line
            with
            | Actor.ActorInvalidStatus msg ->
              err "Logging to %s failed, because of target state. Actor said: '%s'" x.name msg

        member x.Metric m =
          try
            (x.targets |> List.map snd) <-* Metric m
          with
          | Actor.ActorInvalidStatus msg ->
            err "Sending metric to %s failed, because of target state. Actor said: '%s'" x.name msg

        member x.Level =
          x.level

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
      member x.Metric m = (!logger) |> Option.iter (fun logger -> logger.Metric m)
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
  let startFlyweights logaryInstance =
    lock Globals.criticalSection <| fun () ->
        debug "Logging.logaryRun w/ logaryInstance"
        Globals.singleton := Some logaryInstance
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
  open Logary.Metric
  open Logary.Rules
  open Logary.Targets
  open Logary.Internals.InternalLogger

  module internal Seq =
    let all f s = Seq.fold (fun acc t -> acc && f t) true s
    let any f s = Seq.fold (fun acc t -> acc || f t) false s
    let pmap (f : _ -> Async<_>) s = s |> Seq.map f |> Async.Parallel

  /// Given a configuration and name to find all targets for, looks up all targets
  /// from the configuration matching the passed name and create a composite
  /// acceptor/filter (any matching acceptor).
  let getTargets conf name =
    let rules (rules, _, _) = rules
    let combineAccept rules =
      let accps = Seq.map (fun r -> r.accept) rules
      fun l -> Seq.any (fun a -> a l) accps

    conf.rules
    // first, filter by name
    |> matching name

    // map the target conf and target instance
    |> List.map (fun r -> let t, ti = Map.find r.target conf.targets
                          r, t, (Option.get ti))

    // rules applying to the same target are grouped
    |> Seq.groupBy (fun (r, t, ti) -> t.name)

    // combine acceptors with Seq.any/combineAccept
    |> Seq.map (fun (key, ts) -> let _, t, ti = Seq.head ts in
                                 let rs       = Seq.map rules ts
                                 // find the min matching level from all rules for this target
                                 combineAccept rs, t, ti, (rs |> Seq.map (fun r -> r.level) |> Seq.min))

    // targets should be distinctly returned (deduplicated, so that doubly matching
    // rules don't duply log)
    |> Seq.distinctBy (fun (_, t, _, _) -> t.name)

    // project only the acceptor and the target instance
    |> Seq.map (fun (accept, _, ti, level) -> accept, ti, level)

    // back to a list
    |> List.ofSeq
  /// Given the registry actor, and a name for the meter, get the meter from the registry.
  let getMeter : IActor -> string -> Async<_> =
    getSome GetMeter

  /// Given the registry actor, and a name for the histogram, get the histogram from the registry.
  let getHistogram : IActor -> string -> Async<_> =
    getSome GetHistogram

  /// Given the registry actor, and a name for the timer, get the timer from the registry.
  let getTimer : IActor -> string -> Async<_> =
    getSome GetTimer

  /// Given the registry actor, and a name for the health check, get the health check from the registry.
  let getHealthCheck : IActor -> string -> Async<_> =
    getSome GetHealthCheck

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

  let private snd' (kv : System.Collections.Generic.KeyValuePair<_, _>) = kv.Value
  let private actorInstance = snd' >> snd >> Option.get >> Targets.actor

  let private wasSuccessful = function
    | SuccessWith(Ack, _)     -> 0
    | ExperiencedTimeout _    -> 1
    | SuccessWith(Nack(_), _) -> 1
  let private bisectSuccesses = // see wasSuccessful f-n above
    fun groups -> groups |> Seq.filter (fun (k, _) -> k = 0) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList),
                  groups |> Seq.filter (fun (k, _) -> k = 1) |> Seq.map snd |> (fun s -> if Seq.isEmpty s then [] else Seq.exactlyOne s |> Seq.toList)
  let private nl = System.Environment.NewLine
  let private toTextList s =
    let sb = new Text.StringBuilder()
    for x in s do sb.AppendLine(sprintf " * %A" x) |> ignore
    sb.ToString()

  /// Create a new Logger from the targets passed, with a given name.
  [<CompiledName "FromTargets">]
  let fromTargets name (targets : (Acceptor * TargetInstance * LogLevel) list) =
    { name    = name
    ; targets = targets |> List.map (fun (a, ti, _) -> a, (Targets.actor ti))
    ; level   = targets |> List.map (fun (_, _, level) -> level) |> List.min }
    :> Logger

  let rec private registry conf =
    let targetActors = conf.targets |> Seq.map actorInstance
    (fun (inbox : IActor<_>) ->
      /// In the running state, the registry takes queries for loggers, gauges, etc...
      let rec running () = async {
        let! msg, mopt = inbox.Receive()
        match msg with
        | GetLogger (name, chan) ->
          chan.Reply( name |> getTargets conf |> fromTargets name )
          return! running ()
        | GetCounter (name, chan) ->
          chan.Reply( name |> getTargets conf |> Counter.fromTargets name )
          return! running ()
        | GetGauge (name, chan) ->
          chan.Reply( name |> getTargets conf |> Gauge.fromTargets name )
          return! running ()
        | GetHealthCheck (_, _) ->
          info "TODO: %A" msg
          return! running ()
        | GetHistogram (_, _) ->
          info "TODO: %A" msg
          return! running ()
        | GetMeter (_, _) ->
          info "TODO: %A" msg
          return! running ()
        | GetTimer (_, _) ->
          info "TODO: %A" msg
          return! running ()
        | FlushPending(dur, chan) ->
          info "%s" "registry: flush start"
          let! allFlushed =
            targetActors
            // wait for flushes across all targets
            |> Seq.pmap (Actor.makeRpc Flush (Timeout(dur.ToTimeSpan ())))
          let flushed, notFlushed =
            allFlushed
            |> Seq.groupBy wasSuccessful
            |> bisectSuccesses

          chan.Reply <|
            if List.isEmpty notFlushed then
              info "%s" "registry: flush Ack"
              Ack
            else 
              let msg = sprintf "Failed target flushes:%s%s" nl (toTextList notFlushed)
              err "registry: flush Nack - %s" msg
              Nack msg
          return! running ()
        | ShutdownLogary(dur, ackChan) ->
          return! shutdown dur ackChan }

      /// In the shutdown state, the registry doesn't respond to messages, but rather tries to
      /// flush and shut down all targets, doing internal logging as it goes.
      and shutdown dur ackChan = async {
        info "%s" "registry: shutdown start"
        let! allShutdown =
          targetActors
          |> Seq.pmap (Actor.makeRpc ShutdownTarget (Timeout(dur.ToTimeSpan ())))
        let stopped, failed =
          allShutdown
          |> Seq.groupBy wasSuccessful
          |> bisectSuccesses

        ackChan.Reply <|
          if List.isEmpty failed then
            info "%s" "registry: shutdown Ack"
            Ack
          else
            let msg = sprintf "Failed target shutdowns:%s%s" nl (toTextList failed)
            err "registry: shutdown Nack%s%s" nl msg
            Nack msg

        info "%s" "shutting down registry immediately"
        return () }

      running ())

  /// Start a new registry with the given configuration. Will also launch/start all targets
  /// that are to run inside the registry. Returns a newly configured LogaryInstance.
  /// It is not until runRegistry is called, that each target gets its service metadata,
  /// so it's no need to pass the metadata to the target before this.
  let runRegistry (conf : LogaryConf) =
    let makeTargetLive tuple = tuple |> fst |> initTarget conf.metadata
    let targets' =
      conf.targets
      |> Seq.map (fun kv -> kv.Key, (fst kv.Value, Some(makeTargetLive (snd' kv))))
      |> Map.ofSeq
    let conf' = { conf with targets = targets' }
    let targetActors = targets' |> Seq.map actorInstance
    let supervisor =
      Supervisor.spawn <| Supervisor.Options.Create(None, Supervisor.Strategy.OneForOne, Actor.Options.Create("logaryRoot/supervisor"))
      |> Supervisor.superviseAll targetActors

    debug "runRegistry: supervisor status: %A" supervisor.Status

    let logaryInstance =
      { supervisor = supervisor
      ; registry   = Actor.spawn (Actor.Options.Create("logaryRoot/registry")) (registry conf')
      ; metadata   = conf'.metadata }

    debug "runRegistry: registry status: %A" logaryInstance.registry.Status

    logaryInstance
