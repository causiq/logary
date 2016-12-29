
module internal Logging =

  let send (targets : TargetInstance list) msg : Alt<Promise<unit>> =
    let latch = Latch targets.Length
    let targets = targets |> List.map (fun target -> target, IVar ())
    let traverse =
      targets
      |> List.traverseAltA (fun (target, ack) ->
        Alt.prepareJob <| fun () ->
        Job.start (ack ^=>. Latch.decrement latch) >>-.
        RingBuffer.put target.requests (Log (msg, ack)))
      |> Alt.afterFun (fun _ -> ())

    traverse ^->. memo (Latch.await latch)

  let logWithAck message : _ list -> Alt<Promise<unit>> = function
    | []      ->
      instaPromise
    | targets ->
      message |> send targets

  let inline ensureName (logger : Logger) (msg : Message) =
    match msg.name with
    | PointName [||] ->
      Message.setName logger.name msg
    | _  ->
      msg

  type LoggerInstance =
    { name    : PointName
      targets : (MessageFilter * TargetInstance) list
      level   : LogLevel }

    interface Logger with
      member x.name = x.name

      member x.level : LogLevel =
        x.level

      member x.log logLevel messageFactory =
        if logLevel >= x.level then
          let me : Logger = upcast x
          me.logWithAck logLevel messageFactory // delegate down
          |> Alt.afterFun (fun _ -> ())
        else
          Alt.always()

      member x.logWithAck logLevel messageFactory : Alt<Promise<unit>> =
        if logLevel >= x.level then
          let message = messageFactory logLevel |> ensureName x
          x.targets
          |> List.choose (fun (accept, t) -> if accept message then Some t else None)
          |> logWithAck message
        else
          Promise.instaPromise

  module Advanced =
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

      let getLogger conf name middleware =
        name
        |> getTargets conf
        |> fromTargets name conf.runtimeInfo.logger
        |> Logger.apply middleware

      let compose (logger : Logger) middleware =
        logger.verboseWithBP (
          eventX "Composing {length} middlewares"
          >> setField "length" (List.length middleware))
        >>-. Middleware.compose middleware

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
          do! logger.infoWithBP (eventX "Shutting down polling of the metrics")
          do! state.schedules |> List.traverseJobA (snd >> Cancellation.cancel) |> Job.Ignore

          do! logger.infoWithBP (eventX "Cancelling schedules")
          for (_, x) in state.schedules do
            do! Cancellation.cancel x

          do! logger.infoWithBP (eventX "Shutting down targets")
          let! allShutdown =
            targets
            |> Seq.pjmap (fun t -> Target.shutdown t  ^->. Success Ack
                                  <|> timeOutMillis 200 ^->. TimedOut)

          let stopped, failed =
            allShutdown
            |> Seq.groupBy wasSuccessful
            |> bisectSuccesses

          if List.isEmpty failed then
            do! logger.verboseWithBP (eventX "Shutdown Ack")
          else
            let msg = sprintf "Failed target shutdowns:%s%s" nl (toTextList failed)
            do! logger.errorWithBP (eventX "Shutdown Nack with {message}"
                                >> setField "message" msg
                                >> setField "failed" (Value.ofObject failed))

          do! Ch.give ackCh () <|> nack
          return! logger.verboseWithBP (eventX "Shutting down immediately")
        }

        let rec init state = job {
          let! ctss =
            conf.metrics
            |> Seq.toList
            |> List.traverseJobA (function
              | KeyValue (name, mconf) -> job {
                let! instance = mconf.initialise name
                do! logger.verboseWithBP (eventX "Will poll {name} every {interval}"
                                      >> setField "name" name
                                      >> setFieldFromObject "interval" mconf.tickInterval)

                let! tickCts =
                  Scheduling.schedule Metric.tick instance mconf.tickInterval
                                      (Some mconf.tickInterval)
                                      sched
                do! logger.verboseWithBP (eventX "Getting logger for metric {name}" >> setField "name" name)

                let! middleware = compose logger conf.middleware
                let logger = getLogger conf name middleware

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
            let middleware = extraMiddleware |> Option.fold (fun s t -> t :: s) conf.middleware
            let! middleware = compose logger middleware
            let logger = getLogger conf name middleware

            do! IVar.fill replCh logger
            return! running state

          | FlushPending(ackCh, nack) ->
            do! logger.debugWithBP (eventX "Starting to flush")

            let! allFlushed =
              targets
              |> Seq.pjmap (fun t -> Target.flush t    ^->. Success Ack <|>
                                     timeOutMillis 200 ^->. TimedOut)

            let flushed, notFlushed =
              allFlushed
              |> Seq.groupBy wasSuccessful
              |> bisectSuccesses

            if List.isEmpty notFlushed then
              do! logger.verboseWithBP (eventX "Completed flush")
            else
              let msg = sprintf "Failed target flushes:%s%s" nl (toTextList notFlushed)
              do! logger.errorWithBP (eventX "Flush failed with {nack}." >> setField "nack" msg)

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


    let init = Target.init conf.runtimeInfo
    let supervisor = Supervisor.create logger
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


  module internal Impl =
    open System
    open Logary
    open Logary.Rule
    open Logary.Target
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


    let services =
      let label label =
        Seq.map (fun (KeyValue (k, v)) -> sprintf "Logary.Services.%s(%s)" label k, v)
      Map [
        yield!
          targets
          |> label "Target"
          |> Seq.map (fun (name, x) ->
            Target.toService conf.runtimeInfo.logger name x)
        yield!
          metrics
          |> Seq.map (fun (KeyValue (name, x)) -> Metric.toService conf.runtimeInfo.logger name x)
          |> label "Metric"
        yield!
          hcs
          |> Seq.map (fun (KeyValue (name, x)) -> HealthCheck.toService conf.runtimeInfo.logger name x)
          |> label "HealthCheck"
        yield "Logary.Services.Engine", Engine.toService conf.runtimeInfo.logger engine
        yield "Logary.Services.Globals", globals
      ]

    // TODO: choose over all services' failure
    let initialise =
      let running =
        services
        |> Seq.map (fun (KeyValue (name, svc)) -> Service.start svc)
        |> Job.conCollect
      running >>= Impl.running