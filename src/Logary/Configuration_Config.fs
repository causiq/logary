/// The conf module handles interaction with the registry by building the
/// required configuration for Logary.
[<AutoOpen>]
module Logary.Configuration.Config

open FSharp.Actor

open System
open System.Runtime.CompilerServices

open NodaTime

open Logary
open Logary.Internals
open Logary.Target
open Logary.Metric
open Logary.Registry
open Logary.Targets

let private shutdownLogger<'a when 'a :> Logger> : 'a -> unit = box >> function
  | :? System.IDisposable as d -> d.Dispose()
  | _ -> ()

/// Create an internal logger from the level and targets given
let createInternalLogger level targets =
  InternalLogger.Create(level, targets)

/// Configure an internal logger (disposing anything already there); make sure
/// the logger you give is ready to use directly. This function is different from
/// `createInternalLogger` in that it doesn't re-initialise the internal logger
/// for you, but leaves it up to you to give a proper logger for internal
/// logging, instead. `withInternalTarget` uses this function itself after
/// initialising empty metadata and a target.
[<CompiledName "WithInternalLogger">]
let withInternalLogger lgr (conf : LogaryConf) =
  shutdownLogger conf.metadata.logger
  { conf with metadata = { conf.metadata with logger = lgr } }

/// Configure internal logging from a targets (these targets will get
/// everything, without the metrics and/or log lines going through Rules).
[<CompiledName "WithInternalTargets">]
let withInternalTargets level tconfs (conf : LogaryConf) =
  let nullMd = { serviceName = conf.metadata.serviceName; logger = NullLogger() }
  let targets = tconfs |> List.map (Target.init nullMd)
  let logger = createInternalLogger level targets
  conf |> withInternalLogger logger

/// Set the internal target for logary
[<CompiledName "WithInternalTarget">]
let withInternalTarget level tconf =
  withInternalTargets level [tconf]

/// Start logary configuration given a name of the service that is being configured.
/// The name of the service is the foundation for a lot of the sorting that goes
/// on with the logs after they have been sent. By default the internal logging
/// level will be Info, but you can use `withInternalTarget` to re-initialise
/// the internal logging if you wish, with a different level.
[<CompiledName "ConfigureLogary">]
let confLogary serviceName =
  { rules      = []
    targets    = Map.empty
    metrics    = Map.empty
    pollPeriod = Duration.FromMilliseconds 500L
    metadata   = { serviceName = serviceName
                   logger      = NullLogger() } }
  |> withInternalTarget Info (Console.create Console.empty "cons")

/// Add a new target to the configuration. You also need to supple a rule for
/// the target.
[<CompiledName "WithTarget">]
let withTarget (t : TargetConf) conf =
  { conf with targets = conf.targets |> Map.add t.name (t, None) }

/// Add a list of targets to the configuration. You also need to supply a rule
/// for each of the target names.
[<CompiledName "WithTargets">]
let withTargets ts (conf : LogaryConf) =
  ts
  |> Seq.map Target.validate
  |> Seq.fold (fun s t -> s |> withTarget t) conf

/// Add a rule to the configuration - adds to existing rules.
[<CompiledName "WithRule">]
let withRule r conf =
  { conf with rules = r :: conf.rules }

/// Specify the rules to use for filtering what should be logged to what
/// targets - appends list to existing rules.
[<CompiledName "WithRules">]
let withRules rs conf =
  { conf with rules = (rs |> List.ofSeq) @ conf.rules }

/// Adds a metric configuration to the configuration to run in the registry.
[<CompiledName "WithMetric">]
let withMetric (m : MetricConf) conf =
  { conf with metrics = conf.metrics |> Map.add m.name (m, None) }

/// Adds a list of metric configurations to the configuration to run in the
/// registry.
[<CompiledName "WithMetrics">]
let withMetrics ms conf =
  ms
  |> Seq.map Metric.validate
  |> Seq.fold (fun s t -> s |> withMetric t) conf

/// Validate the configuration for Logary, throwing a ValidationException if
/// configuration is invalid.
[<CompiledName "ValidateLogary"; Extension>]
let validate ({ targets  = targets
                rules    = rules
                metadata = { logger = lgr }
                metrics  = metrics } as conf) =
  let rtarget r = r.target
  let tnames = targets |> Map.fold (fun acc k _ -> k :: acc) [] |> Set.ofList

  let rules', targets', metrics' =
    rules |> Set.ofList,
    targets |> Map.fold (fun acc _ (target, _) -> target :: acc) [] |> Set.ofList,
    metrics |> Map.fold (fun acc _ (metric, _) -> metric :: acc) [] |> Set.ofList

  let oRules, oTargets, oMetrics =
    let boundTargets =
      rules
      |> Seq.map (rtarget >> (flip Map.tryFind targets))
      |> Seq.filter Option.isSome
      |> Seq.map (Option.get >> fst)
      |> Set.ofSeq
    Set.filter (rtarget >> (flip Set.contains tnames) >> not) rules',
    Set.filter (flip Set.contains boundTargets >> not) targets',
    Set.filter (fun m -> List.isEmpty (Rule.matching m.name rules)) metrics'

  match oRules.Count, oTargets.Count, oMetrics.Count with
  | 0, 0, 0 -> conf
  | _ -> raise (ValidationException("rules do not have matching targets", oRules,
                                    "targets do not have bound rules", oTargets,
                                    "metrics have no matching rule hieras", oMetrics))

/// Start logary with a given configuration
[<CompiledName "RunLogary"; Extension>]
let runLogary conf =
  let instance = Advanced.create conf
  Logging.startFlyweights instance
  instance

/// Shutdown logary, waiting maximum flushDur + shutdownDur.
[<CompiledName "ShutdownLogary">]
let shutdown' (flushDur : Duration) (shutdownDur : Duration)
  ({ registry = reg; metadata = { logger = lgr } } : LogaryInstance)
  = 
  let log = LogLine.setPath "Logary.Configuration.Config.shutdown" >> Logger.log lgr
  async {
  LogLine.info "start shutdown" |> log
  let! res = Advanced.flushAndShutdown flushDur shutdownDur reg
  LogLine.info "stop shutdown" |> log
  Logging.shutdownFlyweights ()
  shutdownLogger lgr
  return res
  }

/// Shutdown logary, waiting maximum 30 seconds, 15s for flush and 15s for
/// shutdown.
[<CompiledName "ShutdownLogary"; Extension>]
let shutdown =
  shutdown' (Duration.FromSeconds 15L) (Duration.FromSeconds 15L)

/// Wrap the LogaryInstance as a LogManager
[<CompiledName "AsLogManager"; Extension>]
let asLogManager (inst : LogaryInstance) =
  let run = Async.RunSynchronously
  { new LogManager with
      member x.RuntimeInfo        = inst.metadata
      member x.GetLogger name     = name |> getLogger inst.registry |> run
      member x.FlushPending dur   = Advanced.flushPending dur inst.registry |> run
      member x.Shutdown fDur sDur = shutdown' fDur sDur inst |> run
      member x.Dispose ()         = shutdown inst |> Async.Ignore |> run
  }

/// Configure Logary completely with the given service name and rules, targets
/// and metrics. This will call the `validate` function too.
[<CompiledName "Configure">]
let configure serviceName targets metrics rules (internalLevel, internalTarget) =
  confLogary serviceName
  |> withTargets (targets |> List.ofSeq)
  |> withRules (rules |> List.ofSeq)
  |> withMetrics (metrics |> List.ofSeq)
  |> withInternalTarget internalLevel internalTarget
  |> validate
  |> runLogary
  |> asLogManager

/// Configure Logary completely with the given service name and a function that
/// configured the configuration. This will call the `validateLogary` function
/// too. The un-primed version of the function `withLogary` doesn't return a
/// `LogManager` but the F#-oriented LogaryInstance.
[<CompiledName "WithLogaryInstance">]
let withLogary serviceName fConf =
  fConf (confLogary serviceName)
  |> validate
  |> runLogary

/// Configure Logary completely with the given service name and a function that
/// configured the configuration. This will call the `validateLogary` function
/// too. The un-primed version of the function `withLogary` doesn't return a
/// `LogManager` but the F#-oriented LogaryInstance.
[<CompiledName "WithLogary">]
let withLogary' serviceName fConf =
  fConf (confLogary serviceName)
  |> validate
  |> runLogary
  |> asLogManager
