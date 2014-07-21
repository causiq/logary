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
open Logary.Registry
open Logary.Targets

/// Start logary configuration given a name of the service that is being configured.
/// The name of the service is the foundation for a lot of the sorting that goes
/// on with the logs after they have been sent.
[<CompiledName "ConfigureLogary">]
let confLogary serviceName =
  let nullMd = { serviceName = serviceName; logger = NullLogger() }
  let console = Console.create (Console.ConsoleConf.Default) "cons" |> init nullMd
  { rules    = []
    targets  = Map.empty
    metadata =
      { serviceName = serviceName
        logger      = InternalLogger.Create(Verbose, [ console ]) } }

/// Add a new target to the configuration. You also need to supple a rule for
/// the target.
[<CompiledName "WithTarget">]
let withTarget t conf =
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

/// Adds a list of metric configurations to the configuration to run in the
/// registry.
[<CompiledName "WithMetrics">]
let withMetrics ms conf =
  conf

/// Validate the configuration for Logary, throwing a ValidationException if
/// configuration is invalid.
[<CompiledName "ValidateLogary"; Extension>]
let validateLogary ({ targets = targets; rules = rules; metadata = { logger = lgr } } as conf) =
  let targets = targets |> Map.fold (fun acc k _ -> k :: acc) [] |> Set.ofList
  let invalidRules =
    [ for r in rules do
        if not(targets |> Set.contains r.target) then
          yield r ]
  match invalidRules with
  | [] ->
    LogLine.info "validation successful" |> Logger.log lgr
    conf
  | rs ->
    let msg = sprintf "validation failed for\n%A" rs
    LogLine.error msg |> Logger.log lgr
    raise <| ValidationException(msg, invalidRules, [])

/// Start logary with a given configuration
[<CompiledName "RunLogary"; Extension>]
let runLogary conf =
  let instance = Registry.Advanced.runRegistry conf
  Logging.startFlyweights instance
  instance

/// Shutdown logary, waiting maximum flushDur + shutdownDur.
[<CompiledName "ShutdownLogary">]
let shutdown' (flushDur : Duration) (shutdownDur : Duration)
  ({ registry = reg; metadata = { logger = lgr } } as inst : LogaryInstance)
  = async {
  LogLine.info "config: shutdownLogary start" |> Logger.log lgr
  let! res = Advanced.flushAndShutdown flushDur shutdownDur reg
  LogLine.info "config: shutdownLogary done" |> Logger.log lgr
  Logging.shutdownFlyweights ()
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

/// Configure Logary completely with the given service name and rules and
/// targets. This will call the validateLogary function too.
[<CompiledName "Configure">]
let configure serviceName targets rules =
  confLogary serviceName
  |> withTargets (targets |> List.ofSeq)
  |> withRules (rules |> List.ofSeq)
  |> validateLogary
  |> runLogary
  |> asLogManager

/// Configure Logary completely with the given service name and a function that
/// configured the configuration. This will call the `validateLogary` function
/// too. The un-primed version of the function `withLogary` doesn't return a
/// `LogManager` but the F#-oriented LogaryInstance.
[<CompiledName "WithLogaryInstance">]
let withLogary serviceName fConf =
  fConf (confLogary serviceName)
  |> validateLogary
  |> runLogary

/// Configure Logary completely with the given service name and a function that
/// configured the configuration. This will call the `validateLogary` function
/// too. The un-primed version of the function `withLogary` doesn't return a
/// `LogManager` but the F#-oriented LogaryInstance.
[<CompiledName "WithLogary">]
let withLogary' serviceName fConf =
  fConf (confLogary serviceName)
  |> validateLogary
  |> runLogary
  |> asLogManager
