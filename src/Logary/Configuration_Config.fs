namespace Logary.Configuration

open Logary.Targets

/// Thrown from 'validateLogary' if the configuration is wrong
type ValidationException(msg, invalidRules : Rule list, invalidTargets : TargetConf list) =
  inherit System.Exception(msg)
  /// Gets the invalid rules that failed validation
  member x.InvalidRules   = invalidRules
  /// Gets the invalid tagets that failed validation
  member x.InvalidTargets = invalidTargets

/// The conf module handles interaction with the registry
/// by building the required configuration for Logary.
[<AutoOpen>]
module Config =
  open FSharp.Actor

  open Logary
  open Registry

  open System

  /////////////////
  // Fluent API F#
  /////////////////

  /// Start logary configuration given a name of the service that is being configured.
  /// The name of the service is the foundation for a lot of the sorting that goes
  /// on with the logs after they have been sent.
  let confLogary serviceName =
    { rules = []
    ; targets = Map.empty
    ; metadata = { serviceName = serviceName } }

  /// Add a new target to the configuration
  let addTarget t conf =
    { conf with targets = conf.targets |> Map.add t.name (t, None) }
    
  /// Specify what targets to log with
  let withTargets ts (conf : LogaryConf) =
    ts
    |> List.map Targets.validateTarget
    |> List.fold (fun s t -> s |> addTarget t) conf

  /// Specify the rules to use for filtering
  /// what should be logged to what targets.
  let withRules rs conf =
    { conf with rules = rs }

  /// Add a rule to the configuration
  let addRule r conf =
    { conf with rules = r :: conf.rules }

  open Logary.Internals.InternalLogger

  /// Validate the configuration for Logary.
  let validateLogary conf =
    let targets   = conf.targets |> Map.fold (fun acc k _ -> k :: acc) [] |> Set.ofList
    let ruleTargs = conf.rules |> List.map (fun r -> r.target)
    let invalidRules =
      [ for r in conf.rules do
         if not(targets |> Set.contains r.target) then
           yield r ]
    match invalidRules with
    | [] ->
      info "%s" "validation successful"
      conf
    | rs ->
      let msg = sprintf "validation failed for\n%A" rs
      err "%s" msg
      raise <| ValidationException(msg, invalidRules, [])

  /// Start logary with a given configuration
  let runLogary conf = Registry.runRegistry conf

  /// Shutdown logary
  let shutdownLogary (instance : LogaryInstance) =
    async {
      info "%s" "config: shutdownLogary start"
      let! flushAck    = instance.registry |> Actor.reqReply FlushPending Infinite
      let! shutdownAck = instance.registry |> Actor.reqReply ShutdownLogary Infinite
      Logging.logaryShutdown ()
      info "%s" "config: shutdownLogary done"
      return shutdownAck }

  ///////////////
  // Direct API
  ///////////////

  /// Configure Logary completely with the given service name, targets and rules.
  [<CompiledName "Configure">]
  let configure serviceName targets rules =
    confLogary serviceName
    |> withRules (rules |> List.ofSeq)
    |> withTargets (targets |> List.ofSeq)
    |> validateLogary
    |> runLogary
    :> LogManager

  open Logary.Target

  /// Run with console and debugger targets with sane configurations.
  let goodDefaults name =
    // TODO: should include Riemann health checks
    // TODO: should include a Graphite target
    confLogary name
    |> withTargets
      [ Console.create Console.ConsoleConf.Default "console"
      ; Debugger.create Debugger.DebuggerConf.Default "debugger" ]
    |> withRules
      [ { Rules.forAny "console" with level = Debug }
      ; { Rules.forAny "debugger" with level = Debug } ]

  /// Run with console and debugger targets with sane configurations as well
  /// as a logstash configuration.
  let goodDefaultsAndLogstash name hostname port =
    goodDefaults name
    |> addTarget (Logstash.create (Logstash.LogstashConf.Create(hostname, port)) "logstash")
    |> addRule ({ Rules.forAny "logstash" with level = Debug })

  /// Start logary with sane SOA/service-defaults, remember to call
  /// shutdownLogary at the end of the program. Pass the name of the
  /// service you are configuring.
  let runWithGoodDefaults name =
    goodDefaults name
    |> validateLogary
    |> runLogary

  /// Run with console and debugger targets with sane configurations as well
  /// as a logstash configuration, and start logary.
  let runWithGoodDefaultsAndLogstash name hostname port =
    goodDefaultsAndLogstash name hostname port
    |> validateLogary
    |> runLogary
