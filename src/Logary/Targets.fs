namespace Logary

#nowarn "1104"

/// A module defining the types relevant for targets to implement
/// and methods to interact with those targets.
module Targets =
  open System
  open System.Text.RegularExpressions

  open FSharp.Actor
  open Logary.Metrics

  /// This is the accept filter that is before the log line is passed to the logger
  /// instance.
  type Acceptor = LogLine -> bool

  /// A rule specifies what log lines and metrics a target should accept.
  [<CustomEquality; NoComparison>]
  type Rule =
    /// This is the regular expression that the 'path' must match to be loggable
    { hiera  : Regex
    /// This is the name of the target that this rule applies to
    ; target : string
    /// This is the accept filter that is before the log line is passed to the logger
    /// instance.
    ; accept : Acceptor
    /// This is the level at which the target will accept log lines. It's inclusive, so
    /// anything below won't be accepted.
    ; level  : LogLevel }
    /// Create a new rule with the given hiera, target, accept function and min level
    /// acceptable.
    static member Create(hiera, target, accept : System.Func<_, _>, level : LogLevel) =
      { hiera  = hiera
      ; target = target
      ; accept = fun l -> accept.Invoke l
      ; level  = level }

    override x.GetHashCode () = hash (x.hiera.ToString(), x.target, x.level)

    override x.Equals other =
      match other with
      | null -> false
      | :? Rule as o -> (x :> IEquatable<Rule>).Equals(o)
      | _ -> false

    interface System.IEquatable<Rule> with
      member x.Equals r = r.hiera.ToString() = x.hiera.ToString() && r.target = x.target && r.level = x.level

    override x.ToString() =
      sprintf "Rule { hiera=%O; target=%s; level=%O }" x.hiera x.target x.level

  /// The protocol that a target can speak
  type TargetMessage =
    /// Log this log line!
    | Log            of LogLine
    /// Log this metric!
    | Metric         of Measure
    /// Flush log lines/metrics! Also, reply when you're done flushing
    /// your queue.
    | Flush          of Acks FSharp.Actor.Types.ReplyChannel
    /// Shut down! Also, reply when you're done shutting down!
    | ShutdownTarget of Acks FSharp.Actor.Types.ReplyChannel

  /// A target instance is a spawned actor instance together with
  /// the name of this target instance.
  type TargetInstance =
    /// The backing actor instance of the target.
    { actor  : IActor
    /// The human readable name of the target.
    ; name   : string }

  /// Get the actor from the target instance
  let actor ti = ti.actor

  /// A target configuration is the 'reference' to the to-be-run target
  /// while it is being configured, and before Logary fully starts up.
  type TargetConf =
    { name     : string
    ; initer   : ServiceMetadata -> TargetInstance }
    override x.ToString() = sprintf "{ name = %s } : TargetConf" x.name

  /// 'API helper' method for flowing the target through
  /// a configurator factory that is then used to change the
  /// TargetConf value that is returned.
  let confTarget name (factory : string -> TargetConf) =
    factory name

  /// Validates the target according to its validation rules.
  let validateTarget (conf : TargetConf) = conf

  /// Initialises the target with metadata and a target configuration,
  /// yielding a TargetInstance in return which contains the running target.
  let initTarget metadata conf =
    conf.initer metadata

  /// Send the target a log line, returning the same instance
  /// as was passed in.
  let send msg instance =
    instance.actor <-- Log msg
    instance

  /// Log to the target and just return unit
  let logTarget i logLine = send logLine i |> ignore

  /// Send the metric to the target and return unit
  let metricTarget i msr = i.actor <-- Metric msr

  /// Send a flush RPC to the target and return the async with the ACKs
  let flushTarget i =
    i.actor |> Actor.makeRpc Flush Infinite

  /// Shutdown the target, waiting indefinitely for it to stop
  let shutdownTarget tInst =
    tInst.actor |> Actor.makeRpc ShutdownTarget Infinite

  /// Module with utilities for Targets to use for formatting LogLines.
  /// Currently only wraps a target loop function with a name and spawns a new actor from it.
  module TargetUtils =

    /// Create a new standard named target, with no particular namespace,
    /// given an actor-based loop function and a name for the target.
    let stdNamedTarget loop name =
      { name = name
      ; initer = fun metadata ->
        { actor = Actor.spawn (Actor.Options.Create(sprintf "logaryRoot/%s" name)) (loop metadata)
        ; name  = name } }

  /// A module that contains the required interfaces to do an "object oriented" DSL
  /// per target
  module FactoryApi =

    open System
    open System.Reflection
    open System.Text.RegularExpressions

    type SpecificTargetConf =
      /// Build the target configuration from a name (and previously called
      /// methods on the instance behind the interface).
      abstract Build : string -> TargetConf

    /// You cannot supply your own implementation of this 
    /// interface; its aim is not to provide Liskov substitution,
    /// but rather to guide you to use the API properly/easily.
    type TargetConfBuild<'T when 'T :> SpecificTargetConf> =

      /// Target-specific configuration, varies by T
      abstract member Target : 'T

      /// The minimum level that the target logs with. Inclusive, so it
      /// will configure the rule for the target to log just above this.
      abstract member MinLevel : LogLevel -> TargetConfBuild<'T>

      /// Only log with the target if the source path matches the regex.
      /// You can use (new Regex(".*")) to allow any, or simply avoid calling
      /// this method.
      abstract member SourceMatching : Regex -> TargetConfBuild<'T>

      /// <summary>
      /// Only accept log lines that match the acceptor.
      /// </summary>
      /// <param name="acceptor">
      /// The function to call for every log line, to verify
      /// whether to let it through
      /// </param>
      abstract member AcceptIf : Func<LogLine, bool> -> TargetConfBuild<'T>

    /// All SpecificTargetConf implementors should take this as their single argument
    /// ctor, to go back into the parent context
    type ParentCallback<'T when 'T :> SpecificTargetConf> =
      SpecificTargetConf -> TargetConfBuild<'T> ref
