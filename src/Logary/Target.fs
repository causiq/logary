/// A module defining the types relevant for targets to implement
/// and methods to interact with those targets.
module Logary.Target

#nowarn "1104"

open System
open System.Text.RegularExpressions

open Hopac

open Logary.DataModel
open Logary.Internals

/// The protocol that a target can speak
type TargetMessage =
  /// Flush messages! Also, reply when you're done flushing
  /// your queue.
  | Flush of IVar<Acks>
  /// Shut down! Also, reply when you're done shutting down!
  | Shutdown of IVar<Acks>

/// A target instance is a spawned actor instance together with
/// the name of this target instance.
type TargetInstance =
  { logMb : BoundedMb<Message>
    reqCh : Ch<TargetMessage>
    /// The human readable name of the target.
    name : string }

/// A target configuration is the 'reference' to the to-be-run target while it
/// is being configured, and before Logary fully starts up.
[<CustomEquality; CustomComparison>]
type TargetConf =
  { name   : string
    initer : RuntimeInfo -> TargetInstance }
  override x.ToString() =
    sprintf "TargetConf(name = %s)" x.name

  override x.Equals other =
      match other with
      | :? TargetConf as other ->
        x.name.Equals(other.name, StringComparison.InvariantCulture)
      | _ -> false

  override x.GetHashCode() =
    hash x.name

  interface System.IEquatable<TargetConf> with
    member x.Equals other =
      x.name.Equals (other.name, StringComparison.InvariantCulture)

  interface System.IComparable with
    member x.CompareTo yobj =
      match yobj with
      | :? TargetConf as y -> compare x.name y.name
      | _ -> invalidArg "yobj" "cannot compare values of different types"

/// 'API helper' method for flowing the target through
/// a configurator factory that is then used to change the
/// TargetConf value that is returned.
let confTarget name (factory : string -> TargetConf) =
  factory name

/// Validates the target according to its validation rules.
let validate (conf : TargetConf) = conf

/// Initialises the target with metadata and a target configuration,
/// yielding a TargetInstance in return which contains the running target.
let init metadata (conf : TargetConf) =
  conf.initer metadata

/// Send the target a message, returning the same instance
/// as was passed in.
let send msg (instance : TargetInstance) =
  Job.Global.start (BoundedMb.put instance.logMb msg)
  instance

/// Send a flush RPC to the target and return the async with the ACKs
let flush i = job {
  let! ack = IVar.create ()
  do! Ch.give i.reqCh (Flush ack)
  return! ack
}

/// Shutdown the target, waiting indefinitely for it to stop
let shutdown i = job {
  let! ack = IVar.create ()
  do! Ch.give i.reqCh (Shutdown ack)
  return! IVar.read ack
}

/// Module with utilities for Targets to use for formatting LogLines.
/// Currently only wraps a target loop function with a name and spawns a new actor from it.
module TargetUtils =

  // TODO: Complete guesswork.
  let private defaultMbBuffer = 128

  // TODO: Make mb buffer size configurable
  /// Create a new standard named target, with no particular namespace,
  /// given a job function and a name for the target.
  let stdNamedTarget loop name : TargetConf =
    { name = name
      initer = fun metadata ->
        Job.Global.start (loop metadata)
        { logMb = BoundedMb.create defaultMbBuffer |> Job.Global.run
          reqCh = Ch.Now.create () 
          name = name } }

// TODO: Make the new Hopac-based system support this
/// A module that contains the required interfaces to do an "object oriented" DSL
/// per target
module FactoryApi =

  open System
  open System.Reflection
  open System.Text.RegularExpressions

  /// This is useful to implement if you want add-on assemblies to be able to
  /// extend your builder. Then you just implement an interface that also
  /// inherits this interface and makes your extensions to the target configuration
  /// be extension methods in the extending assembly, calling this method to
  /// read the conf, and then returning the modified configuration to the
  /// builder by other means (e.g. by calling a method on the 'intermediate')
  /// interface (that is in the core target builder configuration). Since the
  /// builder knows its callback, it can implement this 'intermediate' interface
  /// with a method taking the new configuration (that was read from and mutated
  /// from here).
  type ConfigReader<'a> =
    /// an accessor for the internal state; don't use unless you know what you're
    /// doing! Used by the migrations to get the current configuration. Allows you
    /// to modify or use the configuration.
    abstract ReadConf : unit -> 'a

  /// This interface is used to construct a target specific configuration
  /// from the builder.
  type SpecificTargetConf =
    /// Build the target configuration from a name (and previously called
    /// methods on the instance behind the interface).
    abstract Build : string -> TargetConf

  /// You cannot supply your own implementation of this interface; its aim is
  /// not to provide Liskov substitution, but rather to guide you to use the
  /// API properly/easily.
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
    abstract member AcceptIf : Func<Message, bool> -> TargetConfBuild<'T>

  /// All SpecificTargetConf implementors should take this as their single argument
  /// ctor, to go back into the parent context
  type ParentCallback<'T when 'T :> SpecificTargetConf> =
    SpecificTargetConf -> TargetConfBuild<'T> ref
