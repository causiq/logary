/// A module defining the types relevant for targets to implement
/// and methods to interact with those targets.
module Logary.Target

#nowarn "1104"

open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals

/// The protocol for a targets runtime path (not the shutdown).
type TargetMessage =
  /// Log and send something that can be acked with the message.
  | Log of message:Message * ack:IVar<unit>

  /// Flush messages! Also, reply when you're done flushing your queue.
  | Flush of ackCh:Ch<unit> * nack:Promise<unit>

/// A target instance is a spawned actor instance together with the name of this
/// target instance.
type TargetInstance =
  { server   : (obj -> Job<unit>) -> obj option -> Job<unit>
    requests : RingBuffer<TargetMessage>
    shutdown : Ch<IVar<unit>>
    /// The human readable name of the target.
    name     : PointName }

/// A target configuration is the 'reference' to the to-be-run target while it
/// is being configured, and before Logary fully starts up.
[<CustomEquality; CustomComparison>]
type TargetConf =
  { name   : PointName
    initer : RuntimeInfo -> Job<TargetInstance> }

  override x.ToString() =
    sprintf "TargetConf(name = %O)" x.name

  override x.Equals other =
    match other with
    | :? TargetConf as other ->
      x.name = other.name

    | _ ->
      false

  override x.GetHashCode() =
    hash x.name

  interface System.IEquatable<TargetConf> with
    member x.Equals other =
      x.name = other.name

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

/// Initialises the target with metadata and a target configuration, yielding a
/// TargetInstance in return which contains the running target.
let init metadata (conf : TargetConf) : Job<TargetInstance> =
  conf.initer metadata

/// Send the target a message, returning the same instance as was passed in when
/// the Message was acked.
let log (i : TargetInstance) (msg : Message) : Alt<Promise<unit>> =
  let ack = IVar ()
  (Log (msg, ack) |> RingBuffer.put i.requests)
  ^->. (ack :> Promise<unit>)

/// Send a flush RPC to the target and return the async with the ACKs. This will
/// create an Alt that is composed of a job that blocks on placing the Message
/// in the queue first, and *then* is selective on the ack/nack once the target
/// has picked up the TargetMessage.
let flush i =
  Alt.withNackJob <| fun nack ->
  let ackCh = Ch ()
  (Flush (ackCh, nack)) |> RingBuffer.put i.requests >>-.
  ackCh

/// Shutdown the target. The commit point is that the target accepts the
/// shutdown signal and the promise returned is that shutdown has finished.
let shutdown i : Alt<Promise<_>> =
  let p = IVar ()
  i.shutdown *<- p ^->. (p :> Promise<_>)

/// Module with utilities for Targets to use for formatting LogLines.
/// Currently only wraps a target loop function with a name and spawns a new actor from it.
module TargetUtils =

  /// Create a new standard named target, with no particular namespace,
  /// given a job function and a name for the target.
  let stdNamedTarget (loop : RuntimeInfo -> RingBuffer<_> -> Ch<IVar<unit>> -> Job<unit>) name : TargetConf =
    let name' = PointName.parse name
    { name = name'
      initer = fun metadata -> job {
        let! requests = RingBuffer.create 500u
        let shutdown = Ch ()
        return { server   = fun _ _ -> loop metadata requests shutdown
                 requests = requests
                 shutdown = shutdown
                 name     = name' }
      }
    }

  let willAwareNamedTarget loop name : TargetConf =
    let name' = PointName.parse name
    { name = name'
      initer = fun metadata -> job {
        let! requests = RingBuffer.create 500u
        let shutdown = Ch ()
        return { server   = loop metadata requests shutdown
                 requests = requests
                 shutdown = shutdown
                 name     = name' }
      }
    }

/// A module that contains the required interfaces to do an "object oriented" DSL
/// per target
module FactoryApi =

  open System
  open System.Reflection
  open System.ComponentModel
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

//  [<Interface>]
//  type HideObjectMembers =
//    [<EditorBrowsable(EditorBrowsableState.Never)>]
//    abstract Equals : obj -> bool
//    [<EditorBrowsable(EditorBrowsableState.Never)>]
//    abstract GetHashCode : unit -> int
//    [<EditorBrowsable(EditorBrowsableState.Never)>]
//    abstract GetType : unit -> Type
//    [<EditorBrowsable(EditorBrowsableState.Never)>]
//    abstract ToString : unit -> string

  /// This interface is used to construct a target specific configuration
  /// from the builder.
  [<NoEquality; NoComparison>]
  type SpecificTargetConf =
    /// Build the target configuration from a name (and previously called
    /// methods on the instance behind the interface).
    abstract Build : string -> TargetConf

  /// You cannot supply your own implementation of this interface; its aim is
  /// not to provide Liskov substitution, but rather to guide you to use the
  /// API properly/easily.
  [<NoEquality; NoComparison>]
  type TargetConfBuild<'T when 'T :> SpecificTargetConf> =

    /// Target-specific configuration, varies by T
    abstract Target : 'T

    /// The minimum level that the target logs with. Inclusive, so it
    /// will configure the rule for the target to log just above this.
    abstract MinLevel : LogLevel -> TargetConfBuild<'T>

    /// Only log with the target if the source path matches the regex.
    /// You can use (new Regex(".*")) to allow any, or simply avoid calling
    /// this method.
    abstract SourceMatching : Regex -> TargetConfBuild<'T>

    /// <summary>
    /// Only accept log lines that match the acceptor.
    /// </summary>
    /// <param name="acceptor">
    /// The function to call for every log line, to verify
    /// whether to let it through
    /// </param>
    abstract AcceptIf : Func<Message, bool> -> TargetConfBuild<'T>

  type MetricsConfBuild =
    abstract member Foo : unit -> MetricsConfBuild

  /// All SpecificTargetConf implementors should take this as their single argument
  /// ctor, to go back into the parent context
  type ParentCallback<'T when 'T :> SpecificTargetConf> =
    SpecificTargetConf -> TargetConfBuild<'T> ref
