/// A module defining the types relevant for targets to implement
/// and methods to interact with those targets.
namespace Logary

#nowarn "1104"

open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.Message
open Logary.Internals
open Logary.Supervisor

/// The protocol for a targets runtime path (not the shutdown).
type TargetMessage =
  /// Log and send something that can be acked with the message.
  | Log of message:Message * ack:IVar<unit>
  /// Flush messages! Also, reply when you're done flushing your queue.
  | Flush of ackCh:IVar<unit> * nack:Promise<unit>

/// Logary's way to talk with Targets. Targets are responsible for selecting
/// over these channels in order to handle shutdown and messages.
type TargetAPI =
  /// Gives you a way to perform internal logging and communicate with Logary.
  abstract runtimeInfo : RuntimeInfo
  /// A ring buffer that gives a Message to log and an ACK-IVar to signal after
  /// logging the message.
  abstract requests : RingBuffer<TargetMessage>
  /// A channel that the target needs to select on and then ACK once the target
  /// has fully shut down. 
  abstract shutdownCh : Ch<IVar<unit>>

/// A target configuration is the 'reference' to the to-be-run target while it
/// is being configured, and before Logary fully starts up.
[<CustomEquality; CustomComparison>]
type TargetConf =
  { name       : string
    rules      : Rule list
    bufferSize : uint32
    /// Supervision policy
    policy     : Policy
    server     : RuntimeInfo * TargetAPI -> Job<unit> }

  override x.ToString() =
    sprintf "TargetConf(name = %s)" x.name

module Target =

  /// A target instance is a spawned actor instance together with the name of this
  /// target instance.
  type T<'will> =
    private {
      server   : SupervisedJob<unit>
      requests : RingBuffer<TargetMessage>
      shutdown : Ch<IVar<unit>>
      will     : Will<'will>
    }

  /// Send the target a message, returning the same instance as was passed in when
  /// the Message was acked.
  let log (i : T<'will>) (msg : Message) : Alt<Promise<unit>> =
    let ack = IVar ()
    Log (msg, ack)
    |> RingBuffer.put i.requests
    |> Alt.afterFun (fun _ -> ack :> Promise<unit>)

  /// Send a flush RPC to the target and return the async with the ACKs. This will
  /// create an Alt that is composed of a job that blocks on placing the Message
  /// in the queue first, and *then* is selective on the ack/nack once the target
  /// has picked up the TargetMessage.
  let flush (t : T<_>) =
    Alt.withNackJob <| fun nack ->
    let ackCh = IVar ()
    Flush (ackCh, nack)
    |> RingBuffer.put t.requests
    |> Alt.afterFun (fun _ -> ackCh)

  /// Shutdown the target. The commit point is that the target accepts the
  /// shutdown signal and the promise returned is that shutdown has finished.
  let shutdown i : Alt<Promise<_>> =
    let p = IVar ()
    i.shutdown *<- p ^->. (p :> Promise<_>)

  let create internalLogger name (conf : TargetConf) : T<'will> =
    let ilogger =
      internalLogger |> Logger.apply (setSimpleName (sprintf "Logary.Target(%s)" name))
    { server = 
      requests = RingBuffer.create conf.bufferSize
      shutdown = Ch ()
      will     = Will.create () }

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

  /// Creates a new target instance that is capable of handling callback upon
  /// previous crash, with the saved state (that it itself needs to pass into)
  /// Logary before rethrowing its exception.
  let willAwareNamedTarget loop name : TargetConf =
    let name' = PointName.parse name
    { name = name'
      initer = fun runtimeInfo -> job {
        let! requests = RingBuffer.create 500u
        let shutdown = Ch ()
        return { server   = loop runtimeInfo requests shutdown
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
    // TODO: nest one more level here...
    abstract member AddMetric : Duration * string * Func<PointName, Job<Logary.Metric.Metric>> -> MetricsConfBuild

  /// All SpecificTargetConf implementors should take this as their single argument
  /// ctor, to go back into the parent context
  type ParentCallback<'T when 'T :> SpecificTargetConf> =
    SpecificTargetConf -> TargetConfBuild<'T> ref
