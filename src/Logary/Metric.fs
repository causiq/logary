namespace Logary

open Hopac
open Logary.Internals

/// A function, that, given a stream of all events in the system, is allowed
/// to process them and can send new `Message` values to the channel.
/// Should return an Alt so that it can be cancelled. Hoisted into a metric
/// in the end.
type Processing = Processing of (Stream<Message> -> Ch<Message> -> Alt<unit>)

/// The low-water-mark for stream processing nodes (operators)
type LWM = EpochNanoSeconds

/// Values that a node can take as input.
[<RequireQualifiedAccess>]
type NodeInput =
  | M of Message
  /// A low-water-mark signal that can be repeatedly committed to. (TBD: back
  /// with a MVar?)
  ///
  /// invariant: all Messages after a LWM have >= timestamp.
  | LWM of LWM

/// A node in a stream processing engine
type Node =
  /// An alternative that can be repeatedly committed to in order to receive
  /// messages in the node.
  abstract inputs : Alt<NodeInput>
  /// Gives you a way to perform internal logging and communicate with Logary.
  abstract runtimeInfo : RuntimeInfo

/// Logary's way to talk with Metrics.
type MetricAPI =
  inherit Node
  /// The metric can produce values through this function call.
  abstract produce : Message -> Alt<unit> // Stream.Src<Message>
  /// A channel that the metric needs to select on and then ACK once the target
  /// has fully shut down. 
  abstract shutdownCh : Ch<IVar<unit>>

// TODO: a way of composing these metrics via operators to filter them

type MetricConf =
  { name       : string
    bufferSize : uint32
    policy     : Policy
    server     : RuntimeInfo * MetricAPI -> Job<unit> }

  override x.ToString() =
    sprintf "MetricConf(%s)" x.name

module Metric =

  type T =
    private {
      shutdownCh : Ch<IVar<unit>>
      pauseCh : Ch<unit>
      resumeCh : Ch<unit>
      // etc...
    }

  let create (ri : RuntimeInfo) (conf : MetricConf) : Job<T> =
    { shutdownCh = Ch ()
      pauseCh = Ch ()
      resumeCh = Ch ()
    }
    |> Job.result

type Check =
  | Healthy of contents:string
  | Warning of contents:string
  | Critical of contents:string

type HealthCheckAPI =
  inherit Node
  /// Health checks should call this to produce their output as often as they
  /// like.
  abstract produce : Check -> Alt<unit>
  abstract shutdownCh : Ch<IVar<unit>>

type HealthCheckConf =
  { name       : string
    bufferSize : uint32
    policy     : Policy
    server     : RuntimeInfo * HealthCheckAPI -> Job<unit> }

module HealthCheck =

  type T =
    private {
      shutdownCh : Ch<IVar<unit>>
      pauseCh : Ch<unit>
      resumeCh : Ch<unit>
      // etc...
    }

  let create (ri : RuntimeInfo) (conf : HealthCheckConf) : Job<T> =
    { shutdownCh = Ch ()
      pauseCh = Ch ()
      resumeCh = Ch ()
    }
    |> Job.result