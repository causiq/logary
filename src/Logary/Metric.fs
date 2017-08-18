namespace Logary

open Hopac
open Logary.Internals
open Hopac.Stream

/// The low-water-mark for stream processing nodes (operators)
type LowWaterMark = EpochNanoSeconds

/// Values that a node can take as input.
[<RequireQualifiedAccess>]
type NodeInput =
  /// The node received an input.
  | Message of message:Message
  /// A low-water-mark signal that can be repeatedly committed to. (TBD: back
  /// with a MVar?)
  ///
  /// invariant: all Messages after a LWM have >= timestamp.
  | EventTime of lowWaterMark:LowWaterMark

/// A function, that, given a stream of all events in the system, is allowed
/// to process them and can send new `Message` values to the channel.
/// Should return an Alt so that it can be cancelled. Hoisted into a metric
/// in the end.
type Processing =
  Processing of ((* all things *) Message -> (* publish *) Ch<Message> -> Alt<unit>)
with
  static member empty =
    Processing (fun _ _ -> Alt.always ())

type internal Iterator =
  abstract tryAdvance: unit -> bool
  abstract dispose: unit -> unit

type internal Iterable =
  abstract bulk: unit -> unit
  abstract iterator: Iterator

[<AllowNullLiteral>]
type internal FlowCTS() =
  let mutable isCancelled = false
  let mutable linked : FlowCTS list = []
  member x.createLinked() = let sc = FlowCTS() in linked <- sc :: linked; sc
  member x.cancel() = isCancelled <- true
  member x.cancelled = isCancelled

type internal Iterator<'t> =
  { cont: 't -> unit
    completed: FlowCTS }

/// Represents the current executing contex
type internal Context<'t> =
  { /// The composed continutation
    cont: 't -> unit
    /// The completed continuation
    complete: unit -> unit
    cts: FlowCTS }

type internal Collector<'t, 'r> =
  /// Gets an iterator over the elements.
  abstract iterator: unit -> Iterator<'t>
  /// The result of the collector.
  abstract result: 'r

type Flow<'t> =
  internal { run: Context<'t> -> Iterable }
  member inline internal x.runBulk ctx = (x.run ctx).bulk()

[<RequireQualifiedAccessAttribute>]
module Flow =

  let inline internal Flow f = { run = f }

  module Internals = 
    // Public permanent entrypoint to implement inlined versions of map, filter, choose etc.
    // 'f' is called with one argument before iteration.
    let mapCont f flow =
      Flow (fun { complete = complete; cont = iterf; cts = cts } ->
        flow.run { complete = complete; 
                   cont = f iterf; 
                   cts = cts })

  /// <summary>The empty flow.</summary>
  /// <returns>An empty flow.</returns>
  let empty<'T> : Flow<'T> =
    Flow (fun { complete = complete; cont = iterf; cts = cts } ->
      { new Iterable with
          member x.bulk() = ()
          member x.iterator =
            { new Iterator with 
              member x.tryAdvance() = false
              member x.dispose () = ()} })

  /// <summary>Creates a singleton flow.</summary>
  /// <param name="source">The singleton flow element</param>
  /// <returns>A stream of just the given element</returns>
  let singleton (source: 'T) : Flow<'T> =
    Flow (fun { complete = complete; cont = iterf; cts = cts }->
      let pulled = ref false
      { new Iterable with 
          member __.bulk() = iterf source; complete ()
          member x.iterator =
            { new Iterator with
                member x.tryAdvance() =
                  if !pulled then false
                  else
                    iterf source
                    pulled := true
                    complete ()
                    true
                member x.dispose() = ()} })
                

  /// <summary>Wraps array as a flow.</summary>
  /// <param name="source">The input array.</param>
  /// <returns>The result flow.</returns>
  let ofArray (source : 'T []) : Flow<'T> =
    Flow (fun { complete = complete; cont = iterf; cts = cts } ->
      let bulk () =
          if not <| obj.ReferenceEquals(cts, null) then
              let mutable i = 0
              while i < source.Length && not cts.cancelled do
                  iterf source.[i]
                  i <- i + 1
          else
              for i = 0 to source.Length - 1 do
                  iterf source.[i]
          complete ()

      let iterator() =
        let i = ref 0
        let cts = if cts = null then FlowCTS() else cts 
        { new Iterator with 
            member __.tryAdvance() = 
              if cts.cancelled then
                false
              else if !i < source.Length then
                iterf source.[!i] 
                if cts.cancelled then
                  complete ()
                incr i
                true
              else
                cts.cancel()
                complete ()
                true
            member __.dispose() = () }
      { new Iterable with 
          member __.bulk() = bulk()
          member __.iterator = iterator() })

  // Used to indicate that we don't want a closure to be curried
  let inline internal nocurry() = Unchecked.defaultof<unit>

  /// <summary>Transforms each element of the input stream.</summary>
  /// <param name="f">A function to transform items from the input stream.</param>
  /// <param name="stream">The input stream.</param>
  /// <returns>The result stream.</returns>
  let inline map (f : 'T -> 'R) (flow : Flow<'T>) : Flow<'R> =
    flow |> Internals.mapCont (fun iterf -> nocurry(); fun value -> iterf (f value))

  let inline mapi (f : int -> 'T -> 'R) (flow : Flow<'T>) : Flow<'R> =
    flow |> Internals.mapCont (fun iterf -> let counter = ref -1 in (fun value -> incr counter; iterf (f !counter value))) 

  /// <summary>Filters the elements of the input filter.</summary>
  /// <param name="predicate">A function to test each source element for a condition.</param>
  /// <param name="filter">The input filter.</param>
  /// <returns>The result filter.</returns>
  let inline filter (predicate : 'T -> bool) (filter : Flow<'T>) : Flow<'T> =
    filter |> Internals.mapCont (fun iterf -> nocurry(); fun value -> if predicate value then iterf value)

  /// <summary>Transforms each element of the input flow to a new flow and flattens its elements.</summary>
  /// <param name="f">A function to transform items from the input flow.</param>
  /// <param name="flow">The input flow.</param>
  /// <returns>The result flow.</returns>
  let flatMap (f : 'T -> Flow<'R>) (flow : Flow<'T>) : Flow<'R> =
    Flow (fun { complete = complete; cont = iterf; cts = cts } ->
      flow.run
        { complete = complete
          cont = (fun value ->
            let flow' = f value
            let cts' = if cts = null then cts else cts.createLinked()
            flow'.runBulk { complete = (fun () -> ()); cont = iterf; cts = cts' } )
          cts = cts })


  // http://riemann.io/concepts.html
  let tag tag = filter (Message.hasTag tag)
  let name name = filter (fun m -> m.name = name)
  let service svc = filter (fun m -> m.context |> HashMap.tryFind "service" = Some svc)
  let level lvl = filter (fun m -> m.level = lvl)

  let aand pred1 pred2 = fun m -> pred1 m && pred2 m

  let where pred = filter pred

  // target
  // let target name (flow: Flow<'R>) =
  //   Flow (fun ctx ->
  //     flow.runBulk { complete = (fun () -> ())
  //                    cont = f
  //                    cts = null } 
  //   )


/// A node in a stream processing engine
type NodeAPI =
  /// An alternative that can be repeatedly committed to in order to receive
  /// messages in the node.
  abstract inputs : Alt<NodeInput>
  /// The metric can produce values through this function call.
  abstract produce : Message -> Alt<unit> // Stream.Src<Message>
  /// Gives you a way to perform internal logging and communicate with Logary.
  abstract runtimeInfo : RuntimeInfo

/// Logary's way to talk with Metrics.
type MetricAPI =
  inherit NodeAPI
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
  inherit NodeAPI
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