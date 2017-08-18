/// A module defining the types relevant for targets to implement
/// and methods to interact with those targets.
namespace Logary

#nowarn "1104"

open System
open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.Message
open Logary.Internals

/// A target configuration is the 'reference' to the to-be-run target while it
/// is being configured, and before Logary fully starts up.
type TargetConf = // formerly TargetUtils
  { name       : string
    rules      : Rule list
    bufferSize : uint32
    /// Supervision policy
    policy     : Policy
    middleware : Middleware list
    server     : RuntimeInfo * TargetAPI -> Job<unit> }

  override x.ToString() =
    sprintf "TargetConf(%s)" x.name

  interface Service with
    member x.name = x.name


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TargetConf =

  let create policy bufferSize server name : TargetConf =
    let will = Will.create ()
    { name       = name
      rules      = Rule.empty :: []
      bufferSize = bufferSize
      policy     = policy
      middleware = []
      server     = server will }

  let createSimple server name : TargetConf =
    { name       = name
      rules      = Rule.empty :: []
      bufferSize = 500u
      policy     = Policy.exponentialBackoffSix
      middleware = []
      server     = server }

  let bufferSize size conf =
    { conf with bufferSize = size }

  let setRule r conf =
    { conf with rules = r :: [] }

  let addRule r conf =
    { conf with rules = r :: conf.rules }

  let policy policy conf =
    { conf with policy = policy }

  let middleware mid conf =
    { conf with middleware = mid :: conf.middleware }

module Target =

  /// A target instance is a target loop job that can be re-executed on failure,
  /// and a normal path of communication; the `requests` `RingBuffer` as well as
  /// and out-of-band-method of shutting down the target; the `shutdownCh`.
  type T =
    private {
      server     : Job<unit>
      requests   : RingBuffer<TargetMessage>
      shutdownCh : Ch<IVar<unit>>
    }

  /// Send the target a message, returning the same instance as was passed in when
  /// the Message was acked.
  let log (x : T) (msg : Message) : Alt<Promise<unit>> =
    let ack = IVar ()
    Log (msg, ack)
    |> RingBuffer.put x.requests
    |> Alt.afterFun (fun () -> ack :> Promise<unit>)

  /// Logs the `Message` to all the targets.
  let logAll (xs : T seq) (msg : Message) : Alt<Promise<unit>> =
    // NOTE: it would probably be better to create a nack that cancels
    // all outstanding requests to log (but lets others through)
    let targets = List.ofSeq xs 
    let latch = Latch targets.Length

    let traverse =
      targets
      |> List.map (fun target -> target, IVar ())
      |> List.traverseAltA (fun (target, ack) ->
        Alt.prepareJob <| fun () ->
        Job.start (ack ^=>. Latch.decrement latch) >>-.
        RingBuffer.put target.requests (Log (msg, ack)))
      |> Alt.afterFun (fun _ -> ())

    traverse ^->. memo (Latch.await latch)

  /// Send a flush RPC to the target and return the async with the ACKs. This will
  /// create an Alt that is composed of a job that blocks on placing the Message
  /// in the queue first, and *then* is selective on the ack/nack once the target
  /// has picked up the TargetMessage.
  let flush (x : T) =
    Alt.withNackJob <| fun nack ->
    let ack = IVar ()
    RingBuffer.put x.requests (Flush (ack, nack)) >>-.
    ack

  /// Shutdown the target. The commit point is that the target accepts the
  /// shutdown signal and the promise returned is that shutdown has finished.
  let shutdown x : Alt<Promise<_>> =
    let ack = IVar ()
    Ch.give x.shutdownCh ack ^->. upcast ack

  let create (ri : RuntimeInfo) (conf : TargetConf) : Job<T> =
    let ri =
      let setName = setSimpleName (sprintf "Logary.Target(%s)" conf.name)
      let setId = setContext "targetId" (Guid.NewGuid())
      let logger = ri.logger |> Logger.apply (setName >> setId)
      ri |> RuntimeInfo.setLogger logger

    let shutdownCh = Ch ()
    RingBuffer.create conf.bufferSize >>- fun requests ->

    let api =
      { new TargetAPI with
          member x.runtimeInfo = ri
          member x.requests = requests
          member x.shutdownCh = shutdownCh
      }

    { server     = conf.server (ri, api)
      requests   = requests
      shutdownCh = shutdownCh }

  //let toService (ilogger : Logger) (x : T) : string -> Job<InitialisingService<_>> =
  //  fun name ->
  //    Service.createSimple ilogger name x.shutdownCh x.server