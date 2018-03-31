/// A module defining the types relevant for targets to implement
/// and methods to interact with those targets.
namespace Logary

#nowarn "1104"

open System
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals

/// A target configuration is the 'reference' to the to-be-run target while it
/// is being configured, and before Logary fully starts up.
type TargetConf = // formerly TargetUtils
  { name: string
    rules: Rule list
    bufferSize: uint16
    /// Supervision policy. Use `Logary.Internals.Policy` to choose. By default,
    /// uses exponential backup with a maximum delay, retrying forever.
    policy: Policy
    middleware: Middleware list
    server: RuntimeInfo * TargetAPI -> Job<unit> }

  override x.ToString() =
    sprintf "TargetConf(%s)" x.name


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TargetConf =

  let create policy bufferSize server name: TargetConf =
    let will = Will.create ()
    { name = name
      rules = Rule.empty :: []
      bufferSize = bufferSize
      policy = policy
      middleware = []
      server = server will }

  let createSimple server name: TargetConf =
    { name = name
      rules = Rule.empty :: []
      bufferSize = 512us
      policy = Policy.exponentialBackoffForever
      middleware = []
      server = server }

  let bufferSize size conf =
    { conf with bufferSize = size }

  let setRule r conf =
    { conf with rules = r :: [] }

  /// specific rule should comes last, be careful with the add order
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
      name: string
      api: TargetAPI
      middleware: Middleware list
      rules: Rule list
    }
  with
    member x.Name = x.name

  let needSendToTarget (target: T) msg =
    Rule.canPass msg target.rules

  /// Logs the `Message` to all the targets.Target Middleware compose at here
  let logAll (xs: T seq) (msg: Message): Alt<Promise<unit>> =
    let allAppendedAcks = IVar ()

    let appendToBufferConJob =
      xs
      |> Seq.filter (fun t -> needSendToTarget t msg)
      |> Hopac.Extensions.Seq.Con.mapJob (fun target ->
         let ack = IVar ()
         let msg = msg |> Middleware.compose target.middleware
         RingBuffer.put target.api.requests (Log (msg, ack)) ^->. ack)

    let logToAllTargetAlt = Alt.prepareJob <| fun _ ->
      Job.start (appendToBufferConJob >>= fun acks -> IVar.fill allAppendedAcks acks)
      >>-. allAppendedAcks

    logToAllTargetAlt ^-> fun acks -> Job.conIgnore acks |> memo

  /// Send the target a message, returning the same instance as was passed in when
  /// the Message was acked.
  let log (x: T) (msg: Message): Alt<Promise<unit>> =
    logAll [x] msg

  /// Send a flush RPC to the target and return the async with the ACKs. This will
  /// create an Alt that is composed of a job that blocks on placing the Message
  /// in the queue first, and *then* is selective on the ack/nack once the target
  /// has picked up the TargetMessage.
  let flush (x: T) =
    Alt.withNackJob <| fun nack ->
    let ack = IVar ()
    RingBuffer.put x.api.requests (Flush (ack, nack)) >>-.
    ack

  /// Shutdown the target. The commit point is that the target accepts the
  /// shutdown signal and the promise returned is that shutdown has finished.
  let shutdown x: Alt<Promise<_>> =
    let ack = IVar ()
    Ch.give x.api.shutdownCh ack ^->. upcast ack

  let create (ri: RuntimeInfo) (conf: TargetConf): Job<T> =
    let specificName =  sprintf "Logary.Target(%s)" conf.name
    let ri =
      let setName = setName (PointName.parse specificName)
      let setId = setContext "targetId" (Guid.NewGuid())
      let logger = ri.logger |> Logger.apply (setName >> setId)
      ri |> RuntimeInfo.setLogger logger

    let shutdownCh = Ch ()
    RingBuffer.create conf.bufferSize >>= fun requests ->

    let api =
      { new TargetAPI with
          member x.runtimeInfo = ri
          member x.requests = requests
          member x.shutdownCh = shutdownCh
      }
    let t =
      { name        = conf.name
        middleware  = conf.middleware
        rules       = conf.rules
        api         = api }

    let serverJob = conf.server (ri, api)
    Job.supervise api.runtimeInfo.logger conf.policy serverJob
    |> Job.startIgnore
    >>-. t


