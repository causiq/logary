/// A module defining the types relevant for targets to implement
/// and methods to interact with those targets.
namespace Logary

#nowarn "1104"

open System
open Hopac
open Hopac.Extensions
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals

/// A target configuration is the 'reference' to the to-be-run target while it
/// is being configured, and before Logary fully starts up.
type TargetConf =
  { name: string
    rules: Rule list
    bufferSize: uint16
    /// Supervision policy. Use `Logary.Internals.Policy` to choose. By default,
    /// uses exponential backup with a maximum delay, retrying forever.
    policy: Policy
    middleware: Middleware list
    server: TargetAPI -> Job<unit> }

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
  [<Struct>]
  type T =
    val name: string
    val api: TargetAPI
    val transform: Message -> Message
    val rules: Rule list
    internal new (name, api, middleware, rules) =
      { name = name
        api = api
        transform = Middleware.compose middleware
        rules = rules }
    member x.accepts message =
      Rule.accepts message x.rules

  /// If the alternative is committed to, the RingBuffer WILL enqueue the message. The tryLog operation is done as
  /// `Ch.give x.tryPut m`.
  let tryLog (msg: Message) (x: T): LogResult =
    let msg = x.transform msg
    if not (x.accepts msg) then LogResult.rejected
    else
      let ack = IVar ()
      let targetMsg = Log (msg, ack)
      // This returns immediately if the buffer is full; and the RingBuffer's server is
      // always available to notify the caller that the buffer is full.
      RingBuffer.tryPut x.api.requests targetMsg ^-> function
        | true ->
          Result.Ok (upcast ack)
        | false ->
          Result.Error (BufferFull x.name)

  /// Logs the `Message` to all the targets.
  let tryLogAll (targets: T[]) (msg: Message): Alt<Result<Promise<unit>, LogError>[]> =
    Alt.withNackFun <| fun nack ->
    let putAllPromises = IVar ()
    let tryPutAll =
      let abortPut = nack ^->. Result.Error Rejected
      Seq.Con.mapJob (fun t -> tryLog msg t <|> abortPut) targets
      >>- fun futures -> futures.ToArray()
      >>= IVar.fill putAllPromises
    putAllPromises

  let private folder t s =
    match s, t with
    | Ok _, Result.Error err ->
      Result.Error [ err ]
    | Ok oks, Ok ok ->
      Ok (ok :: oks)
    | Result.Error errors, Result.Error err ->
      Result.Error (err :: errors)
    | Result.Error errors, Ok _ ->
      Result.Error errors

  let tryLogAllReduce targets msg: LogResult =
    tryLogAll targets msg ^=> function
      | [||] ->
        LogResult.success :> Job<_>
      | ps ->
        match (ps, Ok []) ||> Array.foldBack folder with
        | Ok promises ->
          let latch = Latch promises.Length
          let dec = Latch.decrement latch
          Job.start (promises |> Seq.Con.iterJob (fun p -> p ^=>. dec))
          >>-. Ok (memo (Latch.await latch))
        | Result.Error [] ->
          // should not happen, error list always non empty:
          Job.result (Result.Error Rejected)
        | Result.Error (e :: _) ->
          Job.result (Result.Error e)

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
  let shutdown (x: T): Alt<Promise<_>> =
    let ack = IVar ()
    Ch.give x.api.shutdownCh ack ^->. upcast ack

  let create (ri: RuntimeInfo) (conf: TargetConf): Job<T> =
    let specificName = sprintf "Logary.Target(%s)" conf.name
    let ri =
      let setName = setName (PointName.parse specificName)
      let setId = setContext "targetId" (Guid.NewGuid())
      let logger = ri.logger |> Logger.apply (setName >> setId)
      ri |> RuntimeInfo.setLogger logger

    let shutdownCh = Ch ()
    RingBuffer.create conf.bufferSize >>= fun requests ->

    let api =
      { new TargetAPI with
          member x.runtime = ri
          member x.requests = requests
          member x.shutdownCh = shutdownCh
      }

    let t = T(conf.name, api, conf.middleware, conf.rules)
    let serverJob = conf.server api
    Job.supervise api.runtime.logger conf.policy serverJob
    |> Job.startIgnore
    >>-. t
