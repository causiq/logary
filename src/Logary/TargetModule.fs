namespace Logary

module Target =
  open System
  open Hopac
  open Hopac.Infixes
  open Hopac.Extensions
  open Logary.Message
  open Logary.Internals

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

  /// WARNING: this may cause you headaches; are you sure you don't want to use tryLog?
  ///
  /// Returns an Alt that commits on the Targets' buffer accepting the message.
  let log (x: T) (msg: Message): LogResult =
    let msg = x.transform msg
    if not (x.accepts msg) then LogResult.rejected
    else
      let ack = IVar ()
      let targetMsg = Log (msg, ack)
      RingBuffer.put x.api.requests targetMsg ^->.
      Ok (upcast ack)

  /// If the alternative is committed to, the RingBuffer WILL enqueue the message. The tryLog operation is done as
  /// `Ch.give x.tryPut m`.
  ///
  /// Returns an Alt that commits on the Targets' buffer accepting the message OR the RingBuffer telling the caller that
  /// it was full, and returning an Error(BufferFull [targetName]) LogResult.
  let tryLog (x: T) (msg: Message): LogResult =
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

  /// returns: an Alt committed on:
  ///   buffer took message <- dangerousBlockOnBuffer
  ///   buffer maybe took message <- not dangerousBlockOnBuffer
  let private logAll_ dangerousBlockOnBuffer targets msg =
    Alt.withNackJob <| fun nack ->
    //printfn "tryLogAll: creating alt with nack for %i targets" targets.Length
    let putAllPromises = IVar ()
    let abortPut = nack ^->. Result.Error Rejected
    let createPutJob t =
      if dangerousBlockOnBuffer then
        log t msg <|> abortPut
      else
        tryLog t msg <|> abortPut
    let tryPutAll =
      Seq.Con.mapJob createPutJob targets
      >>- fun results ->
            //printfn "tryLogAll: array-ing %i results" results.Count
            results.ToArray()
      >>= IVar.fill putAllPromises
    Job.start tryPutAll >>-. putAllPromises

  /// WARNING: this may cause you headaches; are you sure you don't want to use tryLogAll?
  ///
  /// Returns an Alt that commits on ALL N Targets' buffers accepting the message. Even if the Alt was not committed to,
  /// one or more targets (fewer than N) may have accepted the message.
  let logAll (targets: T[]) (msg: Message): Alt<Result<Promise<unit>, LogError>[]> =
    logAll_ (* DO BLOCK — WARNING *) true targets msg

  /// Tries to log the `Message` to all the targets.
  let tryLogAll (targets: T[]) (msg: Message): Alt<Result<Promise<unit>, LogError>[]> =
    logAll_ (* do not block *) false targets msg

  let tryLogAllReduce targets msg: LogResult =
    tryLogAll targets msg ^=> function
      | [||] ->
        //printfn "tryLogAllReduce: Success from empty targets array"
        LogResult.success :> Job<_>
      | ps ->
        match Result.sequence ps with
        | Ok promises ->
          let latch = Latch promises.Length
          let dec = Latch.decrement latch
          Job.start (promises |> Seq.Con.iterJob (fun p -> p ^=>. dec))
          >>-. Ok (memo (Latch.await latch))
        | Result.Error [] ->
          // should not happen, error list always non empty:
          //printfn "tryLogAllReduce: Failure from empty error array"
          Job.result (Result.Error Rejected)
        | Result.Error (e :: _) ->
          //printfn "tryLogAllReduce: Failure from NON-empty error array %A" e
          Job.result (Result.Error e)

  // TODO: logAllReduce

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
