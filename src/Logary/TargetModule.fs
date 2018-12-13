namespace Logary
open NodaTime

module Target =
  open System
  open Hopac
  open Hopac.Infixes
  open Hopac.Extensions
  open Logary.Message
  open Logary.Internals
  open NodaTime

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


  /// <summary> Returns an Alt that commits on the Targets' buffer accepting the message. </summary>
  /// 
  /// <param name="putBufferTimeOut">
  /// means it will try to wait this duration for putting the message onto buffer,otherwise return timeout error, and cancel putting message.
  /// if putBufferTimeOut <=  Duration.Zero , it will try to detect whether the buffer is full and return immediately
  /// </param> 
  /// 
  let log (x: T) (msg: Message) (putBufferTimeOut: Duration): LogResult =
    let msg = x.transform msg
    if not (x.accepts msg) then LogResult.notAcceptByTarget x.name
    else
      let ack = IVar ()
      let targetMsg = Log (msg, ack)
      if putBufferTimeOut <= Duration.Zero then
        RingBuffer.tryPut x.api.requests targetMsg ^-> function
        | true ->
          Result.Ok (upcast ack)
        | false ->
          LogError.targetBufferFull x.name
      else
        RingBuffer.put x.api.requests targetMsg ^->. Result.Ok (upcast ack)
        <|>
        timeOut (putBufferTimeOut.ToTimeSpan()) ^-> fun _ -> LogError.timeOutToPutBuffer x.name putBufferTimeOut.TotalSeconds

  let private logAll_ putBufferTimeOut targets msg =
    Alt.withNackJob <| fun nack ->
    //printfn "tryLogAll: creating alt with nack for %i targets" targets.Length
    let putAllPromises = IVar ()
    let abortPut = nack ^->. LogError.clientAbortLogging
    let createPutJob t =
      log t msg putBufferTimeOut <|> abortPut
    let tryPutAll =
      Seq.Con.mapJob createPutJob targets
      >>- fun results ->
            //printfn "tryLogAll: array-ing %i results" results.Count
            results.ToArray()
      >>= IVar.fill putAllPromises
    Job.start tryPutAll >>-. putAllPromises

  let private logAllReduceHandler (results: ProcessResult[]) =
    match results with
    | [||] ->
      //printfn "tryLogAllReduce: Success from empty targets array"
      LogResult.success :> Job<_>
    | ps ->
      match ProcessResult.reduce ps with
      | Ok promises ->
        let latch = Latch promises.Length
        let dec = Latch.decrement latch
        Job.start (promises |> Seq.Con.iterJob (fun p -> p ^=>. dec))
        >>-. Ok (memo (Latch.await latch))
      | Result.Error msg ->
        //printfn "tryLogAllReduce: Failure from NON-empty error array %A" e
        Job.result (Result.Error msg)

  /// Returns an Alt that commits on ALL N Targets' buffers accepting the message. 
  /// Even if the Alt was not committed to, one or more targets (fewer than N) may have accepted the message.
  let tryLogAllReduce targets msg: LogResult =
    logAll_ Duration.Zero targets msg ^=> logAllReduceHandler

  /// Returns an Alt that commits on ALL N Targets' buffers accepting the message. 
  /// Even if the Alt was not committed to, one or more targets (fewer than N) may have accepted the message.
  let logAllReduce targets msg putBufferTimeOut : LogResult =
    logAll_ putBufferTimeOut targets msg ^=> logAllReduceHandler

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
