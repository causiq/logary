namespace Logary

open NodaTime
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Internals

module Target =
  /// A target instance is a target loop job that can be re-executed on failure,
  /// and a normal path of communication; the `requests` `RingBuffer` as well as
  /// and out-of-band-method of shutting down the target; the `shutdownCh`.
  [<Struct>]
  type T =
    val name: string
    val api: TargetAPI
    val transform: Model.LogaryMessageBase -> Model.LogaryMessageBase
    val rules: Rule list
    internal new (name, api, middleware, rules) =
      { name = name
        api = api
        transform = Middleware.compose middleware
        rules = rules }

    member x.accepts message =
      Rule.accepts message x.rules


  /// Returns an Alt that commits on the Targets' buffer accepting the message.
  ///
  /// - `putBufferTimeOut` means it will try to wait this duration for putting the message onto buffer,otherwise return timeout error, and cancel putting message.
  ///   if `putBufferTimeOut <=  Duration.Zero` , it will try to detect whether the buffer is full and return immediately
  let log (putBufferTimeOut: Duration) (x: T) (msg: Model.LogaryMessageBase): LogResult =
    let msg = x.transform msg
    if not (x.accepts msg) then LogResult.targetBufferFull x.name
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
        let putA = RingBuffer.put x.api.requests targetMsg ^->. Result.Ok (ack :> Promise<_>)
        let timeA = timeOut (putBufferTimeOut.toTimeSpanSafe()) ^-> fun _ -> LogError.targetBufferFull x.name
        putA <|> timeA

  let tryLog (x: T) (msg: Model.LogaryMessageBase): LogResult = log Duration.Zero x msg

  let private logAll_ putBufferTimeOut targets msg =
    Alt.withNackJob <| fun nack ->
    //printfn "tryLogAll: creating alt with nack for %i targets" targets.Length
    let putAllPromises = IVar ()
    let abortPut = nack ^-> LogError.clientAbortLogging
    let createPutJob t =
      log putBufferTimeOut t msg <|> abortPut
    let tryPutAll =
      Seq.Con.mapJob createPutJob targets
      >>= IVar.fill putAllPromises
    Job.start tryPutAll >>-. putAllPromises


  /// Returns an Alt that commits on ALL N Targets' buffers accepting the message.
  /// Even if the Alt was not committed to, one or more targets (fewer than N) may have accepted the message.
  let logAllReduce putBufferTimeOut targets msg: LogResult =
    logAll_ putBufferTimeOut targets msg ^-> ProcessResult.reduce

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
    let specificName = PointName.parse (sprintf "Logary.Target(%s)" conf.name)
    let targetId = Value.Str (Id.create().toBase64String())
    let ri =
      let logger =
        ri.logger |> Logger.apply (fun m ->
            m.name <- specificName
            m.setContext("targetId", targetId))
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
