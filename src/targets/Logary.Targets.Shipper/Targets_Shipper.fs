module Logary.Targets.Shipper

open System
open Logary.Model
open MBrace.FsPickler
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Configuration
open fszmq
open fszmq.Socket

module Serialisation =

  let private binarySerializer = FsPickler.CreateBinarySerializer ()

  let serialise (msg: #LogaryMessageBase): byte[] =
    binarySerializer.Pickle msg

  let deserialise (datas: byte[]): #LogaryMessageBase =
    binarySerializer.UnPickle datas

type ShipperConf =
  { publishTo: string option
    pushTo: string option }
  static member createPub endpoint =
    { publishTo = Some endpoint
      pushTo = None }
  static member createPush endpoint =
    { publishTo = None
      pushTo = Some endpoint }

let empty =
  { publishTo = None
    pushTo = None }

module internal Impl =

  type State =
    { zmqCtx: Context
      sender: Socket }
    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.sender :> IDisposable).Dispose()

  let createState connectTo createSocket _: State =
    let context = new Context()
    let sender = createSocket context
    Socket.connect sender connectTo
    { zmqCtx = context
      sender = sender }

  let serve (conf: ShipperConf) (api: TargetAPI) =

    let rec loop (state: State): Job<unit> =
      Alt.choose [
        RingBuffer.take api.requests ^=> function
          | Log (msg, ack) ->
            job {
              let msgBase = msg.getAsBase(EventMessage)
              let bytes = Serialisation.serialise msgBase
              do! Job.Scheduler.isolate (fun _ -> bytes |>> state.sender)
              do! ack *<= ()
              return! loop state
            }

          | Flush (ackCh, nack) ->
            ackCh *<= ()
            >>= fun () -> loop state

        api.shutdownCh ^=> fun ack ->
          Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          >>=. IVar.fill ack ()
      ] :> Job<_>

    match conf.publishTo, conf.pushTo with
    | Some endpoint, _ ->
      loop (createState endpoint Context.pub "PUB")
    | _, Some endpoint ->
      loop (createState endpoint Context.push "PUSH")
    | _, _ ->
      failwithf "Failed to start Shipper target, because all of {publishTo, pushTo, sseBinding} had empty values."

/// Create a new Shipper target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.serve conf) name

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
type Builder(conf, callParent: Target.ParentCallback<Builder>) =
  let ret fn =
    ! (callParent <| Builder(fn conf, callParent))

  /// Opens a ZMQ PUB socket
  member x.PublishTo(endpoint: string) =
    ret <| fun conf -> { conf with publishTo = Some endpoint }

  /// Opens a ZMQ PUSH socket to an existing binding
  member x.PushTo(endpoint: string) =
    ret <| fun conf -> { conf with pushTo = Some endpoint }

  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
