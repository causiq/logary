module Logary.Targets.Shipper

open System
open System.Threading
open MBrace.FsPickler
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Targets
open Logary.Internals
open Logary.Configuration
open fszmq
open fszmq.Socket

module Serialisation =
  open System.IO
  open Logary
  open MBrace.FsPickler.Combinators

  (*
  let pValue : Pickler<Value> =
    Pickler.auto

  let pUnits =
    Pickler.sum (fun x bits bytes ss ms scls amps kels mols cdls mul pow div root log ->
      match x with
      | Bits -> bits ()
      | Bytes -> bytes ()
      | Seconds -> ss ()
      | Metres -> ms ()
      | Scalar -> scls ()
      | Amperes -> amps ()
      | Kelvins -> kels ()
      | Moles -> mols ()
      | Candelas -> cdls ()
      | Mul (u1, u2) -> mul (u1, u2)
      | Pow (u1, u2) -> pow (u1, u2)
      | Div (u1, u2) -> div (u1, u2)
      | Root u -> root u
      | Log10 u -> log u)
    ^+ Pickler.variant Bits*)

  let private binarySerializer = FsPickler.CreateBinarySerializer ()

  let serialise (msg : Logary.Message) : byte [] =
    binarySerializer.Pickle msg

  let deserialise (datas : byte [] []) : Logary.Message =
    use ms = new MemoryStream(datas |> Array.fold (fun s t -> s + t.Length) 0)
    for bs in datas do ms.Write(bs, 0, bs.Length)
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    binarySerializer.UnPickle (ms.ToArray())

type ShipperConf =
  | PublishTo of connectTo:string
  | PushTo of connectTo:string
  | Unconfigured

let empty =
  Unconfigured

module internal Impl =

  type State =
    { zmqCtx : Context
      sender : Socket }
    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.sender :> IDisposable).Dispose()

  let createState connectTo createSocket mode : State =
    let context = new Context()
    let sender = createSocket context
    Socket.connect sender connectTo
    //Socket.bind sender connectTo
    { zmqCtx = context
      sender = sender }

  let serve (conf : ShipperConf)
            (ri : RuntimeInfo)
            (requests : RingBuffer<_>)
            (shutdown : Ch<_>) =

    let rec init = function
      | Unconfigured ->
        failwith "Rutta.Shipper should not start in Unconfigured"

      | PublishTo connectTo ->
        createState connectTo Context.pub "PUB"
        |> loop

      | PushTo connectTo ->
        createState connectTo Context.push "PUSH"
        |> loop

    and loop (state : State) : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack -> job {
          do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          return! ack *<= ()
        }

        RingBuffer.take requests ^=> function
          | Log (msg, ack) ->
            job {              
              let bytes = Serialisation.serialise msg
              do! Job.Scheduler.isolate (fun _ -> bytes |>> state.sender)
              do! ack *<= ()
              return! loop state
            }

          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! loop state
            }
      ] :> Job<_>

    init conf

/// Create a new Shipper target
let create conf = TargetUtils.stdNamedTarget (Impl.serve conf)

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.PublishTo(connectTo : string) =
    ! (callParent <| Builder(PublishTo connectTo, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
