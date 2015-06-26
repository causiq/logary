module Logary.Targets.Heka

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Net.Security
open FSharp.Actor
open Logary
open Logary.Heka
open Logary.Heka.Client
open Logary.Heka.Messages
open Logary.Target
open Logary.Internals

type Logary.Heka.Messages.Message with
  static member ofLogLine (line : LogLine) =
    Message()
  static member ofMeasure (msr : Measure) =
    Message()

module internal Impl =

  let logFailure (ri : RuntimeInfo) = function
    | HeaderTooLarge err ->
      Logger.warn ri.logger err
    | MessageTooLarge err ->
      Logger.warn ri.logger err

  type State =
    { client : TcpClient
      stream : Stream }

  let loop (conf : HekaConfig) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec initialise () = async {
      let ep, useTLS = conf.endpoint
      let client = new TcpClient(ep)
      client.NoDelay <- true
      let stream =
        if useTLS then
          let validate = new RemoteCertificateValidationCallback(fun _ -> conf.caValidation)
          new SslStream(client.GetStream(), false, validate) :> Stream
        else
          client.GetStream() :> Stream
      return! running { client = client; stream = stream }
      }

    and running state = async {
      let write msg = async {
        match msg |> Encoder.encode conf state.stream with
        | Choice1Of2 run ->
          do! run
        | Choice2Of2 err ->
          logFailure ri err
        return! running state
        }
      let! msg, _ = inbox.Receive()
      match msg with
      | Log l ->
        return! write (Message.ofLogLine l)
      | Measure msr ->
        return! write (Message.ofMeasure msr)
      | Flush ackChan ->
        ackChan.Reply Ack
        return! running state
      | Shutdown ackChan ->
        return! shutdown state ackChan
      }

    and shutdown state ackChan =
      let dispose x = (x :> IDisposable).Dispose()
      async {
        Try.safe "riemann target disposing tcp stream, then client" ri.logger <| fun () ->
          dispose state.stream
          dispose state.client
        ackChan.Reply Ack
      }

    initialise ()

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# Interop: Create a new Noop target
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

/// Use with LogaryFactory.New( s => s.Target<Heka.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  let signConf conf f = // abstract
    match conf.signingConfig with
    | None -> { conf with signingConfig = Some (f (MessageSigningConfig.Empty)) }
    | Some x -> { conf with signingConfig = Some (f x) }

  let buildSignConf conf f = Builder(signConf conf f, callParent) // compose
  let buildSignConf (f : MessageSigningConfig -> _) = buildSignConf conf f // curry

  /// By default = UInt16.MaxValue + 1 = 65536
  member x.MaxRecordSize(bytes : uint32) =
    Builder({ conf with maxMessageSize = bytes }, callParent)

  /// If you want something other than the SHA1-default, call this. Heka supports
  /// MD5HMAC and SHA1HMAC as of approximately v0.10.
  member x.HashFunction(hf : HmacHashFunction) =
    buildSignConf (fun x -> { x with hash = hf })

  /// The Principal Id to authenticate as, and the key to use to sign the
  /// messages (UTF8.getbytes(key) will be used).
  member x.SignWith(signerName : string, signingKey : string) =
    buildSignConf (fun x -> { x with key = signingKey
                                     name = signerName })

  member x.SignatureMajorVer(majVer : uint32) =
    buildSignConf (fun x -> { x with version = majVer })

  member x.Endpoint(ep : IPEndPoint, useTLS : UseTLS) =
    Builder({ conf with endpoint = ep, useTLS }, callParent)

  member x.Done() =
    ! (callParent x)

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(HekaConfig.Empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
