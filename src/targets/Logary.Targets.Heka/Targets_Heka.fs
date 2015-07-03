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

type HekaConfig = Logary.Heka.HekaConfig

type Logary.LogLevel with
  static member toSeverity = function
    | Verbose -> 7
    | Debug   -> 7
    | Info    -> 6
    | Warn    -> 4
    | Error   -> 3
    | Fatal   -> 2

type Logary.Heka.Messages.Message with
  static member ofLogLine (line : LogLine) =
    let msg = Message(logger = line.path)
    msg.severity <- Nullable (line.level |> LogLevel.toSeverity)
    msg.payload <- line.message
    msg

  static member ofMeasure (msr : Measure) =
    let msg = Message(logger = DP.joined msr.m_path)
    msg.severity <- Nullable (msr.m_level |> LogLevel.toSeverity)
    msg

/// The message type most often used in filters inside Heka (see e.g. the getting-
/// started guide).
let MessageType = "heka.logary"

module internal Impl =

  let logFailure (ri : RuntimeInfo) = function
    | HeaderTooLarge err ->
      Logger.warn ri.logger err
    | MessageTooLarge err ->
      Logger.warn ri.logger err

  type State =
    { client   : TcpClient
      stream   : Stream
      hostname : string }

  let loop (conf : HekaConfig) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let debug = LogLine.debug >> LogLine.setPath "Logary.Targets.Heka" >> Logger.log ri.logger

    let rec initialise () = async {
      debug "initialising heka target"

      let ep, useTLS = conf.endpoint
      let client = new TcpClient()
      client.NoDelay <- true
      do! client.ConnectAsync(ep.Address, ep.Port)

      let stream =
        if useTLS then
          let validate = new RemoteCertificateValidationCallback(fun _ -> conf.caValidation)
          let sslStream = new SslStream(client.GetStream(), false, validate)
          //sslStream.AuthenticateAsClient(ep.Address) // or ep.Hostname
          sslStream :> Stream
        else
          client.GetStream() :> Stream

      debug "initialise: tcp stream open"

      let hostname = Dns.GetHostName()
      return! running { client = client; stream = stream; hostname = hostname }
      }

    and running state = async {
      let write (msg : Message) = async {
        msg.uuid        <- Guid.NewGuid().ToByteArray()
        msg.hostname    <- state.hostname
        msg.env_version <- Lib.LogaryVersion
        msg.``type``    <- MessageType
        msg.addField (Field("service", Nullable ValueType.STRING, null,
                            [ ri.serviceName ]))

        match msg |> Encoder.encode conf state.stream with
        | Choice1Of2 run ->
          debug "running: writing to heka"
          try
            do! run
            debug "running: wrote to heka"
          with e -> LogLine.error "error writing to heka"
                    |> LogLine.setPath "Logary.Targets.Heka"
                    |> LogLine.setExn e |> Logger.log ri.logger
        | Choice2Of2 err ->
          logFailure ri err
        debug "running: recursing"
        return! running state
        }

      debug "running: receiving"

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
        Try.safe "heka target disposing tcp stream, then client" ri.logger <| fun () ->
          dispose state.stream
          dispose state.client
        ackChan.Reply Ack
      }

    initialise ()

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# Interop: Create a new Noop target
[<CompiledName "Create">]
let createInterop (conf, name) =
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
