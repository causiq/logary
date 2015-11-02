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
open Logary.Formatting

type HekaConfig = Logary.Heka.HekaConfig

type Logary.LogLevel with
  static member toSeverity = function
    | Verbose -> 7
    | Debug   -> 7
    | Info    -> 6
    | Warn    -> 4
    | Error   -> 3
    | Fatal   -> 2

type Logary.Heka.Messages.Field with
  static member ofField (name: string, (value, units)) =
    let unitsS = defaultArg (Option.map (DataModel.Units.symbol) units) ""

    // TODO: This should be reformatted if possible to avoid ridiculous amounts of copypaste
    match value with
    | DataModel.String s when s.Length > 0 ->
      Some <| Messages.Field(name, Nullable ValueType.STRING, unitsS, [s])
    // TODO/CONSIDER: Heka doesn't support arbitrary precision integers, so we use a string instead.
    | DataModel.BigInt bi ->
      Some <| Messages.Field(name, Nullable ValueType.STRING, unitsS, [bi.ToString ()])
    | DataModel.Float f ->
      Some <| Messages.Field(name, Nullable ValueType.DOUBLE, unitsS, [float f])
    | DataModel.Bool b ->
      Some <| Messages.Field(name, Nullable ValueType.BOOL, unitsS, [b])
    | DataModel.Binary (bytes, mime) when bytes.Length > 0 ->
      Some <| Messages.Field(name, Nullable ValueType.BYTES, mime, [bytes])
    // TODO/CONSIDER: We assume here that arrays are homogenous, even though object model permits heterogenous arrays.
    | DataModel.Array arr when not arr.IsEmpty ->
      // TODO: Figure out a way to implement arrays neatly without copypaste
      match arr.Head with
      | DataModel.String _ ->
        Some <| Messages.Field(name, Nullable ValueType.STRING, unitsS,
                               Seq.choose (fst DataModel.Value.StringPIso) arr)
      | DataModel.BigInt _ ->
        Some <| Messages.Field(name, Nullable ValueType.STRING, unitsS,
                               Seq.choose (fst DataModel.Value.BigIntPIso) arr
                               |> Seq.map (string))
      | DataModel.Float _ ->
        Some <| Messages.Field(name, Nullable ValueType.DOUBLE, unitsS,
                               Seq.choose (fst DataModel.Value.FloatPIso) arr
                               |> Seq.map (float))
      | DataModel.Bool _ ->
        Some <| Messages.Field(name, Nullable ValueType.BOOL, unitsS,
                               Seq.choose (fst DataModel.Value.BoolPIso) arr)
      | DataModel.Binary _ ->
        // TODO: Handle mime types some way
        Some <| Messages.Field(name, Nullable ValueType.BYTES, unitsS,
                               Seq.choose (fst DataModel.Value.BinaryPIso) arr
                               |> Seq.map (fst))
    | DataModel.Object m as o when not m.IsEmpty ->
      Some <| Messages.Field(name, Nullable ValueType.STRING, "json",
                             [Json.format <| Json.valueToJson o])
    // TODO/CONSIDER: Fractions could also be serialized into an int array with a clarifying representation string
    | DataModel.Fraction _ as frac ->
      Some <| Messages.Field(name, Nullable ValueType.STRING, "json",
                             [Json.format <| Json.valueToJson frac])


type Logary.Heka.Messages.Message with
  static member ofMessage (msg : DataModel.Message) =
    let hmsg = Message(logger = DataModel.PointName.joined msg.name)
    hmsg.severity <- Nullable (msg.level |> LogLevel.toSeverity)
    hmsg.timestamp <- msg.timestamp

    match msg.value with
    | DataModel.Event _ -> hmsg.payload <- Formatting.formatMessage msg
    | _ -> ()

    hmsg

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
    let debug = DataModel.Message.debug >> DataModel.Message.Context.serviceSet "Logary.Targets.Heka" >> Logger.log ri.logger

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
        // TODO: fix
        msg.env_version <- "4.0.0"
        msg.``type``    <- MessageType
        msg.addField (Field("service", Nullable ValueType.STRING, null,
                            [ ri.serviceName ]))

        match msg |> Encoder.encode conf state.stream with
        | Choice1Of2 run ->
          debug "running: writing to heka"
          try
            do! run
            debug "running: wrote to heka"
          with e -> DataModel.Message.error "error writing to heka"
                    |> DataModel.Message.Context.serviceSet "Logary.Targets.Heka"
                    |> DataModel.Message.addExn e |> Logger.log ri.logger
        | Choice2Of2 err ->
          logFailure ri err
        debug "running: recursing"
        return! running state
        }

      debug "running: receiving"

      let! msg, _ = inbox.Receive()
      match msg with
      | Log msg | Measure msg ->
        return! write (Message.ofMessage msg)
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
