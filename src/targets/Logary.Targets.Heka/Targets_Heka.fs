module Logary.Targets.Heka

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Net.Security
open Logary.Serialisation.Chiron
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Heka
open Logary.Heka.Client
open Logary.Heka.Messages
open Logary.Target
open Logary.Internals
open Logary.Configuration

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
  static member ofMessage (msg : Logary.Message) =
    let hmsg = Message(logger = PointName.format msg.name)
    hmsg.severity <- Nullable (msg.level |> LogLevel.toSeverity)
    hmsg.timestamp <- msg.timestamp
    hmsg.payload <- MessageWriter.verbatim.format msg
    hmsg

/// The message type most often used in filters inside Heka (see e.g. the getting-
/// started guide).
let MessageType = "heka.logary"

module internal Impl =

  let logFailure (ri : RuntimeInfo) = function
    | HeaderTooLarge err ->
      ri.logger.warnWithBP (eventX err)
    | MessageTooLarge err ->
      ri.logger.warnWithBP (eventX err)

  type State =
    { client   : TcpClient
      stream   : Stream
      hostname : string }

    interface IDisposable with
      member x.Dispose () =
        (x.client :> IDisposable).Dispose()
        x.stream.Dispose()

  let loop (conf : HekaConfig)
           (ri : RuntimeInfo, api : TargetAPI) : Job<unit> =

    let rec initialise () : Job<unit> =
      job {
        do! ri.logger.debugWithBP (eventX "Initialising heka target.")

        let ep, useTLS = conf.endpoint
        let client = new TcpClient()
        client.NoDelay <- true
        do! Job.fromUnitTask (fun _ -> client.ConnectAsync(ep.Address, ep.Port))

        let stream =
          if useTLS then
            let validate = new RemoteCertificateValidationCallback(fun _ -> conf.caValidation)
            let sslStream = new SslStream(client.GetStream(), false, validate)
            //sslStream.AuthenticateAsClient(ep.Address) // or ep.Hostname
            sslStream :> Stream
          else
            client.GetStream() :> Stream

        do! ri.logger.debugWithBP (eventX "initialise: tcp stream open")

        let hostname = Dns.GetHostName()
        return! running { client = client; stream = stream; hostname = hostname }
      }

    and running state : Job<unit> =
      let write (msg : Message) = job {
        msg.uuid        <- Guid.NewGuid().ToByteArray()
        msg.hostname    <- state.hostname
        // TODO: ensure right Logary version is sent
        msg.env_version <- "5.0.0"
        msg.``type``    <- MessageType
        msg.addField (Field("service", Nullable ValueType.STRING, null,
                            [ ri.service ]))

        match msg |> Encoder.encode conf state.stream with
        | Choice1Of2 run ->
          do! ri.logger.debugWithBP (eventX "Writing to heka" >> setNameEnding "running")
          try
            do! run
            do! ri.logger.debugWithBP (eventX "Wrote to heka" >> setNameEnding "running")

          with e ->
            do! ri.logger.errorWithBP (eventX "error writing to heka" >> addExn e)

        | Choice2Of2 err ->
          do! logFailure ri err
      }

      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          job {
            do! Job.Scheduler.isolate <| fun _ ->
              try
                    (state :> IDisposable).Dispose ()
              with e ->
                Message.eventError "Heka target disposing tcp stream, then client."
                |> Message.addExn e
                |> Logger.logSimple ri.logger
              
            do! ack *<= ()
          }

        RingBuffer.take api.requests ^=> function
          | Log (logMsg, ack) ->
            ri.logger.debugWithBP (eventX "running: received message")
            >>=. write (Message.ofMessage logMsg)
            >>=. running state

          | Flush (ack, nack) ->
            ack *<= () >>=. running state

      ] :> Job<_>

    initialise ()

/// Create a new Noop target
let create conf = TargetConf.createSimple (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<Heka.Builder>() )
type Builder(conf, callParent : Target.ParentCallback<Builder>) =
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

  new(callParent : Target.ParentCallback<_>) =
    Builder(HekaConfig.Empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
