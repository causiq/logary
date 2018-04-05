namespace Logary.Services.Rutta

module Router =
  open Hopac
  open System
  open System.Text
  open System.IO
  open Logary
  open Logary.Message
  open Logary.Codecs
  open Logary.Configuration
  open Logary.Targets
  open Logary.EventsProcessing
  open Logary.Formatting
  open Logary.Internals.Chiron
  open fszmq

  type State =
    { zmqCtx: Context
      receiver: Socket
      forwarder: LogManager
      logger: Logger }

    static member create zmqContext receiver forwarder logger =
      { zmqCtx = zmqContext
        receiver = receiver
        forwarder = forwarder
        logger = logger }

    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.receiver :> IDisposable).Dispose()

  let private init baseConf createSocket: State =
    let context = new Context()
    let receiver = createSocket context
    let forwarder = Config.build baseConf |> run
    let targetLogger = forwarder.getLogger (PointName.parse "Logary.Services.Rutta.Router")
    State.create context receiver forwarder targetLogger

  let rec private recvLoop receiver logger =
    match Socket.recvAll receiver with
    // note: sending empty messages
    | null | [||] -> ()
    | datas ->
      let message = Logary.Targets.Shipper.Serialisation.deserialise datas

      Logger.logWithAck logger message.level (fun _ -> message)
      |> run
      |> start

      recvLoop receiver logger

  let pullBind baseConf binding codec =
    printfn "Spawning router in PULL mode from %s" binding
    use state = init baseConf Context.pull
    Socket.bind state.receiver binding
    recvLoop state.receiver state.logger

  let subConnect baseConf binding codec =
    printfn "Spawning router in SUB mode from %s" binding
    use state = init baseConf Context.sub
    Socket.subscribe state.receiver [""B]
    Socket.connect state.receiver binding
    recvLoop state.receiver state.logger

  let tcpBind baseConf binding (codec: Codec) =
    printfn "Spawning router in STREAM mode from %s" binding
    use state = init baseConf Context.stream
    Socket.bind state.receiver binding

    let next =
      codec
      >> Result.map state.logger.logSimple
      >> Job.result

    Logary.Ingestion.TCP.streamRecvLoop state.receiver next

  let udpBind baseConf binding (codec: Codec) =
    ()

  let httpBind baseConf binding (codec: Codec) =
    ()

  type C = Logary.Services.Rutta.Codec

  let private toCodec = function
    | C.Json ->
      Codec.toJsonStringError Codec.json
    | C.Plain ->
      Codec.plain
    | C.Binary ->
      failwith "TODO: turn Shipper.Serialisation.deserialise into Codec = Binary"

  let private toListener baseConf (mode: RMode) =
    let fn =
      match mode with
      | Pull -> pullBind
      | Sub -> subConnect
      | TCP -> tcpBind
      | UDP -> udpBind
      | HTTP -> httpBind
    fn baseConf

  let start (ilevel: LogLevel) (targets: TargetConf list) (listeners: (RMode * string * C) list) =
    let baseConf =
      let hostName = System.Net.Dns.GetHostName()
      Config.create "Logary Rutta[Router]" hostName
      |> Config.targets targets
      |> Config.processing (
          Events.events
          |> Events.sink (targets |> List.map (fun t -> t.name)))
      |> Config.loggerMinLevel ".*" Verbose
      |> Config.ilogger (ILogger.LiterateConsole ilevel)

    listeners
    |> Seq.map (fun (mode, binding, c) -> toListener baseConf mode, binding, toCodec c)
    |> Seq.map (fun (start, binding, codec) -> start binding codec)
    |> ignore // TODO start them async to avoid blocking on the first one