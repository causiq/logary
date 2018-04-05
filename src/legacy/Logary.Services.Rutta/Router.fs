namespace Logary.Services.Rutta

module Router =
  open Hopac
  open System
  open System.Text
  open System.IO
  open Logary
  open Logary.Message
  open Logary.Codecs
  open Logary.Ingestion
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
      logger: Logger
      sink: Ingest
    }

    static member create zmqContext receiver forwarder logger sink =
      { zmqCtx = zmqContext
        receiver = receiver
        forwarder = forwarder
        logger = logger
        sink = sink
      }

    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.receiver :> IDisposable).Dispose()

  let private initZMQ baseConf codec createSocket: State =
    let context = new Context()
    let receiver = createSocket context
    let forwarder = Config.build baseConf |> run
    let targetLogger = forwarder.getLogger (PointName.parse "Logary.Services.Rutta.Router")
    let sink = codec >> Result.map targetLogger.logSimple >> Job.result
    State.create context receiver forwarder targetLogger sink

  let internal recv cancelled receiver (sink: Ingest) =
    let rec loop () =
      if Promise.Now.isFulfilled cancelled then Job.result () else
      match Socket.recvAll receiver with
      // note: sending empty messages
      | null
      | [||] ->
        Job.result ()
      | datas ->
        sink (Ingested.ofBytes (Array.concat datas)) |> Job.bind (fun _ -> loop ())
    loop ()

  let pullBind (cancelled, baseConf) binding codec =
    printfn "Spawning router in PULL mode from %s" binding
    use state = initZMQ baseConf codec Context.pull
    Socket.bind state.receiver binding
    recv cancelled state.receiver state.sink

  let subConnect (cancelled, baseConf) binding codec =
    printfn "Spawning router in SUB mode from %s" binding
    use state = initZMQ baseConf codec Context.sub
    Socket.subscribe state.receiver [""B]
    Socket.connect state.receiver binding
    recv cancelled state.receiver state.logger

  let tcpBind (cancelled, baseConf) binding (codec: Codec) =
    printfn "Spawning router in STREAM mode from %s" binding
    use state = initZMQ baseConf codec Context.stream
    Socket.bind state.receiver binding
    Logary.Ingestion.TCP.streamRecvLoop state.receiver state.sink

  let udpBind (cancelled, baseConf) binding (codec: Codec) =
    let next =
      codec
      >> Result.map state.logger.logSimple
      >> Job.result
    Logary.Ingestion.UDP.create binding next

  let httpBind (cancelled, baseConf) binding (codec: Codec) =

    ()

  type C = Logary.Services.Rutta.Codec

  let private toCodec = function
    | C.Json ->
      Codec.toJsonStringError Codec.json
    | C.Plain ->
      Codec.plain
    | C.Binary ->
      failwith "TODO: turn Shipper.Serialisation.deserialise into Codec = Binary"

  let private toListener (cancelled, baseConf) (mode: RMode) =
    let fn =
      match mode with
      | Pull -> pullBind
      | Sub -> subConnect
      | TCP -> tcpBind
      | UDP -> udpBind
      | HTTP -> httpBind
    fn (cancelled, baseConf)

  let start (ilevel: LogLevel) (targets: TargetConf list) (listeners: (RMode * string * C) list) =
    let cancelled = IVar ()
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
    |> Seq.map (fun (mode, binding, c) -> toListener (cancelled, baseConf) mode, binding, toCodec c)
    |> Seq.map (fun (exec, binding, codec) -> exec binding codec)
    |> ignore // TODO start them async to avoid blocking on the first one