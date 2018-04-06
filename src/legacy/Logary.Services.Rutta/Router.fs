namespace Logary.Services.Rutta

module Router =
  open Hopac
  open Hopac.Infixes
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

    static member create zmqContext receiver logary logger sink =
      { zmqCtx = zmqContext
        receiver = receiver
        forwarder = logary
        logger = logger
        sink = sink
      }

    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.receiver :> IDisposable).Dispose()

  let private init (logary: LogManager) codec =
    let targetLogger = logary.getLogger (PointName.parse "Logary.Services.Rutta.Router")
    let sink = codec >> Result.map targetLogger.logSimple >> Job.result
    targetLogger, sink

  let private initZMQ (logary: LogManager) codec (modeName, createSocket): State =
    let targetLogger, sink = init logary codec
    logary.runtimeInfo.logger.info (eventX "Spawning router in {modeName} mode." >> setField "modeName" modeName)
    let context = new Context()
    let receiver = createSocket context
    State.create context receiver logary targetLogger sink

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

  let pullBind (cancelled, baseConf) (binding: string) codec =
    let state = initZMQ baseConf codec ("PULL", Context.pull)
    state.forwarder.runtimeInfo.logger.info (eventX "Socket.bind receiver {binding} being called (PULL)" >> setField "binding" binding)
    Socket.bind state.receiver (sprintf "tcp://%s" binding)
    Job.using state <| fun state ->
    recv cancelled state.receiver state.sink

  let subConnect (cancelled, baseConf) (binding: string) codec =
    let state = initZMQ baseConf codec ("SUB", Context.sub)
    state.forwarder.runtimeInfo.logger.info (eventX "Socket.connect receiver {binding} being called (SUB)" >> setField "binding" binding)
    Socket.subscribe state.receiver [""B]
    Socket.connect state.receiver (sprintf "tcp://%s" binding)
    Job.using state <| fun state ->
    recv cancelled state.receiver state.sink

  let tcpBind (cancelled, baseConf) (binding: string) (codec: Codec) =
    let state = initZMQ baseConf codec ("STREAM", Context.stream)
    let config = TCP.TCPConfig.create state.receiver cancelled
    state.forwarder.runtimeInfo.logger.info (eventX "Socket.bind receiver {binding} being called (STREAM)" >> setField "binding" binding)
    Socket.bind state.receiver (sprintf "tcp://%s" binding)
    Job.using state <| fun state ->
    TCP.create config state.sink

  let udpBind (cancelled, logary) (binding: string) (codec: Codec) =
    let logger, sink = init logary codec
    let config = UDP.UDPConfig.create (Parsers.binding binding) cancelled
    let next =
      codec
      >> Result.map logger.logSimple
      >> Job.result
    UDP.create config next

  let httpBind (cancelled, logary) binding (codec: Codec) =
    let logger, sink = init logary codec
    let ep = Parsers.binding binding
    let config = HTTP.HTTPConfig.create("/i/logary", cancelled, ep)
    let next =
      codec
      >> Result.map logger.logSimple
      >> Job.result
    HTTP.create config next

  type C = Logary.Services.Rutta.Codec

  let private toCodec = function
    | C.Json ->
      Codec.toJsonStringError Codec.json
    | C.Plain ->
      Codec.plain
    | C.Binary ->
      Ingested.forceBytes
      >> Shipper.Serialisation.deserialise
      >> Result.Ok
    | C.Log4j ->
      Codec.log4jXML

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

    let logary =
      let hostName = System.Net.Dns.GetHostName()
      Config.create "Logary Rutta[Router]" hostName
      |> Config.targets targets
      |> Config.processing (
          Events.events
          |> Events.sink (targets |> List.map (fun t -> t.name)))
      |> Config.loggerMinLevel ".*" Verbose
      |> Config.ilogger (ILogger.LiterateConsole ilevel)
      |> Config.build
      |> run

    logary.runtimeInfo.logger.debug (eventX "Starting {@listeners}" >> setField "listeners" listeners)

    listeners
    |> Seq.map (fun (mode, binding, c) -> toListener (cancelled, logary) mode, binding, toCodec c)
    |> Seq.map (fun (exec, binding, codec) -> exec binding codec)
    |> Seq.iter start

    { new IDisposable with
        member x.Dispose () =
          run (cancelled *<= ())
    }