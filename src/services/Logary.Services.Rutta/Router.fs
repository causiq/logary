namespace Logary.Services.Rutta

module Router =
  open Hopac
  open Hopac.Infixes
  open Hopac.Extensions
  open System
  open System.Net
  open Logary
  open Logary.Message
  open Logary.Codecs
  open Logary.Ingestion
  open Logary.Configuration
  open Logary.Targets
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

  let private createSink (logary: LogManager) (codec: Logary.Codecs.Codec) =
    let targetLogger = logary.getLogger (PointName.parse "Rutta")
    codec >> Result.map (Array.iter targetLogger.logSimple) >> Job.result

  let internal zmqRecv createSocket =
    let recv (started, shutdown) (cfg: IngestServerConfig) next =
      Job.Scheduler.isolate <| fun () ->
      use context = new Context()
      use socket = createSocket context
      queue (started *<= ())
      while not (Promise.Now.isFulfilled cfg.cancelled) do
        // note: sending empty messages
        match Socket.recvAll socket with
        | null
        | [||] -> ()
        | datas ->
          start (
            next (Ingested.ofBytes (Array.concat datas)) |> Job.map (function
            | Result.Ok () -> ()
            | Result.Error err -> cfg.ilogger.info (eventX "Error from sink {err}" >> setField "err" err)))
      queue (shutdown *<= ())

    IngestServer.create recv

  /// ZMQ PULL
  let pullBind (cancelled, ilogger: Logger, logary) (binding: string) (cname, codec) =
    ilogger.info (
      eventX "Spawning router with {binding} in {mode} mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "mode" Pull
      >> setField "codec" cname)

    let create (context: Context) =
      let socket = Context.pull context
      Socket.bind socket (sprintf "tcp://%s" binding)
      socket
    
    let config =
      let ilogger = ilogger |> Logger.setNameEnding "zmqPull"
      { new IngestServerConfig with
          member x.cancelled = cancelled
          member x.ilogger = ilogger }

    let next = createSink logary codec
    zmqRecv create config next

  /// ZMQ SUB
  let subConnect (cancelled, ilogger: Logger, logary) (binding: string) (cname, codec) =
    ilogger.info (
      eventX "Spawning router with {binding} in {mode} mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "mode" Sub
      >> setField "codec" cname)

    let create (context: Context) =
      let socket = Context.sub context
      Socket.subscribe socket [""B]
      Socket.connect socket (sprintf "tcp://%s" binding)
      socket
      
    let config =
      let ilogger = ilogger |> Logger.setNameEnding "zmqSub"
      { new IngestServerConfig with
          member x.cancelled = cancelled
          member x.ilogger = ilogger }

    let next = createSink logary codec
    zmqRecv create config next

  /// ZMQ STREAM
  let tcpBind (cancelled, ilogger: Logger, logary) (binding: string) (cname, codec) =
    ilogger.info (
      eventX "Spawning router with {binding} in {mode} mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "mode" TCP
      >> setField "codec" cname)

    let create (context: Context) =
      let socket = Context.stream context
      Socket.bind socket (sprintf "tcp://%s" binding)
      socket

    let ilogger = ilogger |> Logger.setNameEnding "zmqStream"
    let config = TCPConfig.create create cancelled ilogger
    let next = createSink logary codec
    TCP.create config next

  /// UDP
  let udpBind (cancelled, ilogger: Logger, logary) (binding: string) (cname, codec) =
    ilogger.info (
      eventX "Spawning router with {binding} in {mode} mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "mode" UDP
      >> setField "codec" cname)

    let ilogger = ilogger |> Logger.setNameEnding "udp"
    let ep = Parsers.binding binding
    let config = UDPConfig.create ep cancelled ilogger
    let next = createSink logary codec
    UDP.create config next

  // HTTP
  let httpBind cors (cancelled, ilogger: Logger, logary) binding (cname, codec) =
    ilogger.info (
      eventX "Spawning router with {binding} in {mode} mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "mode" HTTP
      >> setField "codec" cname)

    let acao = if cors then Some OriginResult.allowAll else None
    let ilogger = ilogger |> Logger.setNameEnding "http"
    let ep = Parsers.binding binding
    let config = HTTPConfig.create("/i/logary", logary, ilogger, cancelled, ep, ?accessControlAllowOrigin = acao)
    let next = createSink logary codec
    HTTP.create config next

  type C = Logary.Services.Rutta.Codec

  let private toCodec = function
    | C.Json ->
      "json", Codec.toJsonStringError Codec.json
    | C.Plain ->
      "plain", Codec.plain
    | C.Binary ->
      "binary",
      Ingested.forceBytes
      >> Shipper.Serialisation.deserialise
      >> Array.singleton
      >> Result.Ok
    | C.Log4jXML ->
      "log4jxml",
      Codec.log4jXML

  type Binding = string
  
  let private toListener (cancelled, cors, ilogger, baseConf) (mode: RMode): Binding -> string * Codec -> Job<IngestServer> =
    let fn =
      match mode with
      | Pull -> pullBind
      | Sub -> subConnect
      | TCP -> tcpBind
      | UDP -> udpBind
      | HTTP -> httpBind cors
    fn (cancelled, ilogger, baseConf)

  let start (cors, ilevel: LogLevel) (targets: TargetConf list) (listeners: (RMode * string * C) list) =
    let cancelled = IVar ()

    let logary =
      let hostName = Dns.GetHostName()
      Config.create "Logary Rutta[Router]" hostName
      |> Config.targets targets
      |> Config.processing (
          Events.events
          |> Events.sink (targets |> List.map (fun t -> t.name)))
      |> Config.loggerMinLevel ".*" Verbose
      |> Config.ilogger (ILogger.LiterateConsole ilevel)
      |> Config.build
      |> run
      
    let ilogger = logary.runtimeInfo.logger |> Logger.setName "Rutta"

    ilogger.debug (
      eventX "Starting {@listeners}, sending to {@targets}"
      >> setField "listeners" listeners
      >> setField "targets" (targets |> List.map (fun t -> t.name)))
    
    let servers =
      listeners
      |> Seq.map (fun (mode, binding, c) -> toListener (cancelled, cors, ilogger, logary) mode, binding, toCodec c)
      |> Seq.mapJob (fun (exec, binding, codec) -> exec binding codec)
      |> run
      
    let allShutdown =
      servers
      |> Seq.map IngestServer.waitForShutdown
      |> Job.conIgnore
    
    ilogger.debug (
      eventX "Router start of {@listeners}, sending to {@targets}, successful."
      >> setField "listeners" listeners
      >> setField "targets" (targets |> List.map (fun t -> t.name)))
      
    { new IDisposable with
        member x.Dispose () =
          run (
            cancelled *<= ()
            >>=. allShutdown
            >>=. logary.shutdown ()
          )
    }