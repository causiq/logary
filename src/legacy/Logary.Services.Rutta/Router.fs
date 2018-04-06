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

  let private createSink (logary: LogManager) codec =
    let targetLogger = logary.getLogger (PointName.parse "Logary.Services.Rutta.Router")
    codec >> Result.map targetLogger.logSimple >> Job.result

  let internal recv cancelled createSocket (ilogger: Logger) (sink: Ingest) =
    let loop () =
      use context = new Context()
      use socket = createSocket context
      while not (Promise.Now.isFulfilled cancelled) do
        // note: sending empty messages
        match Socket.recvAll socket with
        | null
        | [||] -> ()
        | datas ->
          start (
            sink (Ingested.ofBytes (Array.concat datas)) |> Job.map (function
            | Result.Ok () -> ()
            | Result.Error err -> ilogger.info (eventX "Error from sink {err}" >> setField "err" err)))

    let iJ = Job.Scheduler.isolate loop
    Job.queue iJ

  let pullBind (cancelled, logary) (binding: string) (cname, codec) =
    let sink = createSink logary codec
    logary.runtimeInfo.logger.info (
      eventX "Spawning router with {binding} in PULL mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "codec" cname)

    let create (context: Context) =
      let socket = Context.pull context
      Socket.bind socket (sprintf "tcp://%s" binding)
      socket

    recv cancelled create logary.runtimeInfo.logger sink

  let subConnect (cancelled, logary) (binding: string) (cname, codec) =
    let sink = createSink logary codec
    logary.runtimeInfo.logger.info (
      eventX "Spawning router with {binding} in SUB mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "codec" cname)

    let create (context: Context) =
      let socket = Context.sub context
      Socket.subscribe socket [""B]
      Socket.connect socket (sprintf "tcp://%s" binding)
      socket

    recv cancelled create logary.runtimeInfo.logger sink

  let tcpBind (cancelled, logary) (binding: string) (cname, codec) =
    let sink = createSink logary codec
    logary.runtimeInfo.logger.info (
      eventX "Spawning router with {binding} in UDP mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "codec" cname)

    let create (context: Context) =
      let socket = Context.stream context
      Socket.bind socket (sprintf "tcp://%s" binding)
      socket

    let config = TCP.TCPConfig.create create cancelled logary.runtimeInfo.logger
    TCP.create config sink

  let udpBind (cancelled, logary) (binding: string) (cname, codec) =
    let sink = createSink logary codec
    logary.runtimeInfo.logger.info (
      eventX "Spawning router with {binding} in UDP mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "codec" cname)

    let ep = Parsers.binding binding
    let config = UDP.UDPConfig.create ep cancelled logary.runtimeInfo.logger
    UDP.create config sink

  let httpBind (cancelled, logary) binding (cname, codec) =
    let sink = createSink logary codec
    logary.runtimeInfo.logger.info (
      eventX "Spawning router with {binding} in HTTP mode, accepting {codec}."
      >> setField "binding" binding
      >> setField "codec" cname)

    let ep = Parsers.binding binding
    let config = HTTP.HTTPConfig.create("/i/logary", cancelled, ep)
    HTTP.create config sink

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
      >> Result.Ok
    | C.Log4jXML ->
      "log4jxml",
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
          run (
            logary.shutdown () >>=.
            cancelled *<= ()
          )
    }