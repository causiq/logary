module Logary.Services.Rutta.Router

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open System
open Logary
open Logary.Codecs
open Logary.Ingestion
open Logary.Ingestion.HTTP
open Logary.Ingestion.HTTP.CORS
open Logary.Configuration
open Logary.Model
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

let private createIngest (logary: LogManager) (codec: Logary.Codecs.Codec): Ingest =
  let targetLogger = logary.getLogger "Logary.Rutta.Ingest"
  //codec >> Result.map (Array.iter targetLogger.logWith) >> Job.result
  fun input ->
    match codec input with
    | Result.Ok messages ->
      let received = logary.runtimeInfo.getTimestamp()
      messages
        |> Array.map (fun m -> m.received <- Some received; m)
        |> Array.iter targetLogger.log
      Job.result (Result.Ok ())
    | Result.Error error ->
      Job.result (Result.Error error)

let internal zmqRecv (context: Context) createSocket =
  let recv (started, shutdown) (cfg: IngestServerConfig) next =
    Job.Scheduler.isolate <| fun () ->
    use socket = createSocket context
    queue (started *<= ())
    while not (Promise.Now.isFulfilled cfg.cancelled) do
      // note: sending empty messages
      match Socket.recvAll socket with
      | null
      | [||] -> ()
      | datas ->
        start (
          next (Ingested.ofBytes (Array.concat datas))
            |> Job.map (function
            | Result.Ok () -> ()
            | Result.Error err -> cfg.ilogger.info("Error from sink {err}", fun m -> m.setField("err", string err))))
    queue (shutdown *<= ())

  IngestServer.create recv

// ZMQ API docs http://api.zeromq.org/master:_start
// Context: A ØMQ context is thread safe and may be shared among as many application threads as necessary, without any additional locking required on the part of the caller.
// Sockets: ØMQ has both thread safe socket type and not thread safe socket types. Applications MUST NOT use a not thread safe socket from multiple threads except after migrating a socket from one thread to another with a "full fence" memory barrier.

type IngestServerConfigImpl(cancelled, ilogger, bindings) =
  interface IngestServerConfig with
    member x.cancelled = cancelled
    member x.ilogger = ilogger
    member x.bindings = bindings

/// ZMQ PULL
let pullBind (context: Context) (cancelled, ilogger: Logger, _, liveLogary) (bindings: BindingList) (cname: string, codec) =
  ilogger.info ("Spawning router with {binding} in {mode} mode, accepting {codec}.", fun m ->
    m.setField("bindings", bindings)
    m.setField("mode", "pull")
    m.setField("codec", cname))

  let create (context: Context) =
    let socket = Context.pull context
    for binding in bindings do
      Socket.bind socket (sprintf "tcp://%s" binding.nicAndPort)
    socket

  let ingest = createIngest liveLogary codec
  let config = IngestServerConfigImpl(cancelled, ilogger |> Logger.setNameEnding "zmqPull", bindings) :> IngestServerConfig
  zmqRecv context create config ingest

/// ZMQ SUB
let subConnect (context: Context) (cancelled, ilogger: Logger, _, liveLogary) (bindings: BindingList) (cname: string, codec) =
  ilogger.info ("Spawning router with {binding} in {mode} mode, accepting {codec}.", fun m ->
    m.setField("bindings", bindings)
    m.setField("mode", "sub")
    m.setField("codec", cname))

  let create (context: Context) =
    let socket = Context.sub context
    Socket.subscribe socket [""B]
    for binding in bindings do
      Socket.connect socket (sprintf "tcp://%s" binding.nicAndPort)
    socket

  let config = IngestServerConfigImpl(cancelled, ilogger |> Logger.setNameEnding "zmqSub", bindings) :> IngestServerConfig
  let ingest = createIngest liveLogary codec
  zmqRecv context create config ingest

/// ZMQ STREAM
let tcpBind (context: Context) (cancelled, ilogger: Logger, _, liveLogary) (bindings: BindingList) (cname: string, codec) =
  ilogger.info ("Spawning router with {binding} in {mode} mode, accepting {codec}.", fun m ->
    m.setField("bindings", bindings)
    m.setField("mode", "tcp")
    m.setField("codec", cname))

  let ilogger = ilogger |> Logger.setNameEnding "zmqStream"
  let config = TCPConfig.create context bindings cancelled ilogger
  let next = createIngest liveLogary codec
  TCP.create config next

/// UDP
let udpBind (cancelled, ilogger: Logger, _, liveLogary) (bindings: BindingList) (cname: string, codec) =
  ilogger.info ("Spawning router with {binding} in {mode} mode, accepting {codec}.", fun m ->
    m.setField("binding", bindings)
    m.setField("mode", "udp")
    m.setField("codec", cname))

  let ilogger = ilogger |> Logger.setNameEnding "udp"
  let config = UDPConfig.create bindings cancelled ilogger
  let next = createIngest liveLogary codec
  UDP.create config next

// HTTP
let httpBind cors (cancelled, ilogger: Logger, internalLogary: LogManager, liveLogary: LogManager) (bindings: BindingList) (cname: string, codec) =
  ilogger.info ("Spawning router with {bindings} in {mode} mode, accepting {codec}.", fun m ->
    m.setField("bindings", bindings)
    m.setField("mode", HTTP.ToString())
    m.setField("codec", cname))

  let acao = if cors then Some OriginResult.allowAll else None
  let corsConfig = CORSConfig.create(?accessControlAllowOrigin = acao)
  let config = HTTPConfig.create("/", ilogger, internalLogary, liveLogary, cancelled, bindings, corsConfig=corsConfig)
  let next = createIngest liveLogary codec
  HTTP.create config next

// Private:
let private toCodec = function
  | RuttaCodec.Json ->
    "json", Codec.json
  | RuttaCodec.Plain ->
    "plain", Codec.plain
  | RuttaCodec.Binary ->
    "binary",
    Ingested.forceBytes
    >> Shipper.Serialisation.deserialise
    >> Array.singleton
    >> Result.Ok
  | RuttaCodec.Log4jXML ->
    "log4jxml",
    Codec.log4jXML

let private listenersToString (ls: #seq<(RuttaMode * BindingList * RuttaCodec)>) =
  ls
    |> Seq.map (fun (r, bindings, c) -> sprintf "%O %s %O" r (bindings.toCommaSeparatedString()) c)
    |> String.concat ", "

let private targetsToString (ts: #seq<TargetConf>) =
  ts
    |> Seq.map (fun t -> t.name)
    |> String.concat ","

// Public:
let start (cors, internalLogary: LogManager) (targets: TargetConf list) (listeners: (RuttaMode * BindingList * RuttaCodec) list) =
  let cancelled = IVar ()

  let ilogger = internalLogary.getLogger "Logary.Rutta.Router"

  ilogger.debug("Starting {listeners}, sending to {targets}", fun m ->
    m.setField("targets", targetsToString targets)
    m.setField("listeners", listenersToString listeners))

  let zmqContext = lazy (new Context())

  let live =
    internalLogary.runtimeInfo.resource
      |> Resource.setDetail (function Named (c, _) when c = "component" -> true | _ -> false)
                            (Named ("component", "router"))
      |> Config.create
      |> Config.targets targets
      |> Config.processing (
          Events.events
          |> Events.setTargets (targets |> List.map (fun t -> t.name)))
      |> Config.loggerMinLevel ".*" Verbose
      |> Config.ilogger (ILogger.External ilogger)
      |> Config.consoleLock internalLogary.runtimeInfo.consoleLock
      |> Config.buildAndRun

  let servers =
    listeners
      |> Seq.mapJob (fun (mode, bindings, ruttaCodec) ->
        let factory =
          match mode with
          | Pull -> pullBind zmqContext.Value
          | Sub -> subConnect zmqContext.Value
          | TCP -> tcpBind zmqContext.Value
          | UDP -> udpBind
          | HTTP -> httpBind cors
        factory (cancelled, ilogger, internalLogary, live) bindings (toCodec ruttaCodec))
      |> run

  let allShutdown =
    servers
    |> Seq.map IngestServer.waitForShutdown
    |> Job.conIgnore

  ilogger.debug ("Router start of {listeners}, sending to {targets}, successful.", fun m ->
    m.setField("listeners", listenersToString listeners)
    m.setField("targets", targetsToString targets))

  { new IDisposable with
      member x.Dispose () =
        // first, cancel all listeners and wait for them to shut down
        run (cancelled *<= () >>=. allShutdown)

        // now dispose the ZMQ context
        if zmqContext.IsValueCreated then
          (zmqContext.Value :> IDisposable).Dispose()
  }