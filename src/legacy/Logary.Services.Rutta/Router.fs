namespace Logary.Services.Rutta

module Router =
  open Hopac
  open System
  open System.Text
  open System.IO
  open Logary
  open Logary.Message
  open Logary.Configuration
  open Logary.Targets
  open Logary.EventsProcessing
  open Logary.Formatting
  open Logary.Internals.Chiron
  open fszmq
  open MBrace.FsPickler
  open MBrace.FsPickler.Combinators

  type State =
    { zmqCtx: Context
      receiver: Socket
      forwarder: LogManager
      logger: Logger }
    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.receiver :> IDisposable).Dispose()

  let private init binding (targetUris: Uri list) createSocket mode: State =
    let context = new Context()
    let receiver = createSocket context

    let forwarder =
      let hostName = System.Net.Dns.GetHostName()
      let targets = targetUris |> List.map TargetConfig.create

      Config.create (sprintf "Logary Rutta[%s]" mode) hostName
      |> Config.targets targets
      |> Config.processing (
          Events.events
          |> Events.sink (targets |> List.map (fun t -> t.name)))
      |> Config.loggerMinLevel ".*" Verbose
      |> Config.ilogger (ILogger.LiterateConsole Debug)
      |> Config.build
      |> Hopac.Hopac.run

    let targetLogger = forwarder.getLogger (PointName.parse "Logary.Services.Rutta.Router")

    { zmqCtx    = context
      receiver  = receiver
      forwarder = forwarder
      logger    = targetLogger }

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

  let pullFrom binding = function
    | Router_Target target :: _ ->
      printfn "Spawning router in PULL mode from %s" binding
      use state = init binding [ Uri target ] Context.pull "PULL"
      Socket.bind state.receiver binding
      Choice1Of2 (recvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "Unknown parameter(s) %A" x)

  let xsubBind binding = function
    | Router_Target target :: _ ->
      printfn "Spawning router in SUB mode from %s" binding
      use state = init binding [ Uri target ] Context.sub "SUB"
      Socket.subscribe state.receiver [""B]
      Socket.connect state.receiver binding
      Choice1Of2 (recvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "Unknown parameter(s) %A" x)

  let rec private streamRecvLoop receiver (logger: Logger) =
    // https://gist.github.com/lancecarlson/fb0cfd0354005098d579
    // https://gist.github.com/claws/7231548#file-czmq-stream-server-c-L21
    try
      let frame = Socket.recv receiver
      // Aborted socket due to closing process:
      if isNull frame then () else
      let bs = Socket.recv receiver
      // http://api.zeromq.org/4-1:zmq-socket#toc19
      // A connection was made
      if bs.Length = 0 then streamRecvLoop receiver logger else
      // printfn "Data received: %A" message
      use ms = new MemoryStream(bs)
      use sr = new StreamReader(ms, Encoding.UTF8)
      while not sr.EndOfStream do
        let line = sr.ReadLine()
        match Json.parse line |> JsonResult.bind Json.decodeMessage with
        | JPass message ->
          logger.logSimple message
        | JFail failure ->
          logger.verbose (eventX "JFail: {line} => {failure}" >> setField "line" line >> setField "failure" failure)
      //printfn "Looping..."

      streamRecvLoop receiver logger

    with :? ZMQError as zmq ->
      logger.info (eventX "Shutting down streamRecvLoop due to {exn}." >> setField "exn" zmq.Message >> addExn zmq)
      ()

  let streamBind binding = function
    | Router_Target target :: _ ->
      printfn "Spawning router in STREAM mode from %s" binding
      use state = init binding [ Uri target ] Context.stream "STREAM"
      Socket.bind state.receiver binding
      Choice1Of2 (streamRecvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "Unknown parameter(s) %A" x)

