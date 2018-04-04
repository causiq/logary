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
      |> run

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

  let streamBind binding targets =
    let targets = targets |> List.choose (function Router_Target target -> Some (Uri target) | _ -> None)

    if List.length targets = 0 then
      Choice2Of2 (sprintf "Unknown parameter(s) %A" targets)
    else
      printfn "Spawning router in STREAM mode from %s" binding
      use state = init binding targets Context.stream "STREAM"
      Socket.bind state.receiver binding

      let next =
        Codec.toJsonStringError Codec.json
        >> Result.map state.logger.logSimple
        >> Job.result

      Choice1Of2 (Logary.Ingestion.TCP.streamRecvLoop state.receiver next)