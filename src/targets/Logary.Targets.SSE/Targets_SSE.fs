module Logary.Targets.SSE

open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals
open Logary.Configuration.Target

[<assembly:InternalsVisibleTo("Logary.Targets.SSE.Tests")>]
do()

// NOTE: this target is in ALPHA state (and it's fairly simple)
type SSEConf =
  { path: string
    ip: string
    port: int
  }
  static member create (?path, ?ip, ?port) =
    { path = defaultArg path "/"
      ip = defaultArg ip "0.0.0.0"
      port = defaultArg port 8080
    }

let empty = SSEConf.create()

module internal Impl =

  open System
  open System.Threading
  open Suave
  open Suave.Sockets
  open Suave.Sockets.Control
  type SSEMessage = Suave.EventSource.Message
  open Logary.Internals.Chiron
  module E = Serialization.Json.Encode
  open Logary.Adapters.Facade
  open Logary.Formatting
  open Suave.Utils

  type State =
    private {
      cts: CancellationTokenSource
      ms: Stream.Src<Json list>
    }
    member x.tap () =
      Stream.Src.tap x.ms
    member x.give messages =
      Stream.Src.value x.ms messages
    static member create cts ms = { cts=cts; ms=ms}
    interface IAsyncDisposable with
      member x.DisposeAsync () =
        Job.delay <| fun _ ->
        x.cts.Cancel()
        x.cts.Dispose()
        Job.unit ()

  let serialise (messages: Json list) (conn: Connection) =
    let single = "Logary.Message"
    let multi = "Logary.Message[]"
    socket {
      let mId = (ThreadSafeRandom.nextUInt64 ()).ToString(Culture.invariant)
      let data, typ =
        match messages with
        | m :: [] ->
          Json.formatWith JsonFormattingOptions.Compact m,
          single
        | ms ->
          Json.formatWith JsonFormattingOptions.Compact (Array ms),
          multi
      let message = SSEMessage.createType mId data single
      return! EventSource.send conn message
    }

  let rec iter (stream: Stream<Json list>) (conn: Connection) =
    let hb =
      timeOutMillis 1000 ^->.
      Choice1Of2 (Choice2Of2 "♥️")

    let receive =
      stream ^-> function
        | Stream.Cons (evt, next) ->
          Choice1Of2 (Choice1Of2 (evt, next))
        | Stream.Nil ->
          Choice2Of2 (ConnectionError "Stream terminated, please reconnect.")

    socket {
      let! msgOrHB = Job.toAsync (receive <|> hb)
      match msgOrHB with
      | Choice1Of2 (messages, next) ->
        do! serialise messages conn
        return! iter next conn

      | Choice2Of2 hb ->
        do! EventSource.comment conn hb
        return! iter stream conn
    }

  let setupClient (state: State, ilogger) =
    fun (conn: Connection) ->
      // one tap call per client, as denoted by the connection
      iter (state.tap ()) conn

  let api (state: State) ilogger =
    EventSource.handShake (setupClient (state, ilogger))

  let listen (conf: SSEConf) ilogger =
    job {
      let logging =
        { Suave.Logging.Global.defaultConfig with
            getLogger = fun name ->
              ilogger
              |> Logger.setNameEnding (String.concat "-" name)
              |> LoggerAdapter.createGeneric<Suave.Logging.Logger>
        }
      Suave.Logging.Global.initialiseIfDefault logging
      let bindings = HttpBinding.createSimple HTTP conf.ip conf.port :: []
      do ilogger.info (eventX "Starting HTTP recv-loop at {bindings}." >> setField "bindings" bindings)

      let cts = new CancellationTokenSource()
      let ms = Stream.Src.create ()
      let state = State.create cts ms
      let suaveConfig = { Web.defaultConfig with cancellationToken = cts.Token; bindings = bindings }
      let started, instance = startWebServerAsync suaveConfig (api state ilogger)
      // 1. start the server
      // 2. wait for it to start serving requests
      Async.Start(instance, cts.Token)
      return! Job.fromAsync started >>-. state
    }

  type Message with
    member x.toJSONMessage() =
      Json.encode x

  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf: SSEConf) (api: TargetAPI) =
    let ilogger = api.runtime.logger

    let rec initialise () =
      job {
        let! state = listen conf ilogger
        return! running state
      }

    and running (state: State): Job<_> =
      Alt.choose [
        RingBuffer.takeBatch 10us api.requests ^=> fun messages ->
          let entries, acks, flushes =
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                message.toJSONMessage() :: entries,
                ack *<= () :: acks,
                flushes
              | Flush (ackCh, nack) ->
                entries,
                acks,
                ackCh *<= () :: flushes)
              ([], [], [])

          job {
            do ilogger.verbose (eventX "Sending {count} messages to all clients." >> setField "count" entries.Length)
            do! state.give entries
            do ilogger.verbose (eventX "Acking messages.")
            do! Job.conIgnore acks
            do! Job.conIgnore flushes
            return! running state
          }

        api.shutdownCh ^=> fun ack ->
          (state :> IAsyncDisposable).DisposeAsync()
          >>=. ilogger.verboseWithBP (eventX "Disposed state, acking...")
          >>=. ack *<= ()
      ] :> Job<_>

    initialise ()

/// Create a new SSE target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New(s => s.Target<BigQuery>())
type Builder(conf, callParent: ParentCallback<Builder>) =
  let update (conf': SSEConf): Builder =
    Builder(conf', callParent)
  member x.Path(path: string) =
    update { conf with path = path }
  member x.IP(ip: string) =
    update { conf with ip = ip }
  member x.Port(port: int) =
    update { conf with port = port }
  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
