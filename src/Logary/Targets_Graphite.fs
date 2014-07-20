/// A Logary target for Graphite, which is a plotting/graphing
/// server.
module Logary.Targets.Graphite

open FSharp.Actor

open NodaTime

open System
open System.Net
open System.Net.Sockets
open System.Globalization

open Logary
open Logary.Internals
open Logary.Target
open Logary.Measure
open Logary.Internals.Tcp

/// Put this tag on your message (message must be a parseable value)
/// if you want graphite to find it, or use the Metrics API of Logary.
let [<Literal>] TriggerTag = "plottable"

/// Configuration for loggin to graphite.
/// TODO: prefixing with hostname etc
type GraphiteConf =
  { hostname  : string
    port      : uint16
    clientFac : string -> uint16 -> WriteClient }
  static member Create(hostname, ?port, ?clientFac) =
    let port = defaultArg port 2003us
    let clientFac = defaultArg clientFac (fun host port -> new TcpWriteClient(new TcpClient(host, int port)) :> WriteClient)
    { hostname = hostname
      port = port
      clientFac = clientFac }

type private GraphiteState =
  { client         : WriteClient
    sendRecvStream : WriteStream option }

let private utf8 = System.Text.Encoding.UTF8

let private tryDispose (item : 'a option) =
  if item.IsNone then ()
  else match item.Value |> box with
        | :? IDisposable as disposable -> try disposable.Dispose() with _ -> ()
        | _ -> ()

/// All graphite messages are of the following form.
/// metric_path value timestamp\n
let private createMsg path value (timestamp : Instant) =
  let line = String.Format("{0} {1} {2}\n", path, value, timestamp.Ticks / NodaConstants.TicksPerSecond)
  utf8.GetBytes line

let private findPath (tags : string list) =
  let tags' = tags |> List.fold (fun s t -> s |> Set.add t) Set.empty
  if not (tags' |> Set.contains TriggerTag) then
    None
  else
    tags |> List.tryFind (fun t -> t.StartsWith("path"))

let private doWrite state m =
  async {
    let stream =
      match state.sendRecvStream with
      | None   -> state.client.GetStream()
      | Some s -> s
    do! stream.Write( m )
    return { state with sendRecvStream = Some stream } }

let private graphiteLoop (conf : GraphiteConf) (svc : RuntimeInfo) =
  (fun (inbox : IActor<_>) ->
    let rec init () =
      async {
        let client = conf.clientFac conf.hostname conf.port
        return! running { client = client; sendRecvStream = None } }

    and running state =
      async {
        let! msg, mopt = inbox.Receive()
        match msg with
        | Log l ->
          match findPath l.tags with
          | Some path ->
            let! state' = createMsg path l.message l.timestamp |> doWrite state
            return! running state'
          | None -> return! running state
        | Measure ({ m_value = v ; m_path  = p ; m_timestamp = ts } as ms) ->
          let! state' = createMsg p (getValueStr ms) ts |> doWrite state
          return! running state'
        | Flush chan ->
          chan.Reply Ack
          return! running state
        | ShutdownTarget ackChan ->
          return! shutdown state ackChan }

    and shutdown state ackChan =
      async {
        state.sendRecvStream |> tryDispose
        try (state.client :> IDisposable).Dispose() with _ -> ()
        ackChan.Reply Ack
        return () }

    init ())

/// Create a new graphite target configuration.
let create conf = TargetUtils.stdNamedTarget (graphiteLoop conf)

/// Create a new graphite target configuration.
[<CompiledName("Create")>]
let CreateC(conf, name)  = create conf name

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  /// Specify where to connect
  member x.ConnectTo(hostname, port) =
    ! (callParent <| Builder(GraphiteConf.Create(hostname, port), callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(GraphiteConf.Create(""), callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
