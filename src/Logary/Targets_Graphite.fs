/// A Logary target for Graphite, which is a plotting/graphing
/// server.
module Logary.Targets.Graphite

open Hopac

open NodaTime

open System
open System.Net
open System.Net.Sockets
open System.Globalization

open Logary
open Logary.Internals
open Logary.Target
open Logary.Internals.Tcp
open Logary.DataModel

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

module internal Impl =

  type GraphiteState =
    { client         : WriteClient
      sendRecvStream : WriteStream option }

  let tryDispose (item : 'a option) =
    if item.IsNone then ()
    else match item.Value |> box with
          | :? IDisposable as disposable -> try disposable.Dispose() with _ -> ()
          | _ -> ()

  // Allowable characters in a graphite metric name:
  // alphanumeric
  // !#$%&"'*+-:;<=>?@[]\^_`|~
  // . is used as a path separator
  let invalidPathCharacters =
    System.Text.RegularExpressions.Regex("""[^a-zA-Z0-9!#\$%&"'\*\+\-:;<=>\?@\[\\\]\^_`\|~]""", Text.RegularExpressions.RegexOptions.Compiled)//"

  /// Sanitizes Graphite metric paths by converting / to - and replacing all other
  /// invalid characters with underscores.
  let sanitizePath (paths: string list) =
    paths
    |> Seq.map (fun r -> r.Replace("/", "-"))
    |> Seq.map (fun r -> invalidPathCharacters.Replace(r, "_"))
    |> List.ofSeq

  let formatMeasure = Units.formatValue << function
    | Gauge (v, _)
    | Derived (v, _) -> v
    | Event template -> Int64 0L

  /// All graphite messages are of the following form.
  /// metric_path value timestamp\n
  let createMsg path value (timestamp : Instant) =
    let line = String.Format("{0} {1} {2}\n", path, value, timestamp.Ticks / NodaConstants.TicksPerSecond)
    UTF8.bytes line

  (*let private findPath (tags : string list) =
    let tags' = tags |> List.fold (fun s t -> s |> Set.add t) Set.empty
    if not (tags' |> Set.contains TriggerTag) then
      None
    else
      tags |> List.tryFind (fun t -> t.StartsWith("path"))*)

  let doWrite state m =
    job {
      let stream =
        match state.sendRecvStream with
        | None   -> state.client.GetStream()
        | Some s -> s
      do! stream.Write m
      return { state with sendRecvStream = Some stream } }

  let loop (conf : GraphiteConf) (svc : RuntimeInfo) =
    (fun (reqCh : Ch<_>) ->
      let shutdown state ack = job {
        state.sendRecvStream |> tryDispose
        try (state.client :> IDisposable).Dispose() with _ -> ()
        do! IVar.fill ack Ack
        return ()
      }

      let rec running state = job {
        let! msg = Ch.take reqCh
        match msg with
        | Log logMsg ->
          match logMsg.value with
          | Event message ->
            let! state' = createMsg (Message.Context.serviceGet logMsg) message (Instant logMsg.timestamp) |> doWrite state
            return! running state'
          | Gauge _ | Derived _ ->
            let! state' =
              createMsg (sanitizePath logMsg.name
              |> PointName.joined) (formatMeasure logMsg.value) (Instant logMsg.timestamp)
              |> doWrite state
            return! running state'

        | Flush ack ->
          do! IVar.fill ack Ack
          return! running state

        | Shutdown ack ->
          return! shutdown state ack
      }

      let init () = job {
        let client = conf.clientFac conf.hostname conf.port
        return! running { client = client; sendRecvStream = None }
      }

      init ())

/// Create a new graphite target configuration.
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# interop: Create a new graphite target configuration.
[<CompiledName("Create")>]
let create' (conf, name) = create conf name

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  /// Specify where to connect
  member x.ConnectTo(hostname, port) =
    ! (callParent <| Builder(GraphiteConf.Create(hostname, port), callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(GraphiteConf.Create(""), callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
