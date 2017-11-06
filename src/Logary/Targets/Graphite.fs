/// A Logary target for Graphite, which is a plotting/graphing
/// server.
module Logary.Targets.Graphite

open Hopac
open Hopac.Infixes
open NodaTime
open System
open System.Runtime.CompilerServices
open System.Net.Sockets
open System.Text.RegularExpressions
open Logary
open Logary.Internals
open Logary.Configuration.Target

/// Put this tag on your message (message must be a parseable value)
/// if you want graphite to find it, or use the Metrics API of Logary.
[<Literal>]
let TriggerTag = "plottable"

/// Configuration for loggin to graphite.
/// TODO: prefixing with hostname etc
type GraphiteConf =
  { hostname  : string
    port      : uint16 }

  [<CompiledName "Create">]
  static member create(hostname, ?port) =
    let port = defaultArg port 2003us
    { hostname = hostname
      port     = port }

module Impl =

  type GraphiteState =
    { client         : TcpClient
      sendRecvStream : System.Net.Sockets.NetworkStream option }

  let tryDispose (item : 'a option) =
    if item.IsNone then
      ()
    else
      match item.Value |> box with
      | :? IDisposable as disposable ->
        try disposable.Dispose() with _ -> ()
      | _ -> ()

  // Allowable characters in a graphite metric name:
  // alphanumeric
  // !#$%&"'*+-:;<=>?@[]\^_`|~
  // . is used as a path separator
  let invalidPathCharacters =
    Regex("""[^a-zA-Z0-9!#\$%&"'\*\+\-:;<=>\?@\[\\\]\^_`\|~]""", RegexOptions.Compiled)

  /// Sanitises Graphite metric paths by converting / to - and replacing all other
  /// invalid characters with underscores.
  let sanitisePath (PointName segments) =
    segments
    |> Array.map (fun x -> x.Replace("/", "-"))
    |> Array.map (fun x -> invalidPathCharacters.Replace(x, "_"))
    |> PointName.ofArray

  /// All graphite messages are of the following form.
  /// metric_path value timestamp\n
  let createMsg (path : String) (timestamp : Instant) (value : string) =
    let line = String.Format("{0} {1} {2}\n", path, value, timestamp.Ticks / NodaConstants.TicksPerSecond)
    UTF8.bytes line

  let doWrite state buffer =
    let stream =
      match state.sendRecvStream with
      | None   -> state.client.GetStream()
      | Some s -> s

    Job.fromAsync (stream.AsyncWrite buffer) >>-.
    { state with sendRecvStream = Some stream }

  let loop (conf : GraphiteConf) (svc : RuntimeInfo, api : TargetAPI) =

    let rec running state : Job<unit> =
      Alt.choose [
        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            let pointName = sanitisePath message.name |> PointName.format
            message 
            |> MessageWriter.verbatim.format 
            |> createMsg pointName (Instant message.timestampTicks) 
            |> doWrite state >>= fun state' ->
            IVar.fill ack () >>= fun () ->
            running state'

          | Flush (ack, nack) ->
            IVar.fill ack () >>= fun _ -> running state

        api.shutdownCh ^=> fun ack ->
          state.sendRecvStream |> tryDispose
          try (state.client :> IDisposable).Dispose() with _ -> ()
          ack *<= ()

      ] :> Job<_>

    { client = new TcpClient(conf.hostname, int conf.port)
      sendRecvStream = None }
    |> running 

/// Create a new graphite target configuration.
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : ParentCallback<Builder>) =

  /// Specify where to connect
  member x.ConnectTo(hostname, port) =
    ! (callParent <| Builder(GraphiteConf.create(hostname, port), callParent))

  new(callParent : ParentCallback<_>) =
    Builder(GraphiteConf.create(""), callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
