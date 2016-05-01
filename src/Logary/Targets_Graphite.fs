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
open Logary.Target

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

module internal Impl =

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

  let formatMeasure = Units.formatValue << function
    | Gauge (v, _)
    | Derived (v, _) ->
      v

    | Event template ->
      Int64 0L

  /// All graphite messages are of the following form.
  /// metric_path value timestamp\n
  let createMsg (path : String) value (timestamp : Instant) =
    let line = String.Format("{0} {1} {2}\n", path, value, timestamp.Ticks / NodaConstants.TicksPerSecond)
    UTF8.bytes line

  let doWrite state buffer =
    job {
      let stream =
        match state.sendRecvStream with
        | None   -> state.client.GetStream()
        | Some s -> s

      do! stream.AsyncWrite buffer
      return { state with sendRecvStream = Some stream }
    }

  let loop (conf : GraphiteConf) (svc : RuntimeInfo) (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =
    let rec running state : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          state.sendRecvStream |> tryDispose
          try (state.client :> IDisposable).Dispose() with _ -> ()
          ack *<= () :> Job<_>

        RingBuffer.take requests ^=> function
          | Log (logMsg, ack) ->
            match logMsg.value with
            | Event template ->
              job {
                let path = PointName.format logMsg.name
                let instant = Instant logMsg.timestampTicks
                let graphiteMsg = createMsg path template instant
                let! state' = graphiteMsg |> doWrite state
                do! ack *<= ()
                return! running state'
              }

            | Gauge _
            | Derived _ ->
              job {
                let path = sanitisePath logMsg.name
                let instant = Instant logMsg.timestampTicks
                let pointName = PointName.format path
                let bs = createMsg pointName (formatMeasure logMsg.value) instant
                let! state' = bs |> doWrite state
                return! running state'
              }
          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! running state
            }
      ] :> Job<_>

    running { client = new TcpClient(conf.hostname, int conf.port); sendRecvStream = None }

/// Create a new graphite target configuration.
[<CompiledName "Create">]
let create conf name = TargetUtils.stdNamedTarget (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  /// Specify where to connect
  member x.ConnectTo(hostname, port) =
    ! (callParent <| Builder(GraphiteConf.create(hostname, port), callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(GraphiteConf.create(""), callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
