#if INTERACTIVE
#r "bin/Release/protobuf-net.dll"
#else
module Logary.Target.Riemann
#endif

// https://github.com/aphyr/riemann-ruby-client/blob/master/lib/riemann/event.rb
// https://github.com/aphyr/riemann-java-client/tree/master/src/main/java/com

open ProtoBuf

open FSharp.Actor

open NodaTime

open System
open System.Collections.Generic
open System.ComponentModel
open System.IO
open System.Net
open System.Net.Sockets
open System.Net.Security
open System.Security.Cryptography.X509Certificates

open Logary
open Logary.Targets
open Logary.Internals.Tcp
open Logary.Internals.InternalLogger

// # https://github.com/aphyr/riemann-java-client/commit/81dd51349cfcd4daf6b9ff80aa59f4aecbcc078c

// Because protocol buffers is strongly typed, the metric of an event is
// represented as one of metric_d (floating point 64-bit), metric_f (floating
// point 32-bit), or metric_sint64 (64-bit signed integer). Your client should
// emit and consume all of these types.

// TODO: You can also query events from the index using a basic query language.
// Just submit a Message with your query in message.query.string. Search queries
// will return a message with repeated Events matching that expression. A null
// expression will return no states. For some example queries, see The query
// test suite.

[<CLIMutable; ProtoContract>]
type State =
    /// Event.time is the time in unix epoch seconds and is optional
    /// The server will generate a time for each event when received if you do
    /// not provide one.
  { [<ProtoMember 1; DefaultValue 0L>] time               : int64
    [<ProtoMember 2; DefaultValue "">] state              : string
    /// Events are uniquely identified by host and service. Both allow null.
    [<ProtoMember 3; DefaultValue "">] service            : string
    /// Events are uniquely identified by host and service. Both allow null.
    [<ProtoMember 4; DefaultValue "">] host               : string
    [<ProtoMember 5; DefaultValue "">] description        : string
    [<ProtoMember 6; DefaultValue false>] once            : bool
    [<ProtoMember 7>]                     tags            : string List
    [<ProtoMember 8; DefaultValue 0.>] ttl                : single }

  override x.ToString() =
    sprintf "State(time=%d, state=%s, service=%s, host=%s, description=%s, once=%b, tags=%A, ttl=%f)"
      x.time x.state x.service x.host x.description x.once x.tags x.ttl

[<CLIMutable; ProtoContract>]
type Attribute =
  { [<ProtoMember 1; DefaultValue "">] key : string
    [<ProtoMember 2; DefaultValue "">] value : string }

[<ProtoContract>]
type Event =
  [<ProtoMember(1, IsRequired=false); DefaultValue 0L>]
  val mutable time : int64 Nullable

  [<ProtoMember(2, IsRequired=false); DefaultValue "">]
  val mutable state : string

  [<ProtoMember(3, IsRequired=false); DefaultValue "">]
  val mutable service : string

  [<ProtoMember(4, IsRequired=false); DefaultValue "">]
  val mutable host : string

  [<ProtoMember(5, IsRequired=false); DefaultValue "">]
  val mutable description : string

  [<ProtoMember(7)>]
  val mutable tags : string List

  [<ProtoMember(8, IsRequired=false); DefaultValue null>]
  val mutable ttl : single Nullable // F# single = proto float

  [<ProtoMember 9>]
  val mutable attributes : Attribute List

  [<ProtoMember(13, IsRequired=false); DefaultValue(0L)>]
  val mutable metric_sint64 : int64 Nullable // F# int64 = proto signed int64

  [<ProtoMember(14, IsRequired=false); DefaultValue(Double.NaN)>]
  val mutable metric_d : float Nullable // F# float = proto double
  
  [<ProtoMember(15, IsRequired=false); DefaultValue(Single.NaN)>]
  val mutable metric_f : single Nullable

  new () =
    { time = Nullable(SystemClock.Instance.Now.Ticks / NodaConstants.TicksPerSecond)
      state = ""
      service = ""
      host = ""
      description = ""
      tags = List<_>()
      ttl = Nullable()
      attributes = List<_>()
      metric_sint64 = Nullable()
      metric_d = Nullable()
      metric_f = Nullable() }

  new (time : int64, ?state, ?service, ?host, ?description, ?tags, ?ttl, ?attributes, ?metric_sint64, ?metric_d, ?metric_f) =
    { time = Nullable(time)
      state = defaultArg state ""
      service = defaultArg service ""
      host = defaultArg host ""
      description = defaultArg description ""
      tags = defaultArg tags (List<_>())
      ttl = defaultArg ttl (Nullable())
      attributes = defaultArg attributes (List<_>())
      metric_sint64 = defaultArg metric_sint64 (Nullable())
      metric_d = defaultArg metric_d (Nullable())
      metric_f = defaultArg metric_f (Nullable()) }

  override x.ToString() =
    sprintf "Event(time=%O, state=%s, service=%s, host=%s, description=%s, tags=%A, ttl=%O, attributes=%A, metric_sint64=%O, metric_d=%O, metric_f=%O)"
      x.time x.state x.service x.host x.description x.tags x.ttl x.attributes
      x.metric_sint64 x.metric_d x.metric_f

[<ProtoContract>]
type Query =
  [<ProtoMember 1; DefaultValue "">]
  val mutable ``string`` : string

  new () = { ``string`` = "" }
  new str = { ``string`` = str }

  override x.ToString() =
    sprintf "Query(%s)" x.string

[<ProtoContract>]
type Msg =
  [<ProtoMember(2, IsRequired = false); DefaultValue false>]
  val mutable ok     : bool
  [<ProtoMember(3, IsRequired = false); DefaultValue "">]
  val mutable error  : string
  [<ProtoMember(4)>]
  val mutable states : State List
  [<ProtoMember(5, IsRequired = false)>]
  val mutable query  : Query
  [<ProtoMember(6)>]
  val mutable events : Event List

  new () =
    { ok     = false
      error  = ""
      states = List<_>()
      query  = Query()
      events = List<_>() }

  new (ok, error, states, query, events) =
    { ok     = ok
      error  = error
      states = states
      query  = query
      events = events }

  override x.ToString() =
    sprintf "Msg(ok=%b, error=%s, states=%A, query=%O, events=%A)"
      x.ok x.error x.states x.query x.events

let private tcpClientFromEp (f_ca : _ option) (ep : IPEndPoint) =
  let c = new TcpClient(ep)
  match f_ca with
  | Some f -> new TLSWriteClient(c, f) :> WriteClient
  | None ->   new TcpWriteClient(c) :> WriteClient

/// The Riemann target will always use TCP in this version.
type RiemannConf =
  { endpoint     : IPEndPoint
    /// A factory function for the WriteClient - useful for testing the target
    /// and for replacing the client with a high-performance client if the async
    /// actor + async + TcpClient isn't enough, but you want to try the async
    /// socket pattern.
    clientFac    : IPEndPoint -> WriteClient
    /// validation function; setting this means you need to be able to validate
    /// the certificate you get back when connecting to Riemann -- if you set
    /// this value the target will try and create a TLS connection.
    ///
    /// Parameters:
    ///  - X509Certificate certificate
    ///  - X509Chain chain
    ///  - SslPolicyErrors sslPolicyErrors
    ///
    /// Returns: bool indicating whether to accept the cert or not
    caValidation : (X509Certificate -> X509Chain -> SslPolicyErrors -> bool) option
    /// An optional mapping function that can change the Event that is generated by
    /// default.
    fLogLine     : (LogLine -> Event -> Event) option

    /// An optional mapping function that can change the Event that is generated by
    /// default.
    fMeasure     : (Measure -> Event -> Event) option }
  /// Creates a new Riemann target configuration
  static member Create(endpoint : IPEndPoint, ?clientFac) =
    let clientFac = defaultArg clientFac (tcpClientFromEp None)
    { endpoint     = endpoint
      clientFac    = clientFac
      caValidation = None
      fLogLine     = None
      fMeasure     = None }

type private RiemannTargetState =
  { client         : WriteClient
    sendRecvStream : WriteStream option }

let private maybeDispose =
  Option.map box
  >> Option.iter (function
    | :? IDisposable as d ->
      safeTry "disposing in riemann target" <| fun () ->
        d.Dispose()
    | _ -> ())

let private ensureStream state : (_ * WriteStream) =
  let stream =
    match state.sendRecvStream with
    | None   -> state.client.GetStream()
    | Some s -> s
  { state with sendRecvStream = Some stream }, stream

let private send (client : WriteClient) state (evt : 'a) =
  /// A TCP connection to Riemann is a stream of messages. Each message is a
  /// 4 byte network-endian integer *length*, followed by a Protocol Buffer
  /// Message of *length* bytes. See the protocol buffer definition for the details.
  async {
    let state', stream = ensureStream state
    use ms = new MemoryStream()
    do Serializer.Serialize<'a>(ms, evt)
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    do! stream.Write(ms.ToArray())
    return state' }

/// Convert the LogLevel to a Riemann (service) state
let mkState = function
  | Verbose | Debug | Info -> "info"
  | Warn | Error           -> "warn"
  | Fatal                  -> "critical"

/// Create an Event from a LogLine
let mkEventL
  fLogLine
  (ttl : float)
  ({ message      = message
  ; data          = data
  ; level         = level
  ; tags          = tags
  ; timestamp     = timestamp
  ; path          = path
  ; ``exception`` = ex } as ll) =

  new Event(timestamp.Ticks / NodaConstants.TicksPerSecond,
            mkState level,
            path,
            Dns.GetHostName(),
            message,
            List<_>(tags),
            Nullable(single ttl),
            List<_>(), // TODO: attributes, all from map
            Nullable(1L))
  |> fun e -> Option.fold (fun evt fLL -> fLL ll evt) e fLogLine

/// Create an Event from a Measure
let mkEventM fMeasure ttl
  { value     = value
    path      = path
    timestamp = timestamp
    level     = level
    mtype     = mtype } =
  match mtype with
  | Gauge   -> failwith "not impl" // TODO: example "RegisterGauge"
  | Timer t -> failwith "not impl"
  | Counter -> failwith "not impl"

// TODO: could be useful to spawn multiple of this one: each is async and implement
// an easy way to send/recv -- multiple will allow interleaving of said requests
let riemannLoop (conf : RiemannConf) metadata =
  (fun (inbox : IActor<_>) ->
    let rec init () =
      async { 
        let client = conf.clientFac conf.endpoint
        return! running { client = client; sendRecvStream = None } }

    and running state =
      async {
        let! msg, mopt = inbox.Receive()
        // TODO: The server will accept a repeated list of Events, and respond
        // with a confirmation message with either an acknowledgement or an error.
        // Check the `ok` boolean in the message; if false, message.error will
        // be a descriptive string.
        match msg with
        | Log l ->
          let! state' = l |> mkEventL conf.fLogLine 30. |> send state.client state
          // todo: recv
          return! running state'
        | Metric msr ->
          // So currently we're in push mode; did a Guage, Histogram or other thing send
          // us this metric? Or are Logary 'more dump' and simply shovel the more simple
          // counters and measurements (e.g. function execution timing) to Riemann
          // so that riemann can make up its own data?
          //
          // See https://github.com/aphyr/riemann-java-client/blob/master/src/main/java/com/codahale/metrics/riemann/RiemannReporter.java#L282
          let! state' = msr |> mkEventM conf.fMeasure 30. |> send state.client state
          // todo: recv
          return! running state'
        | Flush chan ->
          chan.Reply Ack
          return! running state
        | ShutdownTarget ackChan ->
          return! shutdown state ackChan }

    and shutdown state ackChan =
      async {
        state.sendRecvStream |> maybeDispose
        safeTry "riemann target disposing tcp client" <| fun () ->
          (state.client :> IDisposable).Dispose()
        ackChan.Reply Ack
        return () }
    init ())

/// Create a new Riemann target
let create conf = TargetUtils.stdNamedTarget (riemannLoop conf)

[<CompiledName("Create")>]
let CreateC(conf, name) = create conf name

//  /// Use with LogaryFactory.New( s => s.Target<Riemann.Builder>() )
//  type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
//
//    member x.Hostname(hostname : string) =
//      Builder({ conf with LogstashConf.hostname = hostname }, callParent)
//
//    member x.Port(port : uint16) =
//      Builder({ conf with port = port }, callParent)
//
//    member x.ClientFactory(fac : Func<string, uint16, WriteClient>) =
//      Builder({ conf with clientFac = fun host port -> fac.Invoke(host, port) }, callParent)
//
//    /// Sets the JsonSerializerSettings to use, or uses
//    /// <see cref="Logary.Formatting.JsonFormatter.Settings" /> otherwise.
//    member x.JsonSerializerSettings(settings : JsonSerializerSettings) =
//      Builder({ conf with jsonSettings = settings }, callParent)
//
//    member x.EventVersion(ver : EventVersion) =
//      Builder({ conf with evtVer = ver }, callParent)
//
//    member x.Done() =
//      ! ( callParent x )
//
//    new(callParent : FactoryApi.ParentCallback<_>) =
//      Builder(LogstashConf.Create("127.0.0.1"), callParent)
//
//    interface Logary.Targets.FactoryApi.SpecificTargetConf with
//      member x.Build name = create conf name
