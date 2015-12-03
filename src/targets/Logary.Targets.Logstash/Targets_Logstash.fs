/// A Logary target for LogStash.
module Logary.Targets.Logstash

#nowarn "1104"

open Hopac
open NodaTime
open System
open System.Net
open System.Net.Sockets
open System.IO
open Logary
open Logary.Formatting
open Logary.Target
open Logary.Internals
open Logary.Internals.Tcp
open Logary.Internals.Date

/// Logstash configuration structure.
type LogstashConf =
  { hostname     : string
    port         : uint16
    clientFac    : string -> uint16 -> WriteClient
    evtVer       : EventVersion }
  /// Create a new logstash configuration structure, optionally specifying
  /// overrides on port, client tcp factory, the formatter to log with
  /// and what event versioning scheme to use when writing to log stash
  /// (version 0 is for logstash version 1.1.x and version 1 is for logstash
  /// version 1.2.x and above)
  static member Create(hostname, ?port, ?clientFac, ?jsonSettings, ?evtVer) =
    let port = defaultArg port 1936us
    let clientFac = defaultArg clientFac (fun host port -> new TcpWriteClient(new TcpClient(host, int port)) :> WriteClient)
    let jss = defaultArg jsonSettings JsonFormatter.Default
    let evtVer = defaultArg evtVer One
    { hostname     = hostname
      port         = port
      clientFac    = clientFac
      evtVer       = evtVer }

/// What version of events to output (zero is the oldest version, one the newer)
and EventVersion =
  | Zero
  | One

module internal Impl =
  type LogstashState =
    { client         : WriteClient
      sendRecvStream : WriteStream option }

  let (=>) k v = k, v

    (* Event version 1:
       https://logstash.jira.com/browse/LOGSTASH-675
  {
    "@timestamp": "2012-12-18T01:01:46.092538Z".
    "@version": 1,
    "tags": [ "kernel", "dmesg" ]
    "type": "syslog"
    "message": "usb 3-1.2: USB disconnect, device number 4",
    "path": "/var/log/messages",
    "host": "pork.home"
  }
    Required: @timestamp, @version, message. Nothing else.
    https://github.com/logstash/logstash/blob/master/lib/logstash/codecs/json_lines.rb#L36
  *)
  /// Logstash event v1
  type Event =
    { /// @timestamp is the ISO8601 high-precision timestamp for the event.
      ``@timestamp`` : Instant
      /// @version is always 1 so far.
      ``@version``   : int
      /// tags is the event tags (array of strings)
      tags           : string list
      /// message is the human-readable text message of the event
      message        : string
      /// path is from where in the logger structure that the event comes from,
      /// see LogLine.path
      path           : string
      /// the level of the log line
      level          : string
      /// the host that the log entry comes from, we're using the hostname here.
      hostname       : string
      /// an optional exception
      ``exception``  : exn option }

    static member FromLogLine (l : LogLine) =
      { ``@timestamp`` = l.timestamp
        ``@version``   = 1
        tags           = l.tags
        message        = l.message
        path           = l.path
        hostname       = Dns.GetHostName()
        level          = l.level.ToString()
        ``exception``  = l.``exception`` }

    member x.Serialise ser jobj =
      // bug in logstash: json codec not accepted from tcp input,
      // json_lines barfs on @timestamp name, no error log
      [ yield "timestamp" => box x.``@timestamp``
        yield "@version"  => box x.``@version``
        yield "tags"      => box (List.toArray x.tags)
        yield "message"   => box x.message
        yield "path"      => box x.path
        yield "hostname"  => box x.hostname
        yield "level"     => box x.level
        match x.``exception`` with
        | Some e -> yield "exception"  => box e
        | _      -> () ]
      |> List.iter (fun (k, v) -> jobj.[k] <- Linq.JToken.FromObject(v, ser))
      jobj

  let utf8 = System.Text.Encoding.UTF8

  let maybeDispose ilogger =
    Option.map box
    >> Option.iter (function
      | :? IDisposable as d ->
        Try.safe "disposing in logstash target" ilogger <| fun () ->
          d.Dispose()
      | _ -> ())

  /// All logstash messages are of the following form.
  /// json-event\n
  let createMsg evtVer (jss : JsonSerializerSettings) serviceName (logLine : LogLine) =
    let ser = JsonSerializer.Create jss
    let fbox (f : LogLine -> 'a) = fun l -> f l :> NewtonsoftSerialisable
    let mkEvent = function
      | Zero -> fbox EventV0.FromLogLine
      | One  -> fbox Event.FromLogLine
    let evt = mkEvent evtVer logLine

    JObject()
    |> evt.Serialise ser
    |> fun jobj ->
      for pair in logLine.data do
        jobj.[pair.Key] <- JToken.FromObject(pair.Value, ser)
      jobj.["service"] <- JToken.FromObject(serviceName, ser)
      let line = String.Format("{0}\n", jobj.ToString(Formatting.None))
      utf8.GetBytes line

  let doWrite debug state m =
    async {
      let stream =
        match state.sendRecvStream with
        | None   ->
          debug "send-recv stream not open, opening"
          state.client.GetStream()
        | Some s -> s
      do! stream.Write( m )
      return { state with sendRecvStream = Some stream } }

  let logstashLoop (conf : LogstashConf) metadata =
    let ll level =
      LogLine.create'' "Logary.Targets.Logstash.logstashLoop"
      >> LogLine.setLevel level
    let debug, info =
      let log = Logger.log metadata.logger in
      ll Debug >> log, ll Info  >> log

    (fun (inbox : IActor<_>) ->
      let rec init () =
        async { 
          info (sprintf "initing logstash target, connecting to %s:%d..." conf.hostname conf.port)
          let client = conf.clientFac conf.hostname conf.port
          info "initing logstash target, connected"
          return! running { client = client; sendRecvStream = None } }

      and running state =
        async {
          let! msg, mopt = inbox.Receive()
          match msg with
          | Log line ->
            let! state' =
              line
              |> createMsg conf.evtVer conf.jsonSettings metadata.serviceName
              |> doWrite debug state
            return! running state'
          | Measure _ ->
            return! running state
          | Flush chan ->
            chan.Reply Ack
            return! running state
          | Shutdown ackChan ->
            return! shutdown state ackChan }

      and shutdown state ackChan =
        async {
          state.sendRecvStream |> maybeDispose metadata.logger
          Try.safe "logstash target disposing tcp client" metadata.logger <| fun () ->
            (state.client :> IDisposable).Dispose()
          ackChan.Reply Ack
          return () }
      init ())

let create conf = TargetUtils.stdNamedTarget (Impl.logstashLoop conf)

[<CompiledName("Create")>]
let create'(conf, name) = create conf name

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  member x.Hostname(hostname : string) =
    Builder({ conf with LogstashConf.hostname = hostname }, callParent)

  member x.Port(port : uint16) =
    Builder({ conf with port = port }, callParent)

  member x.ClientFactory(fac : Func<string, uint16, WriteClient>) =
    Builder({ conf with clientFac = fun host port -> fac.Invoke(host, port) }, callParent)

  /// Sets the JsonSerializerSettings to use, or uses
  /// <see cref="Logary.Formatting.JsonFormatter.Settings" /> otherwise.
  member x.JsonSerializerSettings(settings : JsonSerializerSettings) =
    Builder({ conf with jsonSettings = settings }, callParent)

  member x.EventVersion(ver : EventVersion) =
    Builder({ conf with evtVer = ver }, callParent)

  member x.Done() =
    ! ( callParent x )

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(LogstashConf.Create("127.0.0.1"), callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
