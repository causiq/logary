/// A Logary target for LogStash.
module Logary.Target.Logstash

#nowarn "1104"

open FSharp.Actor

open Newtonsoft.Json

open NodaTime

open System
open System.Net
open System.Net.Sockets
open System.IO

open Logary
open Formatting
open Targets

open Logary.Internals.Tcp
open Logary.Internals.Date
open Logary.Internals.InternalLogger

/// Logstash configuration structure.
type LogstashConf =
  { hostname     : string
  ; port         : uint16
  ; clientFac    : string -> uint16 -> WriteClient
  ; jsonSettings : JsonSerializerSettings
  ; evtVer       : EventVersion }
  /// Create a new logstash configuration structure, optionally specifying
  /// overrides on port, client tcp factory, the formatter to log with
  /// and what event versioning scheme to use when writing to log stash
  /// (version 0 is for logstash version 1.1.x and version 1 is for logstash
  /// version 1.2.x and above)
  static member Create(hostname, ?port, ?clientFac, ?jsonSettings, ?evtVer) =
    let port = defaultArg port 1936us
    let clientFac = defaultArg clientFac (fun host port -> new TcpWriteClient(new TcpClient(host, int port)) :> WriteClient)
    let jss = defaultArg jsonSettings (JsonFormatter.Settings())
    let evtVer = defaultArg evtVer One
    { hostname     = hostname
    ; port         = port
    ; clientFac    = clientFac
    ; jsonSettings = jss
    ; evtVer       = evtVer }
/// What version of events to output (zero is the oldest version, one the newer)
and EventVersion =
  | Zero
  | One

type private LogstashState =
  { client         : WriteClient
  ; sendRecvStream : WriteStream option }

let private (=>) k v = k, v
type private NewtonsoftSerialisable =
  abstract Serialise : JsonSerializer -> Linq.JObject -> Linq.JObject

  (* Event version 0:
https://gist.github.com/jordansissel/2996677:
{
"@source":"unknown",
"@type":null,"
  @tags":[],
"@fields":{},
"@message":"Hello world",
"@timestamp":"2012-06-26T15:58:20.135353Z"
}
  *)

/// Logstash event version v0
type private EventV0 =
  { ``@source``    : string
  ; ``@tags``      : string list
  ; ``@fields``    : Map<string, obj>
  ; ``@message``   : string
  ; ``@timestamp`` : Instant }
  /// Create an EventV0 from the log line passed as a parameter.
  static member FromLogLine (l : LogLine) =
    { ``@source``    = Dns.GetHostName()
    ; ``@tags``      = l.tags
    ; ``@fields``    = l.data
    ; ``@message``   = l.message
    ; ``@timestamp`` = l.timestamp }

  interface NewtonsoftSerialisable with
    member x.Serialise ser jobj =
      [ "@source"    => box x.``@source``
      ; "@tags"      => box (List.toArray x.``@tags``)
      ; "@fields"    => box (x.``@fields``)
      ; "@message"   => box x.``@message``
      ; "@timestamp" => box x.``@timestamp`` ]
      |> List.iter (fun (k,v) -> jobj.[k] <- Linq.JToken.FromObject(v, ser))
      jobj

  (* Event version 1:
https://logstash.jira.com/browse/LOGSTASH-675:
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
type private Event =
  /// @timestamp is the ISO8601 high-precision timestamp for the event.
  { ``@timestamp`` : Instant
  /// @version is always 1 so far.
  ; ``@version``   : int
  /// tags is the event tags (array of strings)
  ; tags           : string list
  /// message is the human-readable text message of the event
  ; message        : string
  /// path is from where in the logger structure that the event comes from,
  /// see LogLine.path
  ; path           : string
  /// the level of the log line
  ; level          : string
  /// the host that the log entry comes from, we're using the hostname here.
  ; hostname       : string
  /// an optional exception
  ; ``exception``  : exn option }
  static member FromLogLine (l : LogLine) =
    { ``@timestamp`` = l.timestamp
    ; ``@version``   = 1
    ; tags           = l.tags
    ; message        = l.message
    ; path           = l.path
    ; hostname       = Dns.GetHostName()
    ; level          = l.level.ToString()
    ; ``exception``  = l.``exception`` }

  interface NewtonsoftSerialisable with
    member x.Serialise ser jobj =
      // bug in logstash: json codec not accepted from tcp input,
      // json_lines barfs on @timestamp name, no error log
      [ yield "timestamp" => box x.``@timestamp``
        yield "@version"   => box x.``@version``
        yield "tags"       => box (List.toArray x.tags)
        yield "message"    => box x.message
        yield "path"       => box x.path
        yield "hostname"   => box x.hostname
        yield "level"      => box x.level
        match x.``exception`` with
        | Some e -> yield "exception"  => box e
        | _      -> () ]
      |> List.iter (fun (k, v) -> jobj.[k] <- Linq.JToken.FromObject(v, ser))
      jobj

let private utf8 = System.Text.Encoding.UTF8

let private maybeDispose =
  Option.map box
  >> Option.iter (function
    | :? IDisposable as d ->
      safeTry "disposing in riemann target" <| fun () ->
        d.Dispose()
    | _ -> ())

open Formatting
open Newtonsoft.Json.Linq

/// All logstash messages are of the following form.
/// json-event\n
let private createMsg evtVer (jss : JsonSerializerSettings) serviceName (logLine : LogLine) =
  let jsonSettings = JsonFormatter.Settings()
  let ser = JsonSerializer.Create jsonSettings
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

let private doWrite state m =
  async {
    let stream =
      match state.sendRecvStream with
      | None   -> state.client.GetStream()
      | Some s -> s
    do! stream.Write( m )
    return { state with sendRecvStream = Some stream } }

let logstashLoop (conf : LogstashConf) metadata =
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
          let! state' = l |> createMsg conf.evtVer conf.jsonSettings metadata.serviceName |> doWrite state
          return! running state'
        | Metric _ ->
          return! running state
        | Flush chan ->
          chan.Reply Ack
          return! running state
        | ShutdownTarget ackChan ->
          return! shutdown state ackChan }

    and shutdown state ackChan =
      async {
        state.sendRecvStream |> maybeDispose
        safeTry "logstash target disposing tcp client" <| fun () ->
          (state.client :> IDisposable).Dispose()
        ackChan.Reply Ack
        return () }
    init ())

let create conf = TargetUtils.stdNamedTarget (logstashLoop conf)

[<CompiledName("Create")>]
let CreateC(conf, name) = create conf name

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

  interface Logary.Targets.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
