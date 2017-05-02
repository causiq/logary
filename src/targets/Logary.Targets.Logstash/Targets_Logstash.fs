/// A Logary target for Logstash that logs to its ZeroMQ input. See
/// https://www.elastic.co/guide/en/logstash/current/plugins-inputs-zeromq.html
/// for information about how to configure it.
module Logary.Targets.Logstash

#nowarn "1104"

open Hopac
open NodaTime
open System
open System.Net
open System.Net.Sockets
open System.IO
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open fszmq
open fszmq.Socket
open Logary
open Logary.Formatting
open Logary.Target
open Logary.Internals
open Logary.Utils.Chiron

/// This is the default address this Target publishes messages to.
[<Literal>]
let DefaultPublishTo =
  "tcp://127.0.0.1:2120"

type LogstashMode = PUSHPULL | PUBSUB

let internal applyOverrides (values:Map<string, Json>): Map<string, Json> =
  let overrides =
    [ "@version", String "1"
      "@timestamp", String (MessageParts.formatTimestamp message.timestampTicks)
      "name", String (PointName.format message.name)
    ]

  overrides |> List.fold (fun data (k, v) -> data |> Map.add k v) values

let serialise : Message -> Json =
  fun message ->
    let props =
      match Json.serialize message with
      | Object values ->
        values
      | otherwise ->
        failwithf "Expected Message to format to Object .., but was %A" otherwise
    applyOverrides values |> Object

let serialiseEventFlat : Message -> Json =
  fun message ->
    let props =
      let event =
        match message.value with
        | Event template ->
          template
        | _ ->
          ""
      Map [
        "context", Json.serialise message.context
        "level", Json.serialise message.level
        "name", Json.serialise message.name
        "timestamp", Json.serialise message.timestamp
        "event", Json.String event
      ]
      |> Json.serialise

    applyOverrides props |> Object

// LogstashConf.create(serialiser = serialiseEventFlat)

type LogstashConf =
  { publishTo  : string
    logMetrics : bool
    serialiser : Message -> Json
    mode       : LogstashMode }

  /// Create a new Logstash target config.
  static member create(?publishTo, ?logMetrics, ?mode, ?serialiser) =
    { publishTo  = defaultArg publishTo DefaultPublishTo
      logMetrics = defaultArg logMetrics false
      mode       = defaultArg mode PUSHPULL
      serialiser = defaultArg serialiser serialise }

module internal Impl =

  type State =
    { zmqCtx : Context
      sender : Socket }
    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.sender :> IDisposable).Dispose()

  let createSender context publishTo = function
    | PUSHPULL ->
      let sender = Context.push context
      Socket.connect sender publishTo
      sender
    | PUBSUB ->
      let sender = Context.sub context
      Socket.bind sender publishTo
      sender

  let createState publishTo mode : State =
    let context = new Context()
    let sender = createSender context publishTo mode
    { zmqCtx = context
      sender = sender }

  let loop (conf : LogstashConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =

    let rec init config =
      createState config.publishTo config.mode |> loop

    and loop (state : State) : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack -> job {
          do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          do! ack *<= ()
        }

        RingBuffer.take requests ^=> function
          | Log (message, ack) ->
            job {
              // https://gist.github.com/jordansissel/2996677
              let bytes =
                Json.format (conf.serialiser message)
                |> UTF8.bytes

              do! Job.Scheduler.isolate (fun _ -> state.sender <~| (UTF8.bytes (message.name.ToString())) <<|  bytes)

              do! ack *<= ()
              return! loop state
            }

          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! loop state
            }
      ] :> Job<_>

    init conf

let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<Logstash.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  /// Specifies the publish endpoint that ZeroMQ connects to.
  member x.PublishTo(publishTo : string) =
    Builder({ conf with publishTo = publishTo }, callParent)

  member x.LogMetrics() =
    Builder({ conf with logMetrics = true }, callParent)

  member x.Done() =
    ! (callParent x)

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(LogstashConf.create DefaultPublishTo, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name =
      create conf name