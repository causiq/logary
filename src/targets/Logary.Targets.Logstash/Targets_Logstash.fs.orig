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
  "tcp://127.0.0.1:5001"

type LogstashConf =
  { publishTo  : string
    logMetrics : bool }

  /// Create a new Logstash target config.
  static member create(?publishTo, ?logMetrics) =
    { publishTo  = defaultArg publishTo DefaultPublishTo
      logMetrics = defaultArg logMetrics false }

let serialise : Message -> Json =
  fun message ->
    let props =
      match Json.serialize message with
      | Object values ->
        values

      | otherwise ->
        failwithf "Expected Message to format to Object .., but was %A" otherwise

    let overrides =
      [ "@version", String "1"
        "@timestamp", String (MessageParts.formatTimestamp message.timestampTicks)
        "name", String (PointName.format message.name)
      ]

    let final =
      overrides |> List.fold (fun data (k, v) -> data |> Map.put k v) props

    (Object final)

module internal Impl =

  type State =
    { zmqCtx : Context
      sender : Socket }
    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.sender :> IDisposable).Dispose()

  let createState publishTo : State =
    let context = new Context()
    let sender = Context.pub context
    Socket.connect sender publishTo

    { zmqCtx = context
      sender = sender }

  let loop (conf : LogstashConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =

    let rec init config =
      createState config.publishTo |> loop

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
                Json.format (serialise message)
                |> UTF8.bytes

              do! Job.Scheduler.isolate (fun _ -> bytes |>> state.sender)

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