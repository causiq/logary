/// A Logary target for Logstash that logs to its ZeroMQ input. See
/// https://www.elastic.co/guide/en/logstash/current/plugins-inputs-zeromq.html
/// for information about how to configure it.
module Logary.Targets.Logstash

#nowarn "1104"

open Hopac
open System
open Hopac
open Hopac.Infixes
open fszmq
open fszmq.Socket
open Logary
open Logary.Internals
open Logary.Configuration

let serialise =
  Logary.Formatting.Json.format


/// This is the default address this Target publishes messages to.
[<Literal>]
let DefaultPublishTo =
  "tcp://127.0.0.1:2120"

type LogstashMode = PUSHPULL | PUBSUB

type LogstashConf =
  { publishTo: string
    logMetrics: bool
    mode: LogstashMode }

  /// Create a new Logstash target config.
  static member create(?publishTo, ?logMetrics, ?mode) =
    { publishTo  = defaultArg publishTo DefaultPublishTo
      logMetrics = defaultArg logMetrics false
      mode       = defaultArg mode PUSHPULL }

module internal Impl =

  type State =
    { zmqCtx: Context
      sender: Socket }
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

  let createState publishTo mode: State =
    let context = new Context()
    let sender = createSender context publishTo mode
    { zmqCtx = context
      sender = sender }

  let loop (conf: LogstashConf) (api: TargetAPI): Job<unit> =

    let rec init config =
      createState config.publishTo config.mode |> loop

    and loop (state: State): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack -> job {
          do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          do! ack *<= ()
        }

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            job {
              // https://gist.github.com/jordansissel/2996677
              let bytes =
                message
                |> serialise
                |> System.Text.Encoding.UTF8.GetBytes

              do! Job.Scheduler.isolate (fun _ -> state.sender <~| (System.Text.Encoding.UTF8.GetBytes (message.name.ToString())) <<|  bytes)

              do! ack *<= ()
              return! loop state
            }

          | Flush (ackCh, nack) ->
            job {
              do! IVar.fill ackCh ()
              return! loop state
            }
      ] :> Job<_>

    init conf

let create conf = TargetConf.createSimple (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<Logstash.Builder>() )
type Builder(conf, callParent: Target.ParentCallback<Builder>) =

  /// Specifies the publish endpoint that ZeroMQ connects to.
  member x.PublishTo(publishTo: string) =
    Builder({ conf with publishTo = publishTo }, callParent)

  member x.LogMetrics() =
    Builder({ conf with logMetrics = true }, callParent)

  member x.Done() =
    ! (callParent x)

  new(callParent: Target.ParentCallback<_>) =
    Builder(LogstashConf.create DefaultPublishTo, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name =
      create conf name