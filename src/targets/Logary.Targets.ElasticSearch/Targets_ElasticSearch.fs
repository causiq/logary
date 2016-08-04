/// A Logary target for Logstash that logs to its ZeroMQ input. See
/// https://www.elastic.co/guide/en/logstash/current/plugins-inputs-zeromq.html
/// for information about how to configure it.
module Logary.Targets.ElasticSearch

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
open Logary
open Logary.Formatting
open Logary.Target
open Logary.Internals
open Logary.Utils.Chiron

/// This is the default address this Target publishes messages to.
[<Literal>]
let DefaultPublishTo =
  "http://localhost:9200"

type ElasticSearchConf =
  { publishTo  : string
    _type : string }

  /// Create a new ElasticSearch target config.
  static member create(?publishTo, ?_type) =
    { publishTo  = defaultArg publishTo DefaultPublishTo
      _type       = defaultArg _type "logs" }

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

  open HttpFs.Client

  let generateId =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
    let charsLen = chars.Length
    let random = System.Random()
    fun len -> 
      let randomChars = [|for i in 0..len -> chars.[random.Next(charsLen)]|]
      new System.String(randomChars)
 
  let sendToElasticSearch elasticUrl _type (message : Message) =
    let _index  = "logary-" + DateTime.UtcNow.ToString("yyy-mm-dd")
    let _id = generateId 20
    let endpointUrl = elasticUrl + "/" + _index + "/" + _type + "/" + _id
    let bytes =
      Json.format (serialise message)
      |> UTF8.bytes
    let request = 
      Request.createUrl Post endpointUrl
      |> Request.body (RequestBody.BodyRaw bytes)

    Request.responseAsString request

  let loop (conf : ElasticSearchConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =

    let rec loop (_ : unit) : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack -> job {
          do! ack *<= ()
        }
        RingBuffer.take requests ^=> function
          | Log (message, ack) ->
            job {
              let! _ = sendToElasticSearch conf.publishTo conf._type message
              do! ack *<= ()
              return! loop ()
            }

          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! loop ()
            }
      ] :> Job<_>

    loop ()

let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<ElasticSearch.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  /// Specifies the ElasticSearch url.
  member x.PublishTo(publishTo : string) =
    Builder({ conf with publishTo = publishTo }, callParent)

  /// Change "_type" value, by default is "logs".
  member x.Type(_type : string) =
    Builder({ conf with _type = _type }, callParent)

  member x.Done() =
    ! (callParent x)

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(ElasticSearchConf.create DefaultPublishTo, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name =
      create conf name