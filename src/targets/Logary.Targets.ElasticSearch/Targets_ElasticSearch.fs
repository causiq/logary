/// A Logary target for Elasticsearch
module Logary.Targets.ElasticSearch

#nowarn "1104"

open Hopac
open NodaTime
open System
open System.Net
open System.Net.Sockets
open System.IO
open System.Security.Cryptography
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Formatting
open Logary.Target
open Logary.Internals
open Logary.Serialisation.Chiron

/// This is the default address this Target publishes messages to.
[<Literal>]
let DefaultPublishTo =
  "http://localhost:9200"

type ElasticSearchConf =
  { /// Server URL, by default "http://localhost:9200"
    publishTo  : string
    /// ElasticSearch document "_type", by default "logs"
    _type      : string
    /// Prefix for log indexs, defaults to "logary"
    indexName  : String}

  /// Create a new ElasticSearch target config.
  static member create(?publishTo, ?_type, ?indexName) =
    { publishTo  = defaultArg publishTo DefaultPublishTo
      _type      = defaultArg _type "logs"
      indexName  = defaultArg indexName "logary" }

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

  let generateId (bytes : byte []) =
    use sha1 = SHA1.Create ()
    sha1.ComputeHash bytes
    |> BitConverter.ToString
    |> String.replace "-" ""

  let sendToElasticSearch elasticUrl _type indexName (message : Message) =
    let _index  = indexName + "-" + DateTime.UtcNow.ToString("yyy-MM-dd")
    let bytes =
      Json.format (serialise message)
      |> UTF8.bytes
    let _id = generateId bytes
    let endpointUrl = elasticUrl + "/" + _index + "/" + _type + "/" + _id
    let request =
      Request.createUrl Post endpointUrl
      |> Request.body (RequestBody.BodyRaw bytes)

    Request.responseAsString request
    |> Job.Ignore

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
              do! sendToElasticSearch conf.publishTo conf._type conf.indexName message
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
