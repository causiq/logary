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
open Logary.Configuration
open Logary.Target
open Logary.Internals
open Chiron

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
  let msgJson = 
    match Logary.Formatting.Json.encode message with
    | Json.Object jsonObj -> 
      jsonObj
      |> Inference.Json.Encode.required "@version" 2
      |> Inference.Json.Encode.required "@timestamp" (String (MessageWriter.formatTimestamp message.timestamp))
      |> JsonObject.toJson
    | otherwise ->  failwithf "Expected Message to format to Object .., but was %A" otherwise

  msgJson

let serialiseToJsonBytes : Message -> byte [] =
  serialise
  >> Json.format
  >> System.Text.Encoding.UTF8.GetBytes

module internal Impl =

  open HttpFs.Client

  let generateId (bytes : byte []) =
    use sha1 = SHA1.Create ()
    sha1.ComputeHash bytes
    |> BitConverter.ToString
    |> String.replace "-" ""

  let sendToElasticSearch elasticUrl _type indexName (message : Message) =
    let _index  = indexName + "-" + DateTime.UtcNow.ToString("yyy-MM-dd")
    let bytes = serialiseToJsonBytes message
    let _id = generateId bytes
    let endpointUrl = elasticUrl + "/" + _index + "/" + _type + "/" + _id
    let request =
      Request.createUrl Post endpointUrl
      |> Request.body (RequestBody.BodyRaw bytes)

    Request.responseAsString request
    |> Job.Ignore

  let loop (conf : ElasticSearchConf)
           (ri : RuntimeInfo, api : TargetAPI) =

    let rec loop (_ : unit) : Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack -> job {
          do! ack *<= ()
        }
        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            job {
              do! sendToElasticSearch conf.publishTo conf._type conf.indexName message
              do! ack *<= ()
              return! loop ()
            }

          | Flush (ackCh, nack) ->
            job {
              do! IVar.fill ackCh ()
              return! loop ()
            }
      ] :> Job<_>

    loop ()

let create conf = TargetConf.createSimple (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<ElasticSearch.Builder>() )
type Builder(conf, callParent : Target.ParentCallback<Builder>) =

  /// Specifies the ElasticSearch url.
  member x.PublishTo(publishTo : string) =
    Builder({ conf with publishTo = publishTo }, callParent)

  /// Change "_type" value, by default is "logs".
  member x.Type(_type : string) =
    Builder({ conf with _type = _type }, callParent)

  member x.Done() =
    ! (callParent x)

  new(callParent : Target.ParentCallback<_>) =
    Builder(ElasticSearchConf.create DefaultPublishTo, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name =
      create conf name
