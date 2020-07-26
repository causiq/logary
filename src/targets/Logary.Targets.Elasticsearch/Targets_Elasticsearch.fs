/// A Logary target for Elasticsearch
module Logary.Targets.Elasticsearch

open System.Runtime.CompilerServices

#nowarn "1104"

[<assembly:InternalsVisibleTo("Logary.Targets.Elasticsearch.Tests")>]
do()

open Hopac
open System
open System.Security.Cryptography
open Hopac.Infixes
open Logary
open Logary.Model
open Logary.Configuration
open Logary.Internals
open Logary.Internals.Chiron
module E = Json.Encode

/// This is the default address this Target publishes messages to.
[<Literal>]
let DefaultPublishTo =
  "http://localhost:9200"

type ElasticsearchConf =
  { /// Server URL, by default "http://localhost:9200"
    publishTo: string
    /// Elasticsearch document "_type", by default "logs"
    _type: string
    /// Prefix for log indexs, defaults to "logary"
    indexName: string }

  /// Create a new Elasticsearch target config.
  static member create(?publishTo, ?_type, ?indexName) =
    { publishTo  = defaultArg publishTo DefaultPublishTo
      _type      = defaultArg _type "logs"
      indexName  = defaultArg indexName "logary" }

let empty = ElasticsearchConf.create()

module internal Impl =
  let builder: ObjectBuilder<LogaryMessageBase> =
    fun (m: LogaryMessageBase) ->
      E.required E.int "@version" 2
      >> E.required E.dateTimeOffset "@timestamp" (DateTimeOffset.ofEpoch m.timestamp)
      >> E.requiredMixin E.logaryMessage m

  let encode (m: LogaryMessageBase): Json =
    E.jsonObjectWith builder m

  let serialise m: string =
    Json.serializeObjectWith builder JsonFormattingOptions.Compact m

  open HttpFs.Client

  let sendToElasticsearch elasticUrl _type indexName (message: LogaryMessageBase) =
    let _index  = indexName + "-" + DateTime.UtcNow.ToString("yyy-MM-dd")
    let endpointUrl = elasticUrl + "/" + _index + "/" + _type + "/" + (message.id.toBase64String())
    let request =
      Request.createUrl Post endpointUrl
      |> Request.body (RequestBody.BodyString (serialise message))

    Request.responseAsString request
    |> Job.Ignore

  let loop (conf: ElasticsearchConf) (api: TargetAPI) =
    let rec loop (_: unit): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack -> job {
          do! ack *<= ()
        }
        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            job {
              do! sendToElasticsearch conf.publishTo conf._type conf.indexName message
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

[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<Elasticsearch.Builder>() )
type Builder(conf, callParent: Target.ParentCallback<Builder>) =

  /// Specifies the Elasticsearch url.
  member x.PublishTo(publishTo: string) =
    Builder({ conf with publishTo = publishTo }, callParent)

  /// Change "_type" value, by default is "logs".
  member x.Type(_type: string) =
    Builder({ conf with _type = _type }, callParent)

  member x.Done() =
    ! (callParent x)

  new(callParent: Target.ParentCallback<_>) =
    Builder(ElasticsearchConf.create DefaultPublishTo, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name =
      create conf name
