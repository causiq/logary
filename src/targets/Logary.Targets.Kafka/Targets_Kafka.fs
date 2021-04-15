module Logary.Targets.Kafka

open System
open System.Collections.Generic
open System.Text
open System.Threading
open Confluent.Kafka.Admin
open Confluent.Kafka
open Confluent.SchemaRegistry
open Confluent.SchemaRegistry.Serdes
open Hopac                       // for control flow
open Hopac.Infixes               // for control flow
open Logary                      // for the DataModel
open Logary.Internals            // for the TargetAPI
open Logary.Configuration.Target // for the fluent config API
open Logary.Model
open Newtonsoft.Json.Schema

type KafkaConf =
  /// Most often we agree – document your fields
  { tryCreate: bool
    topicName: string
    /// Whether to only publish to a single topic. Otherwise publishes each type of message to a separate topic.
    singleTopic: bool
    topicConf: IReadOnlyDictionary<string, string>
    partitions: uint16
    replicationFactor: uint16
    batchSize: uint16
    /// Optional producer configuration file to load.
    prodConfFile: string option
    /// Optional schema registry configuration. You can set its properties with dotnet.producer.X.Y as well as sasl.* and similarly available client configuration properties as can be found in the Confluent docs.
    prodConf: ProducerConfig
    /// Optional schema registry configuration. You can set its properties with schema.registry.X.Y
    schemaRegistryConf: SchemaRegistryConfig
    /// A configuration value for the JsonSerializer; we'll use it to configure our own serialiser. You can set its properties with json.X.Y
    jsonSerializerConf: JsonSerializerConfig
    /// Disable usage of the Confluent Schema Registry. Requires explicit opt-out.
    noJsonSchema: bool
    /// Optional URL to load the JSON schemas from. Defaults to https://app.logary.tech/schemas/logary-message.schema.json
    jsonSchemaUrl: string option
    /// Optional JsonSchema value to use instead of `jsonSchemaUrl`.
    jsonSchema: JSchema option
  }

  member x.asTopicSpec: TopicSpecification list =
    let conf = Dictionary<string, string>(x.topicConf)
    let topic name =
      TopicSpecification(
        Name=name,
        NumPartitions=int x.partitions,
        ReplicationFactor=int16 x.replicationFactor,
        Configs=conf
      )
    if x.singleTopic then
      [ topic x.topicName ]
    else
      [ MessageKind.Event
        MessageKind.Gauge
        MessageKind.Histogram
        MessageKind.ForgetUser
        MessageKind.IdentifyUser
        MessageKind.SetUserProperty
      ]
      |> List.map (fun k -> k.ToString().ToLowerInvariant())
      |> List.map (sprintf "%s_%s" (x.topicName.ToLowerInvariant()))
      |> List.map topic


  member x.asSchemaStrategy =
    match x.noJsonSchema, x.jsonSchema, x.jsonSchemaUrl with
    | true, _, _ -> NoSchema
    | _, Some schema, _ -> OfValue schema
    | _, _, Some url -> OfURL url
    | _ -> NoSchema

  interface IValueFormattable with
    member x.toKeyValues baseKey =
      [ yield "tryCreate", Value.Bool x.tryCreate
        yield "topicName", Value.Str x.topicName
        yield "singleTopic", Value.Bool x.singleTopic
        for KeyValue (k, v) in x.topicConf do
          yield sprintf "topicConf.%s" k, Value.Str v
        yield "partitions", Value.Int64 (int64 x.partitions)
        yield "replicationFactor", Value.Int64 (int64 x.replicationFactor)
        yield "batchSize", Value.Int64 (int64 x.batchSize)
        if x.prodConfFile.IsSome then
          yield "prodConfFile", Value.Str x.prodConfFile.Value
        for KeyValue (k, v) in x.prodConf do
          yield sprintf "prodConf.%s" k, Value.Str v
        for KeyValue (k, v) in x.schemaRegistryConf do
          yield sprintf "schemaRegistryConf.%s" k, Value.Str v
        for KeyValue (k, v) in x.jsonSerializerConf do
          yield sprintf "jsonSerializerConf.%s" k, Value.Str v
        yield "noJsonSchema", Value.Bool x.noJsonSchema
        if x.jsonSchemaUrl.IsSome then
          yield "jsonSchemaURL", Value.Str x.jsonSchemaUrl.Value
        if x.jsonSchema.IsSome then
          yield "jsonSchema", Value.Str (x.jsonSchema.Value.ToString())
      ]
      |> List.map (fun (k, v) -> sprintf "%s.%s" baseKey k, v)
      |> Map
      :> IReadOnlyDictionary<_,_>
      |> Choice2Of2

  interface TargetConfWriter<KafkaConf> with
    member x.write(key, value, hasOwnField) =
      if hasOwnField then x else
      if key.StartsWith "schema.registry" then
        x.schemaRegistryConf.Set(key, value)
        x
      elif key.StartsWith "json" then
        x.jsonSerializerConf.Set(key, value)
        x
      else
        let pc = ProducerConfig(x.prodConf)
        pc.Set(key, value)
        { x with prodConf = pc }


let empty =
  { tryCreate = true
    topicName = "logary"
    topicConf = Map.empty
    partitions = 3us
    replicationFactor = 2us
    batchSize = 100us
    prodConfFile = None
    prodConf =
      ProducerConfig(
        BootstrapServers="broker-kafka-bootstrap.kafka.svc:9092",
        EnableIdempotence=Nullable<_> true,
        // https://github.com/tulios/kafkajs/issues/910
        CompressionType=Nullable<_> CompressionType.Snappy,
        ClientId="logary",
        Acks = Nullable<_> Acks.All)
    schemaRegistryConf =
      SchemaRegistryConfig(
        Url = "http://schema-registry:8081")
    jsonSerializerConf =
      JsonSerializerConfig()
    noJsonSchema = false
    jsonSchemaUrl = Some "https://app.logary.tech/schemas/logary-message.merged.schema.json"
    jsonSchema = None
    singleTopic = false
  }

// When creating a new target this module gives the bare-bones
// for the approach you may need to take.
module internal Impl =
  open Logary.Internals.Chiron

  let utf8 = UTF8Encoding(false)

  type Producer = IProducer<string, LogaryMessageBase>

  let keySerializer: ISerializer<string> =
    { new ISerializer<_> with
        member x.Serialize(data, context): byte[] = utf8.GetBytes data
    }

  type SyslogLevel with
    member x.toLogary() =
      match x with
      | SyslogLevel.Emergency | SyslogLevel.Alert | SyslogLevel.Critical -> Fatal
      | SyslogLevel.Error -> Error
      | SyslogLevel.Warning -> Warn
      | SyslogLevel.Notice | SyslogLevel.Info -> Info
      | SyslogLevel.Debug -> Debug
      | _ -> Verbose

  type State =
    { producer: Producer
      srClient: ISchemaRegistryClient }
    static member create (p, srClient) = { producer=p; srClient=srClient }

    interface IDisposable with
      member x.Dispose() =
        x.producer.Dispose()
        x.srClient.Dispose()

  let handleStats (api: TargetAPI) =
    fun (p: Producer) (json: string) ->
      api.runtime.logger.debug("Stats received {stats} for producer={producer}", fun m ->
        m.setField("stats", json)
        m.setField("producer", p.Name))

  let handleLogs (api: TargetAPI) =
    fun (p: Producer) (logMessage: LogMessage) ->
      let e = Event(logMessage.Message, None, level=logMessage.Level.toLogary())
      e.setField("producer", p.Name)
      e.name <- logMessage.Facility |> PointName.parse
      api.runtime.logger.log(e)

  let logCreateTopicExn (topicNames: string, api: TargetAPI) (e: CreateTopicsException) =
    try
      if e.Results.[0].Error.Code <> ErrorCode.TopicAlreadyExists then
        api.runtime.logger.error("An error occured creating topics={topics}: {reason}", fun m ->
          m.setField("topics", topicNames)
          m.setField("reason", e.Results.[0].Error.Reason))
      else
        api.runtime.logger.info (sprintf "Topics '%s' already exists, continuing..." topicNames)
    with e ->
      eprintfn "%O" e // exception from exception handler :(

  let maybeCreateTopic (conf: KafkaConf) (api: TargetAPI) =
    if not conf.tryCreate then
      api.runtime.logger.debug "Will not try to create topic, because tryCreate=false in the KafkaConf config."
      Job.unit ()
    else
      job {
        do api.runtime.logger.debug (sprintf "Maybe creating topic '%s'" conf.topicName, fun m -> m.setField("conf", conf))
        use c = AdminClientBuilder(conf.prodConf).Build()
        do api.runtime.logger.debug "Built AdminClient"
        let topics = conf.asTopicSpec
        let topicNames = topics |> List.map (fun t -> t.Name) |> String.concat ", "
        try
          let fourSeconds = TimeSpan.FromSeconds 4. |> Nullable<_>
          do api.runtime.logger.debug("Calling CreateTopicsAsync create for topics={topics}", fun m -> m.setField("topics", topicNames))
          let o = CreateTopicsOptions(OperationTimeout=fourSeconds, RequestTimeout=fourSeconds)
          do! Job.fromUnitTask (fun () -> c.CreateTopicsAsync(topics, o))
        with
        | :? CreateTopicsException as e ->
          logCreateTopicExn (topicNames, api) e
        | :? AggregateException as ae ->
          if ae.InnerException :? CreateTopicsException then
            logCreateTopicExn (topicNames, api) (ae.InnerException :?> CreateTopicsException)
            return ()
          else
            api.runtime.logger.error ("Crashed with AggregateException", fun m -> m.addExn ae)
            return! Job.raises ae
      }

  let private logReportReceived _ = "Delivery report received"

  let dispose (d: #IDisposable) = d.Dispose()

  let loop (conf: KafkaConf) (api: TargetAPI) =
    let deliveriesCh = Ch ()
    let fatalCh = Ch ()

    /// Sends on fatalCh if IsFatal is true
    let handleError (p: Producer) (error: Error) =
      let e = KafkaTargetExn.ofError error
      let setDetails (m: LogaryMessageBase) = m.addExn e
      api.runtime.logger.error(e.Message, setDetails)
      if error.IsFatal then queue (Ch.give fatalCh (p, e))

    let rec initialise () =
      api.runtime.logger.debug("Starting Kafka target", fun m -> m.setField("config", conf))
      maybeCreateTopic conf api
      >>= startProducer

    and startProducer () =
      let builder = ProducerBuilder<string, LogaryMessageBase>(conf.prodConf)
      let registry = new CachedSchemaRegistryClient(conf.schemaRegistryConf)
      let serialiser =
        Serialiser(
          api.runtime.logger,
          registry,
          Json.Encode.logaryMessageBase,
          conf.jsonSerializerConf,
          conf.asSchemaStrategy)
      let producer =
        builder.SetErrorHandler(Action<_,_> handleError)
          .SetKeySerializer(keySerializer)
          .SetLogHandler(Action<_,_> (handleLogs api))
          .SetStatisticsHandler(Action<_,_> (handleStats api))
          .SetValueSerializer(serialiser) // can we integrate with serde/schema registry
          .Build()

      api.runtime.logger.debug("Starting Kafka target's producer", fun m -> m.setField("config", conf))

      let state = State.create (producer, registry)
      Job.using state loop

    and loop (state: State) =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          dispose state
          ack *<= ()

        fatalCh ^=> fun (p, ex) ->
          api.runtime.logger.fatal("Crashing and burning, producer={producer}", fun m ->
            m.setField("producer", p.Name)
            m.addExn ex)
          Job.raises ex

        deliveriesCh ^=> fun _ ->
          api.runtime.logger.verboseDelay(logReportReceived)
          loop state

        RingBuffer.takeBatch conf.batchSize api.requests ^=> fun messages ->
          let cts = lazy (new CancellationTokenSource())
          //let acks = ResizeArray<_>(messages.Length)
          let flushes = ResizeArray<_>(max (messages.Length / 10) 10)

          let iterJ = job {
            for tm in messages do
              match tm with
              | Log (m, ack) ->
                // handleReport fills the ack IVar
                let mDTO =
                  Message<_, _>(
                    Key=m.id.toBase64String(),
                    Value=m,
                    Timestamp=Timestamp(m.timestamp / 1_000_000L, TimestampType.CreateTime)
                  )

                let messageTopic = if conf.singleTopic then conf.topicName else sprintf "%s_%O" conf.topicName (m.kind.ToString().ToLowerInvariant())
                let! _ = state.producer.ProduceAsync(messageTopic, mDTO)
                do! ack *<= ()

              | Flush (ack, nack) ->
                start (nack |> Alt.afterFun cts.Value.Cancel)

                let handleFlush = function
                  | Ok () ->
                    IVar.fill ack ()
                  | Result.Error e ->
                    IVar.fillFailure ack e

                let fJ =
                  Hopac.Job.Scheduler.isolate (fun () ->
                    try
                      Ok (state.producer.Flush(cts.Value.Token))
                    with e ->
                      Result.Error e)
                  >>= handleFlush

                flushes.Add fJ
          }

          iterJ
          >>=. Job.using (new LazyDisposer<_>(cts)) (fun _ -> Job.conIgnore flushes)
          >>= fun () -> loop state

      ] :> Job<_>

    initialise ()

/// Create a new Kafka target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name
  // TargetConf.create Policy.exponentialBackoffForever 512us (Impl.loop conf) name

/// <summary>
/// Use with:
/// </summary>
///
/// <code>
/// using Logary.Targets.Kafka;
/// LogaryFactory.New( s => s.Target<Kafka.Builder>() )
/// </code>
type Builder(conf, callParent: ParentCallback<Builder>) =
  let update (conf': KafkaConf): Builder =
    Builder(conf', callParent)

  member x.Partitions(no: uint16) =
    update { conf with partitions = no }

  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  // this is called in the end, after calling all your custom configuration
  // methods (above) which in turn take care of making the F# record that
  // is the configuration, "just so"
  interface SpecificTargetConf with
    member x.Build name = create conf name