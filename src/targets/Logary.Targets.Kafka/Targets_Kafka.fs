module Logary.Targets.Kafka

open System
open System.Collections.Generic
open System.Threading
open Confluent.Kafka.Admin
open System.Threading.Tasks
open Confluent.Kafka
open Hopac                       // for control flow
open Hopac.Infixes               // for control flow
open Logary                      // for the DataModel
open Logary.Internals            // for the TargetAPI
open Logary.Configuration.Target // for the fluent config API
open Logary.Model

type KafkaConf =
  /// Most often we agree – document your fields
  { tryCreate: bool
    topicName: string
    topicConf: IReadOnlyDictionary<string, string>
    partitions: uint16
    replicationFactor: uint16
    batchSize: uint16
    prodConfFile: string option
    prodConf: ProducerConfig
  }

  member x.asTopicSpec =
    let conf = Dictionary<string, string>(x.topicConf)
    TopicSpecification(Name=x.topicName, NumPartitions=int x.partitions, ReplicationFactor=int16 x.replicationFactor, Configs=conf)

  interface IValueFormattable with
    member x.toKeyValues baseKey =
      [ yield "tryCreate", Value.Bool x.tryCreate
        yield "topicName", Value.Str x.topicName
        for KeyValue (k, v) in x.topicConf do
          yield sprintf "topicConf.%s" k, Value.Str v
        yield "partitions", Value.Int64 (int64 x.partitions)
        yield "replicationFactor", Value.Int64 (int64 x.replicationFactor)
        yield "batchSize", Value.Int64 (int64 x.batchSize)
        if x.prodConfFile.IsSome then
          yield "prodConfFile", Value.Str x.prodConfFile.Value
        for KeyValue (k, v) in x.prodConf do
          yield sprintf "prodConf.%s" k, Value.Str v
      ]
      |> List.map (fun (k, v) -> sprintf "%s.%s" baseKey k, v)
      |> Map
      :> IReadOnlyDictionary<_,_>
      |> Choice2Of2

  interface TargetConfWriter<KafkaConf> with
    member x.write(key, value, hasOwnField) =
      if not hasOwnField then
        let pc = ProducerConfig(x.prodConf)
        pc.Set(key, value)
        { x with prodConf = pc }
      else
        x


let empty =
  { tryCreate = true
    topicName = "logary"
    topicConf = Map.empty
    partitions = 10us
    replicationFactor = 2us
    batchSize = 100us
    prodConfFile = None
    prodConf = ProducerConfig(BootstrapServers="ingress-broker-kafka-bootstrap.kafka.svc") }

type KafkaTargetExn(message, error: Error) =
  inherit Exception(message)
  member x.error = error

  static member ofError (e: Error) =
    KafkaTargetExn(sprintf "Publish failed %s with code=%O" (if e.IsFatal then "fatally" else "non-fatally") e.Code,
                   e)

  static member toKeyValues (baseKey, error: Error) =
    [ "error", Value.Bool true
      "errorCode", Value.Str (error.Code.ToString())
      "errorReason", Value.Str error.Reason
      "isBrokerError", Value.Bool error.IsBrokerError
      "isLocalError", Value.Bool error.IsLocalError ]
    |> List.map (fun (k, v) -> sprintf "%s.%s" baseKey k, v)

  static member toKeyValues (baseKey, report: DeliveryReport<Id, LogaryMessageBase>) =
    KafkaTargetExn.toKeyValues(baseKey, report.Error)

  static member toRODict (baseKey, error: Error) =
    KafkaTargetExn.toKeyValues (baseKey, error)
      |> Map
      :> IReadOnlyDictionary<string, Value>

  static member toRODict (baseKey, report: DeliveryReport<Id, LogaryMessageBase>) =
    KafkaTargetExn.toKeyValues (baseKey, report)
      |> Map
      :> IReadOnlyDictionary<string, Value>

  interface IValueFormattable with
    member x.toKeyValues baseKey =
      KafkaTargetExn.toRODict(baseKey, error)
        |> Choice2Of2


[<Struct>]
type LazyDisposer<'a when 'a :> IDisposable>(dL: Lazy<'a>) =
  interface IDisposable with
    member x.Dispose() = if dL.IsValueCreated then dL.Value.Dispose()

// When creating a new target this module gives the bare-bones
// for the approach you may need to take.
module internal Impl =
  type Producer = IProducer<Id, LogaryMessageBase>

  let idSerializer: ISerializer<Id> =
    { new ISerializer<Id> with
        member x.Serialize(data, context): byte[] = data.toByteArray()
    }

  // TO CONSIDER: how do we handle this?
  let valueSerializer: IAsyncSerializer<LogaryMessageBase> =
    { new IAsyncSerializer<LogaryMessageBase> with
        member x.SerializeAsync(message, context): Task<byte[]> =
          Task.FromResult Array.empty
    }

  type  SyslogLevel with
    member x.toLogary() =
      match x with
      | SyslogLevel.Emergency | SyslogLevel.Alert | SyslogLevel.Critical -> Fatal
      | SyslogLevel.Error -> Error
      | SyslogLevel.Warning -> Warn
      | SyslogLevel.Notice | SyslogLevel.Info -> Info
      | SyslogLevel.Debug -> Debug
      | _ -> Verbose

  type State =
    { producer: Producer }

    static member create p = { producer=p }

    interface IDisposable with
      member x.Dispose() =
        x.producer.Dispose()

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

  let maybeCreateTopic (conf: KafkaConf) (api: TargetAPI) =
    if not conf.tryCreate then
      api.runtime.logger.verbose "Will not try to create topic, because tryCreate=false in the KafkaConf config."
      Job.unit ()
    else
      job {
        do api.runtime.logger.debug (sprintf "Will try to create topic '%s'" conf.topicName)
        use c = AdminClientBuilder(conf.prodConf).Build()
        try
          do! Job.fromUnitTask (fun () -> c.CreateTopicsAsync(conf.asTopicSpec :: []))
        with :? CreateTopicsException as e ->
          if e.Results.[0].Error.Code <> ErrorCode.TopicAlreadyExists then
            api.runtime.logger.error("An error occured creating topic={topic}: {reason}", fun m ->
              m.setField("topic", conf.topicName)
              m.setField("reason", e.Results.[0].Error.Reason))
          else
            api.runtime.logger.verbose "Topic already exists"
      }

  let private logReportReceived _ = "Delivery report received"

  let dispose (d: #IDisposable) = d.Dispose()

  let loop (conf: KafkaConf) (api: TargetAPI) =
    let deliveriesCh = Ch ()
    let fatalCh = Ch ()

    /// Sends on fatalCh if IsFatal is true
    let handleError (p: Producer) (error: Error) =
      let e = KafkaTargetExn.ofError error
      if error.IsFatal then
        queue (Ch.give fatalCh (p, e))
      else
        let setDetails (m: LogaryMessageBase) = m.addExn e
        api.runtime.logger.error("Kafka target received non-fatal error", setDetails)

    /// Also ACKs.
    let handleReport (ack: IVar<unit>) =
      fun (report: DeliveryReport<Id, LogaryMessageBase>) ->
        let fillJ =
          if report.Error.IsFatal then
            // TO CONSIDER: can callers of logWithAck handle failing (with exception) ACKs, or should
            // I do it via "catastrophicsCh" instead?
            let ex = KafkaTargetExn.ofError report.Error
            IVar.fillFailure ack ex
          else
            IVar.fill ack ()

        queue (Ch.send deliveriesCh report >>=. fillJ)

    let rec initialise () =
      api.runtime.logger.debug("Starting Kafka target", fun m -> m.setField("config", conf))
      maybeCreateTopic conf api
      >>= startProducer

    and startProducer () =
      let b = ProducerBuilder<Id, LogaryMessageBase>(conf.prodConf)
      let p =
        b.SetErrorHandler(Action<_,_> handleError)
          .SetKeySerializer(idSerializer)
          .SetLogHandler(Action<_,_> (handleLogs api))
          .SetStatisticsHandler(Action<_,_> (handleStats api))
          .SetValueSerializer(valueSerializer) // can we integrate with serde/schema registry
          .Build()

      Job.using p (State.create >> loop)

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
          let acks = ResizeArray<_>(messages.Length)
          let flushes = ResizeArray<_>(max (messages.Length / 10) 10)

          for tm in messages do
            match tm with
            | Log (m, ack) ->
              // handleReport fills the ack IVar
              state.producer.Produce(conf.topicName, Message<_, _>(Key=m.id,Value=m), handleReport ack)
              acks.Add ack

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

          Job.using (new LazyDisposer<_>(cts)) (fun _ -> Job.conIgnore flushes)
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