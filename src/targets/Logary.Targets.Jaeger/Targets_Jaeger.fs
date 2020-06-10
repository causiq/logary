module Logary.Targets.Jaeger

open System.Globalization
open Hopac
open Hopac.Infixes
open Jaeger.Thrift
open Logary.Model
open NodaTime
open System
open System.Collections.Generic
open System.Runtime.InteropServices
open Thrift.Protocols
open Thrift.Transports.Client
open Logary
open Logary.Configuration
open Logary.Internals
open Logary.Trace
open Logary.Trace.Sampling
open Logary.Targets

type Client =
  inherit IDisposable
  abstract send: batch: Jaeger.Thrift.Batch -> Alt<unit>

type JaegerConf =
  { /// Tags present in all Spans logged from this instance of the target.
    processTags: SpanAttr list

    /// The maximum package size in bytes; should not be larger than the network's MTU,
    /// which often is: `System.UInt16.MaxValue`, 65535 bytes.
    ///
    /// https://serverfault.com/questions/246508/how-is-the-mtu-is-65535-in-udp-but-ethernet-does-not-allow-frame-size-more-than
    ///
    /// Note that anything larger than 1472 bytes will be broken up as _fragments_ on the ethernet
    /// layer.
    ///
    /// Some OSes will NOT transmit UDP if data will be fragmented. I.E. Linux doc, By default, Linux
    /// UDP does path MTU (Maximum Transmission Unit) discovery. This means the kernel will keep
    /// track of the MTU to a specific target IP address and return EMSGSIZE when a UDP packet
    /// write exceeds it.
    packetSize: uint16

    /// Specifies how long to retain log messages not logged with the `SpanLogger`, but only has
    /// a SpanId attached. After this duration, if the parent Span has not completed, the Message
    /// will be dropped.
    retentionTime: Duration

    /// How many items that the Jaeger target will pick from the buffers and try to send at a go. Prefer
    /// larger sizes (towards 512) over smaller sizes, since the default Exporter is buffered and on top of
    /// that, the operating system buffers in the network stack.
    batchSize: uint16

    /// The Jaeger DNS name to send to.
    jaegerHost: string

    /// The Jaeger port to send to.
    jaegerPort: uint16

    /// Formatter to translate log messages into Jaeger tag values.
    messageWriter: MessageWriter

    /// Allows you to override the sending mechanism; have a look at the implementation for docs on how
    /// to do this. Useful if you want to switch to an HTTP transport, or the Binary instead of Compact
    /// format.
    client: Client option

    /// How long do we wait for ambient logs, before sending the Spans off to Jaeger?
    spanDelay: Duration

    /// What Sampler should be used to decide whether to forward the log?
    sampler: Sampler
  }

let empty =
  { processTags = []
    packetSize =
      if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
        // sysctl -a | grep gram
        9216us
      else
        // default UInt16.MaxValue, like in jaeger-client-node
        UInt16.MaxValue
    retentionTime = Duration.FromMinutes 5.
    jaegerHost = "localhost"
    jaegerPort = 6831us
    batchSize = 512us
    messageWriter = SimpleMessageWriter()
    client = None
    spanDelay = Duration.FromSeconds 5L
    sampler = new ConstSampler(true)
  }

module internal Mapping =
  type Value with
    member x.writeTo(tag: Tag) =
      match x with
      | Value.Bool v ->
        tag.VBool <- v
        tag.VType <- TagType.BOOL
      | Value.Str s ->
        tag.VStr <- s
        tag.VType <- TagType.STRING
      | Value.Float f ->
        tag.VDouble <- f
        tag.VType <- TagType.DOUBLE
      | Value.BigInt i ->
        tag.VDouble <- float i
        tag.VType <- TagType.DOUBLE
      | Value.Int64 i ->
        tag.VLong <- i
        tag.VType <- TagType.LONG
      | Value.Fraction _ as v ->
        tag.VDouble <- v.asFloat
        tag.VType <- TagType.DOUBLE


  let toTag (KeyValue (k, v): SpanAttr) =
    let tag = Jaeger.Thrift.Tag(Key = k)
    v.writeTo tag
    tag

  let createJaegerProcess (runtime: RuntimeInfo) (processTags: Tag list) =
    let tag k v = Tag(k, TagType.STRING, VStr=v)
    let p = Process(runtime.service)
    p.Tags.Add(tag "host" runtime.host)
    p.Tags.Add(tag "logary.version" AssemblyVersionInformation.AssemblyVersion)
    p.Tags.AddRange(processTags)
    p

  type Tag with
    static member create(key: string, name: MessageKind) = Tag(key, TagType.STRING, VStr=name.ToString())
    static member create(key: string, name: string) = Tag(key, TagType.STRING, VStr=name)
    static member create(key: string, value: bool) = Tag(key, TagType.BOOL, VBool=value)
    static member create(key: string, value: int) = Tag(key, TagType.LONG, VLong=int64 value)
    static member create(key: string, value: int64) = Tag(key, TagType.LONG, VLong=value)
    static member create(key: string, value: float) = Tag(key, TagType.DOUBLE, VDouble=value)
    static member create(key: string, value: Id) = Tag(key, TagType.STRING, VStr=value.toBase64String())
    static member create(key: string, value: PointName) = Tag.create(key, value.ToString())
    static member create(key: string, value: LogLevel) = Tag.create(key, value.ToString())
    static member create(key: string, value: Value) =
      match value with
      | Value.Str s -> Tag.create(key, s)
      | Value.Bool b -> Tag.create(key, b)
      | _ -> Tag(key, TagType.DOUBLE, VDouble=value.asFloat)
    static member create(key: string, value: U) = Tag.create(key, defaultArg value.name "-")

  type LogaryMessage with
    member x.getJaegerTags(): ResizeArray<Tag> =
      seq {
        yield Tag.create("id", x.id)
        yield Tag.create("component", x.name)
        yield Tag.create("level", x.level)
        yield Tag.create("type", x.kind)

        if x.received.IsSome then
          yield Tag.create("received", x.received.Value)

        if x.gauges.Count > 0 then
          for i, KeyValue (name, g) in x.gauges |> Seq.mapi (fun i kv -> i, kv) do
            yield Tag.create(sprintf "%s.%i.value" name i, g.value)
            yield Tag.create(sprintf "%s.%i.unit" name i, g.unit)

        if x.fields.Count > 0 then
          for KeyValue (name, v) in x.fields do
            yield Tag.create(name, v)

        if x.context.Count > 0 then
          for KeyValue (name, v) in x.context do
            yield Tag.create(name, v)

        match x with
        | :? EventMessage as e ->
          yield Tag.create("event", e.event)
          if e.monetaryValue.IsSome then
            yield Tag.create("monetaryValue.amount", e.monetaryValue.Value.value)
            yield Tag.create("monetaryValue.currency", e.monetaryValue.Value.unit)

        | :? GaugeMessage as g ->
          yield Tag.create("gauge.value", g.gauge.value)
          yield Tag.create("gauge.unit", g.gauge.unit)

        | :? HistogramMessage as h ->
          let buckets = String.Join(",", h.buckets.Keys |> Seq.map (fun f -> (f :> IFormattable).ToString("G", CultureInfo.InvariantCulture)))
          let values = String.Join(",", h.buckets.Values |> Seq.map (fun f -> (f :> IFormattable).ToString("G", CultureInfo.InvariantCulture)))
          yield Tag.create("histogram.count", h.buckets.Count)
          yield Tag.create("histogram.buckets", buckets)
          yield Tag.create("histogram.values", values)
          yield Tag.create("histogram.sum", h.sum)

        | _ -> ()

      } |> ResizeArray<_>

    member x.getJaegerLog() =
      let jaegerTags = x.getJaegerTags()
      Jaeger.Thrift.Log(x.timestamp / 1000L, jaegerTags)


module internal Impl =
  open Mapping
  open System.Runtime.CompilerServices
  [<assembly: InternalsVisibleTo("Logary.Targets.Jaeger.Tests")>]
  do()


  let private dispose (d: #IDisposable) = d.Dispose()

  let createClient conf: Client =
    let protocol =
      new TCompactProtocol(
        new TBufferedClientTransport(
          new TUdpClientTransport(conf.jaegerHost, int conf.jaegerPort),
          int conf.packetSize))

    let client = new Jaeger.Thrift.Agent.Agent.Client(protocol)

    { new Client with
        member x.send batch =
          Alt.fromUnitTask (fun ct -> client.emitBatchAsync(batch, ct))
        member x.Dispose () =
          client.Dispose()
    }

  /// The state is highly mutable, in that it's expected that only one green Hopac thread at a time accesses it,
  /// and when it does, may mutate the instances in the state, before recursing to take further LogMessage requests.
  ///
  type State =
    { /// A cache of `SpanId -> Log list`, with methods corresponding to the use-cases;
      /// - garbage collecting if no matching span was logged
      /// - getting the `Log list` for a given `SpanId`, and removing those from future consideration, and
      /// - enqueueing more `Log` items, for future consideration.
      ///
      /// E.g.
      /// T1 logs M1: "Hello spanId=1"
      /// T2 logs M2: "Span 1 start" (SpanData)
      /// Target receives M1, no matching span in
      ambient: ListCache<SpanId, Jaeger.Thrift.Log>
      deferred: DeferBuffer<SpanId, Jaeger.Thrift.Span * IVar<unit>>
      client: Client
      jaegerProcess: Jaeger.Thrift.Process
    }

  /// https://github.com/jaegertracing/jaeger-client-java/blob/master/jaeger-core/src/test/java/io/jaegertracing/internal/JaegerSpanTest.java#L254-L265
  let buildJaegerSpan (span: SpanMessage, samplerAttrs: Tag list, ambientLogs: Jaeger.Thrift.Log list) =
    let parent = span.parentSpanId |> Option.map (fun x -> x.id) |> Option.defaultValue 0L

    let events =
      let ra = ResizeArray<_>(span.events.Count + ambientLogs.Length)
      ra.AddRange(span.events |> Seq.map (fun m -> m.getJaegerLog()))
      ra.AddRange(ambientLogs)
      ra

    let tags = span.getJaegerTags()
    tags.AddRange(samplerAttrs)

    let jaegerSpan =
      Jaeger.Thrift.Span(span.context.traceId.low, span.context.traceId.high, span.context.spanId.id,
        parent, span.label,
        // if we get here, the span is sampled since we have 'samplerAttrs'
        int (span.flags ||| SpanFlags.Sampled),
        // time unit: Microseconds (µs) for both the epoch timestamp and the duration
        span.started / 1000L, (span.finished - span.started) / 1000L)
    jaegerSpan.Logs <- events
    jaegerSpan.Tags <- tags
    jaegerSpan

  /// When an ambient log `Message` is received, it means it is *not* carrying a `SpanData` instance in its `.context`,
  /// and hence must be either added to an existing deferred `Span`, OR be pushed to the `ambient` `ListCache` in
  /// the state, waiting for a `Span` to be completed and later flushed to the Jaeger client.
  let handleAmbientLog (state: State) (spanId: SpanId, msg: LogaryMessage, ri: RuntimeInfo): State =
    // First convert the message to a Jaeger log message
    let log = msg.getJaegerLog()

    // Now, see if there's a matching deferred `Span`, that we can add the `Log` to;
    let wasAdded =
      state.deferred.tryModify(spanId, fun (span, ack) ->
        ri.logger.verbose("handleAmbientLog found deferred Span={spanId}, saving `Log`:s into it", fun m ->
                          m.spanId <- Some spanId)
        span.Logs.Add log
        span, ack)

    // Otherwise, if there wasn't, we'll keep track of the ambient log `Message` for `conf.retentionTime`. During the
    // `retentionTime` Duration, either a `Span` will be logged and then flushed and the `ambient` `Log` value associated with it,
    // OR the `ambient` `Log` values will be garbage collected.
    if not wasAdded then
      ri.logger.verbose ("handleAmbientLog did not find deferred Span={spanId}, enqueueing Log as ambient", fun m ->
        m.spanId <- Some spanId)
      state.ambient.enqueue(spanId, log)

    state

  let handleSpan (ri: RuntimeInfo) (span: SpanMessage, samplerAttrs: Tag list, ack: IVar<unit>) (state: State): State =
    let ambientLogs = state.ambient.tryGetAndRemove span.context.spanId
    let jaegerSpan = buildJaegerSpan (span, samplerAttrs, ambientLogs)
    ri.logger.verbose("handleSpan deferring Span={spanId} & associated {ambientCount} ambient and {spanCount} Span logs", fun m ->
      m.setField("spanId", span.context.spanId)
      m.setField("spanCount", span.events.Count)
      m.setField("ambientCount", ambientLogs.Length))
    state.deferred.defer(span.context.spanId, (jaegerSpan, ack))
    state

  /// Selects immediately, commits on the completion of `client.send`'s Alt return value, and if NACK:ed,
  /// automatically cancels the client's send operation without cleaning up.
  let flushSpans (ri: RuntimeInfo, client: Client, proc: Process) (spanKVPs: KeyValuePair<SpanId, Jaeger.Thrift.Span * IVar<unit>> list): Alt<unit> =
    if List.isEmpty spanKVPs then Alt.unit () else
    ri.logger.verbose("flushSpans is flushing {count} Spans", fun m -> m.setField("count", spanKVPs.Length))

    let spans = ResizeArray<_>(spanKVPs.Length)

    spanKVPs
      |> Seq.map (fun (KeyValue (_, (span, _))) -> span)
      |> spans.AddRange

    let ackAll =
      spanKVPs
        |> Seq.map (fun (KeyValue (_, (_, ack))) -> ack *<= ())
        |> Job.conIgnore

    let jaegerBatch = Jaeger.Thrift.Batch(proc, spans)

    Alt.tryIn
      (client.send jaegerBatch)
      (fun () -> ackAll)
      (fun exn ->
        ri.logger.error("Sending to Batch to Jaeger Agent failed", fun m -> m.addExn exn)
        ackAll)

  let asTags (attrs: SpanAttr list): Tag list = attrs |> List.map (fun (KeyValue (k, v)) -> Tag.create(k, v))

  let loop (conf: JaegerConf) (api: TargetAPI) =
    api.runtime.logger.info ("Started Jaeger target with endpoint {host}:{port}", fun m ->
      m.setField("host", conf.jaegerHost)
      m.setField("port", conf.jaegerPort))

    let ticker, dueToSend = Ch (), Ch ()

    let rec running (state: State) =
      Alt.choosy [|
        api.shutdownCh ^=> fun ack ->
          api.runtime.logger.verbose ("Shutting down the Jaeger target and flushing {count} deferred Spans...", fun m ->
                                      m.setField("count", state.deferred.count))
          flushSpans (api.runtime, state.client, state.jaegerProcess) (state.deferred.dequeueAll()) >>= fun () ->
            dispose state.ambient
            dispose state.deferred
            dispose state.client
            api.runtime.logger.verbose "Shutting down the Jaeger target."
            ack *<= ()

        dueToSend ^=>
          (flushSpans (api.runtime, state.client, state.jaegerProcess)
           >=> fun () -> running state)

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            let nextState =
              match message with
              | :? SpanMessage as span ->
                match conf.sampler.shouldSample span with
                | Ok attrs ->
                  handleSpan api.runtime (span, asTags attrs, ack) state
                | Result.Error () ->
                  state
              | :? LogaryMessageBase as m when Option.isSome m.spanId ->
                handleAmbientLog state (m.spanId.Value, message, api.runtime)
              | _ ->
                state

            ack *<= () >>= fun () ->
            running nextState


          | Flush (ack, nack) ->
            Alt.choosy [|
              flushSpans (api.runtime, state.client, state.jaegerProcess) (state.deferred.dequeueAll()) ^=> fun () ->
                ack *<= () >>-. state

              nack ^->. state
            |]
            >>= running

        ticker ^=> fun _ ->
          let due =
            state.deferred.dequeueDue() |> List.map (fun (KeyValue (spanId, (span, ack))) ->
            let ambientLogs = state.ambient.tryGetAndRemove spanId
            span.Logs.AddRange ambientLogs
            KeyValuePair<_,_>(spanId, (span, ack)))

          let gced = state.ambient.gc ()
          if gced.Count > 0 then
            api.runtime.logger.verbose("Garbage-collected {count} ambient logs", fun m ->
            m.setField("count", gced.Count))

          // Place the due values in the buffer; the main Jaeger target loop will select from the `dueToSend` channel
          // before taking new ticks anyway, thus ensuring there's only ever one batch waiting in the channel.
          Ch.send dueToSend due >>= fun () ->
          running state
      |]
      :> Job<_>

    let state =
      { ambient = new ListCache<SpanId, Jaeger.Thrift.Log>(api.runtime.getTimestamp, conf.retentionTime)
        deferred = new DeferBuffer<SpanId, Jaeger.Thrift.Span * IVar<unit>>(api.runtime.getTimestamp, conf.spanDelay)
        client = conf.client |> Option.defaultWith (fun () -> createClient conf)
        jaegerProcess = createJaegerProcess api.runtime (asTags conf.processTags)
      }

    let tickerOnce = timeOutMillis 2000 ^=> Ch.give ticker

    Job.foreverServer tickerOnce
    >>=. running state

/// Create a new Jaeger target.
///
/// By default uses UDP, so if you're on macOS, run `sudo sysctl net.inet.udp.maxdgram=65536`
/// to bump it. See https://github.com/jaegertracing/jaeger-client-node/issues/124 and
/// https://www.jaegertracing.io/docs/1.13/client-libraries/#emsgsize-and-udp-buffer-limits
/// for details.
///
/// Also see the `JaegerConf` type's docs.
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

type ISecondStep =
  abstract WithProcessAttrs: processAttrs: SpanAttr list -> ISecondStep
  abstract WithPacketSize: packetSize: uint16 -> ISecondStep
  abstract WithTime: flushInterval: Duration * retentionTime: Duration -> ISecondStep
  abstract WithMessageWriter: writer: MessageWriter -> ISecondStep
  abstract WithSampler: sampler: Sampler -> ISecondStep
  abstract Done: unit -> Target.TargetConfBuild<Builder>

and Builder(conf: JaegerConf, callParent: Target.ParentCallback<_>) =
  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  member x.WithJaegerAgent(host, port) =
    Builder({conf with jaegerHost = host; jaegerPort = port}, callParent)
    :> ISecondStep

  interface ISecondStep with
    member x.WithProcessAttrs tags =
      Builder({conf with processTags = tags}, callParent)
      :> ISecondStep

    member x.WithPacketSize batchSize =
      Builder({conf with packetSize = batchSize}, callParent)
      :> ISecondStep

    member x.WithTime (spanDelay: Duration, retentionTime: Duration)  =
      Builder({conf with spanDelay = spanDelay; retentionTime = retentionTime}, callParent)
      :> ISecondStep

    member x.WithMessageWriter mw =
      Builder({conf with messageWriter = mw}, callParent)
      :> ISecondStep

    member x.WithSampler s =
      Builder({ conf with sampler = s }, callParent)
      :> ISecondStep

    member x.Done () =
      ! (callParent x)

  interface Target.SpecificTargetConf with
      member this.Build(name: string): TargetConf =
        create conf name