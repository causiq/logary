module Logary.Targets.Jaeger

open Hopac
open Hopac.Infixes
open Jaeger.Thrift
open NodaTime
open System
open System.Collections.Generic
open System.Runtime.InteropServices
open Thrift.Protocols
open Thrift.Transports.Client
open Logary
open Logary.Message
open Logary.Configuration
open Logary.Formatting
open Logary.Internals
open Logary.Internals.Chiron.Formatting
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

    sampler: Sampler
  }

let empty =
  // TODO: samplers
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
    messageWriter = MessageWriter.verbatim
    client = None
    spanDelay = Duration.FromSeconds 5L
    sampler = new ConstSampler(true)
  }

module internal Impl =
  open MessagePatterns
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

  type SpanAttrValue with
    member x.writeTo (tag: Tag) =
      match x with
      | SpanAttrValue.B v ->
        tag.VBool <- v
        tag.VType <- TagType.BOOL
      | SpanAttrValue.S s ->
        tag.VStr <- s
        tag.VType <- TagType.STRING
      | SpanAttrValue.V v ->
        match v with
        | Float f ->
          tag.VDouble <- f
          tag.VType <- TagType.DOUBLE
        | BigInt i ->
          tag.VDouble <- float i
          tag.VType <- TagType.DOUBLE
        | Int64 i ->
          tag.VLong <- i
          tag.VType <- TagType.LONG
        | Fraction _ ->
          tag.VDouble <- v.toFloat()
          tag.VType <- TagType.DOUBLE

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

  let toTag ((k, v): SpanAttr) =
    let tag = Jaeger.Thrift.Tag(Key = k)
    v.writeTo tag
    tag

  let objToTag (k: string, v: obj) =
    match v with
    | :? string as s ->
      toTag (k, SpanAttrValue.S s) |> Some
    | :? bool as b ->
      toTag (k, SpanAttrValue.B b) |> Some
    | :? Value as v ->
      toTag (k, SpanAttrValue.V v) |> Some
    | :? float as f ->
      toTag (k, SpanAttrValue.V (Float f)) |> Some
    | :? int as i ->
      toTag (k, SpanAttrValue.V (Int64 (int64 i))) |> Some
    | :? int64 as i ->
      toTag (k, SpanAttrValue.V (Int64 i)) |> Some
    | :? bigint as bi ->
      toTag (k, SpanAttrValue.V (BigInt bi)) |> Some
    | null ->
      None
    | o ->
      toTag (k, SpanAttrValue.S (o.ToString())) |> Some

  let createJaegerProcess (runtime: RuntimeInfo) (processTags: SpanAttr list) =
    let p = Jaeger.Thrift.Process (runtime.service)
    p.Tags <-
      [ "hostname", SpanAttrValue.S runtime.host
        "logary.version", SpanAttrValue.S AssemblyVersionInformation.AssemblyVersion
      ] @ processTags
      |> List.map toTag
      |> ResizeArray
    p

  type Message with
    member x.getJaegerTags (mw: MessageWriter) =
      let str s = SpanAttrValue.S s
      seq {
        yield ("event", str <| mw.format x) |> toTag
        yield ("level", str <| x.level.ToString()) |> toTag
        yield ("component", str <| x.name.ToString()) |> toTag
        for kv in x.context do
          match kv with
          | Gauge _
          | Intern _ -> ()
          | Field (k, v)
          | Context (k, v) ->
            match objToTag (k, v) with
            | None -> ()
            | Some tag -> yield tag

          | Tags tags ->
            for tag in tags |> Set.toSeq do
              yield (tag, SpanAttrValue.B true) |> toTag

          | Exns errors ->
            yield ("error", SpanAttrValue.B true) |> toTag
            yield ("errors", errors |> Json.encodeFormatWith (JsonFormattingOptions.Pretty) |> str) |> toTag
      }
      |> ResizeArray

    member x.getJaegerLog (mw: MessageWriter) =
      let jaegerTags = x.getJaegerTags mw
      new Jaeger.Thrift.Log(x.timestamp / 1000L, jaegerTags)

  /// https://github.com/jaegertracing/jaeger-client-java/blob/master/jaeger-core/src/test/java/io/jaegertracing/internal/JaegerSpanTest.java#L254-L265
  let buildJaegerSpan (mw: MessageWriter) (msg: Message, span: SpanData, samplerAttrs: SpanAttr list, ambientLogs: Jaeger.Thrift.Log list) =
    let parent = span.parentSpanId |> Option.map (fun x -> x.id) |> Option.defaultValue 0L
    let jaegerSpan =
      new Jaeger.Thrift.Span(
        span.context.traceId.low, span.context.traceId.high, span.context.spanId.id,
        parent,
        msg.value,
        int span.flags,
        // time unit: Microseconds (µs) for both the epoch timestamp and the duration
        span.started / 1000L,
        (span.finished - span.started) / 1000L)

    let logs = ResizeArray<_>(span.events.Count + ambientLogs.Length)
    Seq.concat [|
      span.events |> Seq.map (fun m -> m.getJaegerLog mw)
      ambientLogs :> _
    |]
    |> logs.AddRange

    // setting reference is not supported for the time being
    // and this can be implement in this target project, as a Logary's plugin to extension Message/Logger module
    // jaegerSpan.References <- new ResizeArray<_>()
    jaegerSpan.Logs <- logs
    let tags = ResizeArray<_>(msg.getJaegerTags mw)
    tags.AddRange (samplerAttrs |> List.map toTag)
    jaegerSpan.Tags <- tags
    jaegerSpan

  /// When an ambient log `Message` is received, it means it is *not* carrying a `SpanData` instance in its `.context`,
  /// and hence must be either added to an existing deferred `Span`, OR be pushed to the `ambient` `ListCache` in
  /// the state, waiting for a `Span` to be completed and later flushed to the Jaeger client.
  let handleAmbientLog (msg: Message, conf, ri: RuntimeInfo, state: State) (spanId: SpanId): State =
    // First convert the message to a Jaeger log message
    let log = msg.getJaegerLog conf.messageWriter

    // Now, see if there's a matching deferred `Span`, that we can add the `Log` to;
    let wasAdded =
      state.deferred.tryModify(spanId, fun (span, ack) ->
        ri.logger.verbose (eventX "handleAmbientLog found deferred Span={spanId}, saving `Log`:s into it" >> setField "spanId" spanId)
        span.Logs.Add log
        span, ack)

    // Otherwise, if there wasn't, we'll keep track of the ambient log `Message` for `conf.retentionTime`. During the
    // `retentionTime` Duration, either a `Span` will be logged and then flushed and the `ambient` `Log` value associated with it,
    // OR the `ambient` `Log` values will be garbage collected.
    if not wasAdded then
      ri.logger.verbose (eventX "handleAmbientLog did not find deferred Span, enqueueing Log as ambient")
      state.ambient.enqueue(spanId, log)

    state

  let handleSpan (conf, ri: RuntimeInfo) (msg: Message, span: SpanData, samplerAttrs: SpanAttr list, ack: IVar<unit>) (state: State): State =
    let ambientLogs = state.ambient.tryGetAndRemove span.context.spanId
    let jaegerSpan = buildJaegerSpan conf.messageWriter (msg, span, samplerAttrs, ambientLogs)

    ri.logger.verbose (eventX "handleSpan deferring Span={spanId}" >> setField "spanId" span.context.spanId)
    state.deferred.defer(span.context.spanId, (jaegerSpan, ack))
    state

  /// Selects immediately, commits on the completion of `client.send`'s Alt return value, and if NACK:ed,
  /// automatically cancels the client's send operation without cleaning up.
  let flushSpans (ri: RuntimeInfo, client: Client, proc: Process) (spanKVPs: KeyValuePair<SpanId, Jaeger.Thrift.Span * IVar<unit>> list): Alt<unit> =
    if List.isEmpty spanKVPs then Alt.unit () else
    ri.logger.verbose (eventX "flushSpans is flushing {count} Spans" >> setField "count" spanKVPs.Length)

    let spans = ResizeArray<_>(spanKVPs.Length)

    spanKVPs
      |> Seq.map (fun (KeyValue (_, (span, _))) -> span)
      |> spans.AddRange

    let ackAll =
      spanKVPs
        |> Seq.map (fun (KeyValue (_, (_, ack))) -> ack *<= ())
        |> Job.conIgnore

    let jaegerBatch = new Jaeger.Thrift.Batch(proc, spans)

    Alt.tryIn
      (client.send jaegerBatch)
      (fun () -> ackAll)
      (fun exn ->
        ri.logger.error (Message.eventX "Sending to Batch to Jaeger Agent failed" >> Message.addExn exn)
        ackAll)

  let loop (conf: JaegerConf) (api: TargetAPI) =
    api.runtime.logger.info (
      eventX "Started Jaeger target with endpoint {host}:{port}"
      >> setField "host" conf.jaegerHost
      >> setField "port" conf.jaegerPort)

    let ticker, dueToSend = Ch (), Ch ()

    let rec running (state: State) =
      Alt.choosy [|
        api.shutdownCh ^=> fun ack ->
          api.runtime.logger.verbose (eventX "Shutting down the Jaeger target and flushing {count} deferred Spans..." >> setField "count" state.deferred.count)
          flushSpans (api.runtime, state.client, state.jaegerProcess) (state.deferred.dequeueAll()) >>= fun () ->
            dispose state.ambient
            dispose state.deferred
            dispose state.client
            api.runtime.logger.verbose (eventX "Shutting down the Jaeger target.")
            ack *<= ()

        dueToSend ^=>
          (flushSpans (api.runtime, state.client, state.jaegerProcess)
           >=> fun () -> running state)

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            // We either get messages with SpanData in them, we get ambient log messages with SpanId in them,
            // or we get log messages with no SpanId:s in them.
            match Message.tryGetSpanData message with
            | Some span ->
              match conf.sampler.shouldSample span with
              | Ok attrs ->
                handleSpan (conf, api.runtime) (message, span, attrs, ack) state
                |> running
              | Result.Error () ->
                running state

            | None ->
              // Messages without SpanId:s are dropped. This can be avoided by configuring a pipeline processing
              // Pipe to only add Jaeger as a sink if the message has a SpanId attached.
              let nextState =
                Message.tryGetSpanId message
                |> Option.map (handleAmbientLog (message, conf, api.runtime, state))
                |> Option.defaultValue state

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
            api.runtime.logger.verbose (
              eventX "Garbage-collected {count} ambient logs with {spanIds}"
              >> setField "count" gced.Count
              >> setField "spanIds" gced)

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
        jaegerProcess = createJaegerProcess api.runtime conf.processTags
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