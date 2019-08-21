module Logary.Targets.Jaeger

open System
open Logary
open Logary.Internals
open Logary.Configuration
open Logary.Formatting
open Logary.Clients.Jaeger
open NodaTime
open Hopac
open Hopac.Infixes
open System.Collections.Generic
open Thrift.Transports.Client
open Thrift.Protocols
open System.Threading
open System.Security.Cryptography

type SpanSizeCounter =
  abstract sizeOf: span: Jaeger.Thrift.Span -> int

type SpanSender =
  inherit IDisposable
  abstract sendBatch: batch: Jaeger.Thrift.Batch -> Job<unit>

[<Flags>]
type JaegerSpanFlags =
  | Sampled = 1
  | Debug = 2

type JaegerConf = {
  processTags: Map<string,obj>
  /// bytes, spans size reach to this size will be send to jaeger, otherwise it is appended to buffer waitting for flush
  batchSpanSize: int
  /// flush time interval, every flush time, it will flush buffer span to jaeger
  autoFlushInterval: Duration
  /// duration to reserve log message, after the time if span doesn't finish, this message/log will be dropped
  retentionTime: Duration
  host: string
  port: int
  /// format message tpl
  messageWriter: MessageWriter
}

let empty = {
  processTags = Map.empty
  batchSpanSize = 1024 * 1024 // default 1 mb
  autoFlushInterval = Duration.FromSeconds 10.
  retentionTime = Duration.FromMinutes 5.
  messageWriter = MessageWriter.verbatim
  host = "localhost"
  port = 6831
}

/// create you own protocol if not using compact, e.g. TBinaryProtocol
let defaultSpanSizeCounter =
  {
    new SpanSizeCounter with
      member x.sizeOf span =
        use counterTransport = new TSizeCountTransport()
        use sizeCountProtocol = new TCompactProtocol(counterTransport)
        span.WriteAsync(sizeCountProtocol, CancellationToken.None).ConfigureAwait(false).GetAwaiter().GetResult()
        counterTransport.Size
  }

/// create you own protocol if not using compact, e.g. TBinaryProtocol
let createSpanSender conf =
  let host = conf.host
  let port = conf.port
  let paketSize = conf.batchSpanSize
  let protocol = new TCompactProtocol(new TBufferedClientTransport(new TUdpClientTransport(host,port), paketSize))
  let client = new Jaeger.Thrift.Agent.Agent.Client(protocol)

  let sender = {
    new SpanSender with
      member x.sendBatch batch =
        Hopac.Job.fromUnitTask(fun _ -> client.emitBatchAsync(batch,CancellationToken.None))
      member x.Dispose () =
        client.Dispose()
    }

  sender

module internal Impl =

  open System.Runtime.CompilerServices

  [<assembly: InternalsVisibleTo("Logary.Targets.Jaeger.Tests")>]
  do()

  type State =
    { spanIdMap: Dictionary<SpanId, Instant * Jaeger.Thrift.Log list>
      spanList: ResizeArray<Jaeger.Thrift.Span>
      spanListSize: int
      spanSender: SpanSender
      jaegerProcess: Jaeger.Thrift.Process
      spanSizeCounter: SpanSizeCounter
      batchSpanSize: int
      messageWriter: MessageWriter
    }

  let md5 = MD5.Create ()

  let buildJaegerTag k (v: obj) =
    let tag = Jaeger.Thrift.Tag()
    tag.Key <- k

    match v with
    | :? string as v ->
      tag.VStr <- v
      tag.VType <- Jaeger.Thrift.TagType.STRING
    | :? bool as v ->
      tag.VBool <- v
      tag.VType <- Jaeger.Thrift.TagType.BOOL
    | :? array<byte> as v ->
      tag.VBinary <- v
      tag.VType <- Jaeger.Thrift.TagType.BINARY
    | _ ->
      tag.VStr <- Convert.ToString(v)
      tag.VType <- Jaeger.Thrift.TagType.STRING

    tag

  let initProcess (runtime:RuntimeInfo) (processTags: Map<string, obj>) =
    let serviceName = runtime.service
    let host = runtime.host

    let p = Jaeger.Thrift.Process (serviceName)

    let tags =
      processTags |> Map.add "host" (box host)
      |> Map.fold (fun s k v ->
        buildJaegerTag k v :: s
       ) []
      |> ResizeArray

    p.Tags <- tags
    p

  let asJaegerTags (messageWriter: MessageWriter) (msg: Message) =
    let tags = new ResizeArray<_>()
    let fmtMsg = messageWriter.format msg
    buildJaegerTag "logger-level" msg.level |> tags.Add
    buildJaegerTag "logger-name" msg.name |> tags.Add

    // gauges will be formatted in this msg
    buildJaegerTag "logger-msg" fmtMsg |> tags.Add

    let tagStr = String.Join(",", msg |> Message.getAllTags)
    if tagStr <> String.Empty then buildJaegerTag "tags" tagStr |> tags.Add

    let errors = msg |> Message.getExns
    if errors.Length <> 0 then
      let errorsStr = errors |> Json.encodeFormatWith (Logary.Internals.Chiron.Formatting.JsonFormattingOptions.Pretty)
      buildJaegerTag "errors" errorsStr |> tags.Add


    msg.context
    |> Seq.filter (fun (KeyValue (k, _)) -> not (k.StartsWith KnownLiterals.LogaryPrefix || k.StartsWith KnownLiterals.FieldsPrefix))
    |> Seq.iter (fun (KeyValue (k, v)) ->
      let vStr = v |> Json.encodeFormat
      buildJaegerTag k vStr |> tags.Add
    )

    tags

  let asJaegerLog (messageWriter: MessageWriter) (msg: Message) =
    let timestamp = msg.timestamp / Constants.NanosPerTick / Constants.TicksPerMicro
    let tags = asJaegerTags messageWriter msg
    let jaegerLog = new Jaeger.Thrift.Log(timestamp, tags)
    jaegerLog

  let buildJaegerSpan (messageWriter: MessageWriter) (spanMsg: Message) (s: SpanData) (childLogs: Jaeger.Thrift.Log list) =
    // time unit: Microseconds
    let startTime = s.started / Constants.TicksPerMicro
    let duration = s.elapsed / Constants.TicksPerMicro
    // only support sampled for now, and always sampled
    let flags = int JaegerSpanFlags.Sampled
    let operationName = spanMsg.value
    let tags = asJaegerTags messageWriter spanMsg

    let parent = s.parentSpanId |> Option.map (fun x -> x.id) |> Option.defaultValue 0L

    // https://github.com/jaegertracing/jaeger-client-java/blob/master/jaeger-core/src/test/java/io/jaegertracing/internal/JaegerSpanTest.java#L254-L265
    let jaegerSpan =
      new Jaeger.Thrift.Span(
        s.context.traceId.low, s.context.traceId.high, s.context.spanId.id,
        parent, operationName, flags, startTime, duration.ToInt64Nanoseconds() / 1000L)

    // setting reference is not supported for the time being
    // and this can be implement in this target project, as a Logary's plugin to extension Message/Logger module
    // jaegerSpan.References <- new ResizeArray<_>()
    jaegerSpan.Logs <- new ResizeArray<_>(childLogs)
    jaegerSpan.Tags <- tags
    jaegerSpan

  let flushToJaeger (logger: Logger) state =
    if state.spanList.Count > 0 then
      let jaegerBatch = new Jaeger.Thrift.Batch(state.jaegerProcess, state.spanList)
      let state' =  { state with spanListSize = 0; spanList = new ResizeArray<_>() }
      let emitBatchJ = state.spanSender.sendBatch jaegerBatch

      Job.tryIn emitBatchJ (fun _ -> Job.result state' ) (fun exn ->
        Message.eventX "flush to jaeger failed"
        >> Message.addExn exn
        |> logger.error

        Job.result state'
      )
    else Job.result state

  let flushToJaegerAndGC (logger: Logger) state retentionTime =
    flushToJaeger logger state >>- (fun state ->
      let now = SystemClock.Instance.GetCurrentInstant()
      state.spanIdMap
      |> Seq.filter (fun (KeyValue(_, (firstOccurTime, _))) -> now - firstOccurTime > retentionTime)
      |> Seq.map (fun (KeyValue(k,_)) -> k)
      |> Seq.toList
      |> List.iter (fun k -> state.spanIdMap.Remove k |> ignore)

      state
    )

  let appendToBuffer (logger: Logger) spanMsg (data: SpanData) childMsgs (state: State) =
    let jaegerSpan = buildJaegerSpan state.messageWriter spanMsg data childMsgs
    let spanSize = state.spanSizeCounter.sizeOf jaegerSpan
    let spanListSize = spanSize + state.spanListSize

    logger.verbose (Message.eventX "{spanSize} / {spanListSize}" >> Message.setFields [|spanSize; spanListSize|])

    state.spanList.Add jaegerSpan
    state.spanIdMap.Remove(data.context.spanId) |> ignore

    if spanListSize >= state.batchSpanSize then
      flushToJaeger logger state
    else
      Job.result {state with spanListSize = spanListSize}

  let tryLogSpan (logger: Logger) (msg: Message) (state: State) =
    match Message.tryGetSpanData msg with
    | Some data ->
      let childLogs =
        match state.spanIdMap.TryGetValue data.context.spanId with
        | true, (_, childLogs) -> childLogs
        | false, _ -> []

      let state = appendToBuffer logger msg data childLogs state
      state

    | None ->
      match Message.tryGetSpanId msg with
      | Some spanId ->
        // append this message wait for some span to occur, these messages will be the span's logs
        let appendedLogs =
          match state.spanIdMap.TryGetValue spanId with
          | true, (instant, logs) ->
            let log = asJaegerLog state.messageWriter msg
            instant, log :: logs
          | false, _ -> SystemClock.Instance.GetCurrentInstant(), [asJaegerLog state.messageWriter msg]

        state.spanIdMap.[spanId] <- appendedLogs

        Job.result state
      | None ->
        // Drop this message, this can be avoid by config a pipeline processing to sink message to this target if has spanid
        Job.result state

  let loop (conf: JaegerConf) spanSender spanSizeCounter (api: TargetAPI) =

    let logger = api.runtime.logger
    let always = Alt.unit ()

    let autoFlushCh = Ch ()

    let rec running (state: State) =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          flushToJaeger logger state >>= fun state ->
            state.spanIdMap.Clear()
            state.spanSender.Dispose()
            ack *<= ()

        RingBuffer.takeAll api.requests ^=> fun logReqMsg ->
          logReqMsg
          |> Hopac.Extensions.Seq.foldFromJob state (fun state logReq ->
            match logReq with
            | Flush (ack, nack) ->
              let flushState =
                always ^=> (fun _ -> flushToJaeger logger state >>= (fun state -> IVar.fill ack () >>-. state))

              ((nack ^->. state) <|> flushState) :> Job<_>

            | Log (msg, ack) ->
              let tryLogSpanJ = tryLogSpan logger msg state
              let tryLogSpanSafe =  Job.tryWith tryLogSpanJ (fun exn ->

                Message.eventX "tryLogSpan failed. Drop msg : {msg}"
                >> Message.setFields [| msg |]
                >> Message.addExn exn
                |> logger.error

                Job.result state
              )

              tryLogSpanSafe >>= (fun state -> IVar.fill ack () >>-. state)
          )
          >>= running


        autoFlushCh ^=> fun _ ->
          flushToJaegerAndGC logger state conf.retentionTime >>= running
      ] :> Job<_>


    let jaegerProcess = initProcess api.runtime conf.processTags

    let state = {
      spanIdMap = new Dictionary<SpanId, Instant * Jaeger.Thrift.Log list>()
      spanList = new ResizeArray<Jaeger.Thrift.Span>()
      spanListSize = 0
      spanSender = spanSender
      jaegerProcess = jaegerProcess
      spanSizeCounter = spanSizeCounter
      batchSpanSize = conf.batchSpanSize
      messageWriter = conf.messageWriter
    }


    let autoFlushJob =
      timeOut (conf.autoFlushInterval.toTimeSpanSafe()) ^=> Ch.give autoFlushCh

    Job.foreverServer autoFlushJob
    >>=. running state


/// Create a new Jaeger target.
[<CompiledName "Create">]
let create conf name =
  let spanSender = createSpanSender conf
  TargetConf.createSimple (Impl.loop conf spanSender defaultSpanSizeCounter) name

type ISecondStep =
    abstract WithTags: Map<string,obj> -> ISecondStep
    abstract WithBatchSpanSize: int -> ISecondStep
    abstract WithTime: Duration * Duration -> ISecondStep
    abstract WithMessageWriter: MessageWriter -> ISecondStep
    abstract Done: unit -> Target.TargetConfBuild<Builder>
and Builder(conf: JaegerConf, callParent: Target.ParentCallback<_>) =

  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  member x.WithJaegerClient(host, port) =
    Builder({conf with host = host; port = port}, callParent)
    :> ISecondStep

  interface ISecondStep with
    member x.WithTags tags =
      Builder({conf with processTags = tags}, callParent)
      :> ISecondStep

    member x.WithBatchSpanSize batchSize =
      Builder({conf with batchSpanSize = batchSize}, callParent)
      :> ISecondStep

    member x.WithTime (flushInterval: Duration, retensionTime: Duration )  =
      Builder({conf with autoFlushInterval = flushInterval; retentionTime = retensionTime}, callParent)
      :> ISecondStep

    member x.WithMessageWriter mw =
      Builder({conf with messageWriter = mw}, callParent)
      :> ISecondStep

    member x.Done () =
      ! (callParent x)

  interface Target.SpecificTargetConf with
      member this.Build(name: string): TargetConf =
        create conf name