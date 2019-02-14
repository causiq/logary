namespace Logary.Targets.Jaeger

open System
open Logary
open Logary.Internals
open Logary.Configuration
open Logary.Formatting
open NodaTime
open Hopac
open Hopac.Infixes
open System.Collections.Generic
open Thrift.Transports.Client
open Thrift.Protocols
open System.Threading
open System.Security.Cryptography
open System.Text

type SpanSizeCounter =
  abstract SizeOf: span: Jaeger.Thrift.Span -> int

type SpanSender =
  inherit IDisposable
  abstract SendBatch: batch: Jaeger.Thrift.Batch -> Job<unit>

[<Flags>]
type JaegerSpanFlags = Sampled = 1

[<RequireQualifiedAccess>]
module JaegerTarget =
  
  type JaegerConf = {
    ProcessTags: Map<string,obj>
    /// bytes, spans size reach to this size will be send to jaeger, otherwise it is appended to buffer waitting for flush
    BatchSpanSize: int
    /// flush time interval, every flush time, it will flush buffer span to jaeger
    AutoFlushInterval: Duration
    /// duration to reserve log message, after the time if span doesn't finish, this message/log will be dropped
    RetentionTime: Duration
    Host: string
    Port: int
    /// format message tpl
    MessageWriter: MessageWriter
  }

  let empty = {
    ProcessTags = Map.empty
    BatchSpanSize = 1024 * 1024 // default 1 mb
    AutoFlushInterval = Duration.FromSeconds 10.
    RetentionTime = Duration.FromMinutes 5.
    MessageWriter = MessageWriter.verbatim
    Host = "localhost"
    Port = 6831
  }

  /// create you own protocol if not using compact, e.g. TBinaryProtocol
  let defaultSpanSizeCounter =
    {
      new SpanSizeCounter with
        member x.SizeOf span =
          use counterTransport = new TSizeCountTransport()
          use sizeCountProtocol = new TCompactProtocol(counterTransport)
          span.WriteAsync(sizeCountProtocol, CancellationToken.None).ConfigureAwait(false).GetAwaiter().GetResult()
          counterTransport.Size
    }

  /// create you own protocol if not using compact, e.g. TBinaryProtocol
  let createSpanSender conf =
    let host = conf.Host
    let port = conf.Port
    let paketSize = conf.BatchSpanSize
    let protocol = new TCompactProtocol(new TBufferedClientTransport(new TUdpClientTransport(host,port), paketSize))
    let client = new Jaeger.Thrift.Agent.Agent.Client(protocol)

    let sender = {
      new SpanSender with
        member x.SendBatch batch =
          Hopac.Job.fromUnitTask(fun _ -> client.emitBatchAsync(batch,CancellationToken.None))
        member x.Dispose () =
          client.Dispose()
      }

    sender

  module internal Impl =

    open System.Runtime.CompilerServices

    [<assembly: InternalsVisibleTo("Logary.Targets.Jaeger.Tests")>]
    do()

    type State = {
      spanIdMap: Dictionary<Guid,Instant * Jaeger.Thrift.Log list>
      spanList: ResizeArray<Jaeger.Thrift.Span>
      spanListSize: int
      spanSender: SpanSender
      jaegerProcess: Jaeger.Thrift.Process
      spanSizeCounter: SpanSizeCounter
      batchSpanSize: int
      messageWriter: MessageWriter
     }

    let md5 = MD5.Create ()

    let asIdHighLow str =
      if String.IsNullOrEmpty str then (0L,0L)
      else
        let byteArr =
          match Guid.TryParse str with
          | true, guid ->
            guid.ToByteArray()
          | false, _ ->
            md5.ComputeHash(Encoding.UTF8.GetBytes(str))

        let high = BitConverter.ToInt64(byteArr, 0)
        let low = BitConverter.ToInt64(byteArr, 8)
        high, low

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
      let formatedMsg = messageWriter.format msg
      buildJaegerTag "logger-level" msg.level |> tags.Add
      buildJaegerTag "logger-name" msg.name |> tags.Add

      // gauges will be formated in this msg
      buildJaegerTag "logger-msg" formatedMsg |> tags.Add

      let tagStr = String.Join(",", msg |> Message.getAllTags)
      if tagStr <> String.Empty then buildJaegerTag "tags" tagStr |> tags.Add

      let errors = msg |> Message.getExns
      if errors.Length <> 0 then
        let errorsStr = errors |> Json.formatWith (Logary.Internals.Chiron.Formatting.JsonFormattingOptions.Pretty)
        buildJaegerTag "errors" errorsStr |> tags.Add


      msg.context
      |> Seq.filter (fun (KeyValue (k, _)) -> not (k.StartsWith KnownLiterals.LogaryPrefix || k.StartsWith KnownLiterals.FieldsPrefix))
      |> Seq.iter (fun (KeyValue (k, v)) ->
        let vStr = v |> Json.format
        buildJaegerTag k vStr |> tags.Add
      )

      tags

    let asJaegerLog (messageWriter: MessageWriter) (msg: Message) =
      let timestamp = msg.timestamp / Constants.NanosPerTick / Constants.TicksPerMicro
      let tags = asJaegerTags messageWriter msg
      let jaegerLog = new Jaeger.Thrift.Log(timestamp, tags)
      jaegerLog

    let buildJaegerSpan (messageWriter: MessageWriter) (spanMsg: Message) (spanLog: SpanLog) (childLogs: Jaeger.Thrift.Log list) =
      // time unit: Microseconds
      let startTime = spanLog.beginAt / Constants.TicksPerMicro
      let duration = spanLog.duration / Constants.TicksPerMicro
      // only support sampled for now, and always sampled
      let flags = int JaegerSpanFlags.Sampled
      let operationName = spanMsg.value
      let tags = asJaegerTags messageWriter spanMsg

      let traceIdHigh, traceIdLow = asIdHighLow spanLog.traceId
      let _, spanIdLow = asIdHighLow spanLog.spanId
      let _, parentSpanIdLow = asIdHighLow spanLog.parentSpanId

      let jaegerSpan = new Jaeger.Thrift.Span(traceIdLow, traceIdHigh, spanIdLow, parentSpanIdLow, operationName, flags, startTime, duration)
      // setting reference is not supported for the time being
      // and this can be implement in this target project, as a logary's plugin to extension Message/Logger module
      // jaegerSpan.References <- new ResizeArray<_>()
      jaegerSpan.Logs <- new ResizeArray<_>(childLogs)
      jaegerSpan.Tags <- tags
      jaegerSpan

    let flushToJaeger (logger: Logger) state =
      if state.spanList.Count > 0 then
        let jaegerBatch = new Jaeger.Thrift.Batch(state.jaegerProcess, state.spanList)
        let state' =  { state with spanListSize = 0; spanList = new ResizeArray<_>() }
        let emitBatchJ = state.spanSender.SendBatch jaegerBatch

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

    let appendToBuffer (logger: Logger) spanId spanMsg spanLog childMsgs (state: State) =
      let jaegerSpan = buildJaegerSpan state.messageWriter spanMsg spanLog childMsgs
      let spanSize = state.spanSizeCounter.SizeOf jaegerSpan
      let spanListSize = spanSize + state.spanListSize

      logger.verbose (Message.eventX "{spanSize} / {spanListSize}" >> Message.setFields [|spanSize; spanListSize|])

      state.spanList.Add jaegerSpan
      state.spanIdMap.Remove(spanId) |> ignore

      if spanListSize >= state.batchSpanSize then
        flushToJaeger logger state
      else
        Job.result {state with spanListSize = spanListSize}

    let tryLogSpan (logger: Logger) (msg: Message) (state: State) =
      match msg |> Message.tryGetSpanInfo with
      | Some spanLog ->
        match Guid.TryParse spanLog.spanId with
        | true, spanId ->
          let childLogs =
            match state.spanIdMap.TryGetValue spanId with
            | true, (_, childLogs) -> childLogs
            | false, _ -> []

          let state = appendToBuffer logger spanId msg spanLog childLogs state
          state

        | false, _ ->
          Message.eventX "{spanId} can't be parsed to GUID"
          >> Message.setFields [| spanLog.spanId |]
          |> logger.error

          Job.result state

      | None ->
        match msg |> Message.tryGetSpanId with
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

  open Impl

  let startServe (conf: JaegerConf) spanSender spanSizeCounter (api: TargetAPI) =

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
          |> Hopac.Extensions.Seq.foldFromJob state (fun state  logReq ->
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
          flushToJaegerAndGC logger state conf.RetentionTime >>= running
      ] :> Job<_>


    let jaegerProcess = initProcess api.runtime conf.ProcessTags

    let state = {
      spanIdMap = new Dictionary<Guid,Instant * Jaeger.Thrift.Log list>()
      spanList = new ResizeArray<Jaeger.Thrift.Span>()
      spanListSize = 0
      spanSender = spanSender
      jaegerProcess = jaegerProcess
      spanSizeCounter = spanSizeCounter
      batchSpanSize = conf.BatchSpanSize
      messageWriter = conf.MessageWriter
    }


    let autoFlushJob =
      timeOut (conf.AutoFlushInterval.toTimeSpanSafe()) ^=> Ch.give autoFlushCh

    Job.foreverServer autoFlushJob
    >>=. running state


  [<CompiledName "Create">]
  let create conf name =
    let spanSender = createSpanSender conf
    TargetConf.createSimple (startServe conf spanSender defaultSpanSizeCounter) name

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
      Builder({conf with Host = host; Port = port}, callParent)
      :> ISecondStep

    interface ISecondStep with
      member x.WithTags tags =
        Builder({conf with ProcessTags = tags}, callParent)
        :> ISecondStep

      member x.WithBatchSpanSize batchSize =
        Builder({conf with BatchSpanSize = batchSize}, callParent)
        :> ISecondStep

      member x.WithTime (flushInterval: Duration, retensionTime: Duration )  =
        Builder({conf with AutoFlushInterval = flushInterval; RetentionTime = retensionTime}, callParent)
        :> ISecondStep

      member x.WithMessageWriter mw =
        Builder({conf with MessageWriter = mw}, callParent)
        :> ISecondStep

      member x.Done () =
        ! (callParent x)

    interface Target.SpecificTargetConf with
        member this.Build(name: string): TargetConf =
          create conf name