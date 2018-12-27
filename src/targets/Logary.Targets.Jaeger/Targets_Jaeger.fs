namespace Logary.Targets.Jaeger

open System
open Logary
open Logary.Internals
open Logary.Configuration
open NodaTime
open Hopac
open Hopac.Infixes
open System.Collections.Generic
open Thrift.Transports.Client
open Thrift.Protocols
open System.Threading
open Hopac

type SpanSizeCounter =
  abstract SizeOf: span: Jaeger.Thrift.Span -> int

module JaegerTarget =

  type JaegerConf =
    {
      ProcessTags: Map<string,obj>
      /// bytes, spans size reach to this size will be send to jaeger, otherwise it is appended to buffer waitting for flush
      BatchSpanSize: int
      /// flush time interval, every flush time, it will flush buffer span to jaeger
      FlushSpanTime: Duration
      /// duration to reserve log message, after the time if span doesn't finish, this message/log will be dropped
      RetentionTime: Duration
      Host: string
      Port: int
    }

  let empty = {
    ProcessTags = Map.empty
    BatchSpanSize = 1024 * 1024 // default 1 mb
    FlushSpanTime = Duration.FromSeconds 10.
    RetentionTime = Duration.FromMinutes 5.
    Host = "localhost"
    Port = 6831
  }

  /// create you own protocol if not using compact, e.g. TBinaryProtocol
  let createJaegerAgentClient conf =
    let host = conf.Host
    let port = conf.Port
    let paketSize = conf.BatchSpanSize
    let protocol = new TCompactProtocol(new TBufferedClientTransport(new TUdpClientTransport(host,port), paketSize))
    let client = new Jaeger.Thrift.Agent.Agent.Client(protocol)
    let sizeCounter = { new SpanSizeCounter with
        member x.SizeOf span =
          use counterTransport = new TSizeCountTransport()
          use sizeCountProtocol = new TCompactProtocol(counterTransport)
          span.WriteAsync(sizeCountProtocol, CancellationToken.None).ConfigureAwait(false).GetAwaiter().GetResult()
          counterTransport.Size
      }

    client, sizeCounter


  // type IThirdStep =
  //   abstract SetConnectTimeOut: int -> IThirdStep
  //   abstract SetReadWriteTimeOut: int -> IThirdStep
  //   abstract Done: unit -> Target.TargetConfBuild<Builder>
  // and ISecondStep =
  //   abstract ConfLogLocation: string * string -> IThirdStep
  // and Builder(conf: JaegerConf, callParent: Target.ParentCallback<_>) =

  //   new(callParent: Target.ParentCallback<_>) =
  //     Builder(empty, callParent)

  //   member x.ConfClient(key, keyId, endpoint) =
  //     Builder({conf with AccessKey = key; AccessKeyId = keyId; Endpoint = endpoint}, callParent)
  //     :> ISecondStep

  //   interface ISecondStep with
  //     member x.ConfLogLocation(project, logstore) =
  //       Builder({conf with Project = project; Logstore = logstore}, callParent)
  //       :> IThirdStep

  //   interface IThirdStep with
  //     member x.SetConnectTimeOut(timoutMS) =
  //       Builder({conf with ClientConnectTimeout = timoutMS}, callParent) :> IThirdStep

  //     member x.SetReadWriteTimeOut(timoutMS) =
  //       Builder({conf with ClientReadWriteTimeout = timoutMS}, callParent) :> IThirdStep

  //     member x.Done () =
  //       ! (callParent x)

  //   interface Target.SpecificTargetConf with
  //       member this.Build(name: string): TargetConf =
  //         if String.IsNullOrEmpty conf.AccessKey || String.IsNullOrEmpty conf.AccessKeyId || String.IsNullOrEmpty conf.Endpoint then
  //           failwith "accesskey keyId endpoint is needed"
  //         else
  //           create conf name


  module internal Impl =

    type State = { 
      spanIdMap: Dictionary<Guid,Instant * Message list>
      spanList: ResizeArray<Jaeger.Thrift.Span>
      spanListSize: int
      jaegerAgentClient: Jaeger.Thrift.Agent.Agent.Client
      jaegerProcess: Jaeger.Thrift.Process
      spanSizeCounter: SpanSizeCounter
      batchSpanSize: int
     }

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


    let ofEpochLocal epoch =
      (DateTimeOffset.ofEpoch epoch).ToLocalTime()

    let buildJaegerSpan (spanMsg: Message) (spanLog: SpanLog) (childMsgs: Message list) =
      let jaegerSpan = new Jaeger.Thrift.Span(1,2,3,4,"op",1,2,3)
      jaegerSpan.References <- ()
      jaegerSpan.Logs <- ()
      jaegerSpan.Tags <- ()
      jaegerSpan.Duration <- ()
      jaegerSpan


    let flushToJaeger state =
      let jaegerBatch =Jaeger.Thrift.Batch(state.jaegerProcess, state.spanList)
      Hopac.Job.fromUnitTask (fun _ -> state.jaegerAgentClient.emitBatchAsync(jaegerBatch, CancellationToken.None))
      >>- fun _ ->
        { state with spanListSize = 0; spanList = new ResizeArray<_>() }

    let appendToBuffer spanMsg spanLog childMsgs (state: State) =
      let jaegerSpan = buildJaegerSpan spanMsg spanLog childMsgs
      let spanSize = state.spanSizeCounter.SizeOf jaegerSpan
      let spanListSize = spanSize + state.spanListSize
      state.spanList.Add jaegerSpan

      if spanListSize >= state.batchSpanSize then
        flushToJaeger state
      else
        Job.result {state with spanListSize = spanListSize}

    let tryLogSpan (logger: Logger) (msg: Message) (state: State) = 
      match msg |> Message.tryGetSpanInfo with
      | Some spanLog ->
        match Guid.TryParse spanLog.spanId with
        | true, spanId ->
          let childMsgs =
            match state.spanIdMap.TryGetValue spanId with
            | true, (_, childMsgs) -> childMsgs
            | false, _ -> []
          
          let state = appendToBuffer msg spanLog childMsgs state
          Job.result state

        | false, _ -> 
          Message.eventX "{spanId} can't be parsed to GUID"
          >> Message.setFields [| spanLog.spanId |]
          |> logger.error

          Job.result state

      | None -> 
        match msg |> Message.tryGetSpanId with
        | Some spanId ->
          // append this message wait for some span to occur, these messages will be the span's logs
          let appendedMsgs = 
            match state.spanIdMap.TryGetValue spanId with
            | true, (instant, msgs) -> instant, msg :: msgs
            | false, _ -> SystemClock.Instance.GetCurrentInstant(), [msg]

          state.spanIdMap.[spanId] <- appendedMsgs

          Job.result state
        | None ->  
          // Drop this message, this can be avoid by config a pipeline processing to sink message to this target if has spanid
          Job.result state


    let startServe (conf: JaegerConf) (api: TargetAPI) =

      let logger = api.runtime.logger
      let always = Alt.unit ()

      let rec running (state: State) =
        Alt.choose [
          api.shutdownCh ^=> fun ack ->
            flushToJaeger state >>= fun state -> 
              state.jaegerAgentClient.Dispose()
              ack *<= ()

          RingBuffer.takeAll api.requests ^=> fun logReqMsg ->
            logReqMsg 
            |> Hopac.Extensions.Seq.foldFromJob state (fun state  logReq ->
              match logReq with
              | Flush (ack, nack) -> 
                let flushState =
                  always ^=> (fun _ -> flushToJaeger state >>= (fun state -> IVar.fill ack () >>-. state))

                ((nack ^->. state) <|> flushState) :> Job<_>

              | Log (msg, ack) ->
                tryLogSpan logger msg state >>= (fun state -> IVar.fill ack () >>-. state)
            )
            >>= running
        ] :> Job<_>


      let jaegerProcess = initProcess api.runtime conf.ProcessTags
      let jaegerAgentClient, spanSizeCounter = createJaegerAgentClient conf

      let state = {
        spanIdMap = new Dictionary<Guid,Instant * Message list>()
        spanList = new ResizeArray<Jaeger.Thrift.Span>()
        spanListSize = 0
        jaegerAgentClient = jaegerAgentClient
        jaegerProcess = jaegerProcess
        spanSizeCounter = spanSizeCounter
        batchSpanSize = conf.BatchSpanSize
      }

      running state // state


  /// Create a new YOUR TARGET NAME HERE target
  [<CompiledName "Create">]
  let create conf name =
    TargetConf.createSimple (Impl.startServe conf) name
