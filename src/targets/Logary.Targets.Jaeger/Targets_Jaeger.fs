module Logary.Targets.AliYun

open System
open Logary
open Logary.Internals
open Logary.Configuration
open NodaTime
open Hopac
open Hopac.Infixes
open System.Collections.Generic

type JaegerConf =
  {
    ProcessTags: Map<string,obj>
    /// bytes
    PaketSize: int
    /// duration to reserve log message, after the time if span doesn't finish, this message/log will be dropped
    RetentionTime: Duration
  }

let empty = {
  ProcessTags = Map.empty;
  PaketSize = 1024 * 1024 // default 1 mb
  RetentionTime = Duration.FromMinutes 5.
}


module internal Impl =

  open Jaeger.Thrift

  type State = { 
    spanIdMap: Dictionary<Guid,Instant * Message list>
    spanList: ResizeArray<Span>
   }


  let buildTag k (v: obj) =
    let tag = Tag()
    tag.Key <- k

    match v with
    | :? string as v ->
      tag.VStr <- v
      tag.VType <- TagType.STRING
    | :? bool as v ->
      tag.VBool <- v
      tag.VType <- TagType.BOOL
    | :? array<byte> as v ->
      tag.VBinary <- v
      tag.VType <- TagType.BINARY
    | _ -> 
      tag.VStr <- Convert.ToString(v)
      tag.VType <- TagType.STRING
    
    tag

  let initProcess (runtime:RuntimeInfo) (processTags: Map<string, obj>) =
    let serviceName = runtime.service
    let host = runtime.host

    let p = Process (serviceName)

    let tags = 
      processTags |> Map.add "host" (box host)
      |> Map.fold (fun s k v ->
        buildTag k v :: s
       ) []
      |> ResizeArray

    p.Tags <- tags
    p

  let buildSpan message =
    message

  let reqestAckJobCreator request =
    match request with
    | Log (msg, ack) ->
      ack *<= ()
    | Flush (ackCh, nack) ->
      ackCh *<= ()

  let ofEpochLocal epoch =
    (DateTimeOffset.ofEpoch epoch).ToLocalTime()


  let sendToJaeger jProcess jSpan =
    let b = Batch(jProcess, jSpan)
    b

  

  let add (logger: Logger) msg (state: State) = 
    match msg |> Message.tryGetSpanInfo with
    | Some spanLog ->
      // generate span and its logs
      // send to jaeger collector if reach its packsize

      match Guid.TryParse spanLog.spanId with
      | true, spanId ->
        let msgs =
          match state.spanIdMap.TryGetValue spanId with
          | true, (_, msgs) -> msgs
          | false, _ -> []



        do ()
      | false, _ -> 
        Message.eventX "{spanId} can't be parsed to GUID"
        >> Message.setFields [| spanLog.spanId |]
        |> logger.error
        do ()

    | None -> 
      match msg |> Message.tryGetSpanId with
      | Some spanId ->
        // do append message as log
        let appendedMsgs = 
          match state.spanIdMap.TryGetValue spanId with
          | true, (instant, msgs) -> instant, msg :: msgs
          | false, _ -> SystemClock.Instance.GetCurrentInstant(), [msg]

        state.spanIdMap.[spanId] <- appendedMsgs
      | None ->  
        // Drop this message, this can be avoid by config a pipeline processing to sink message to this target if has spanid
        do ()


  let startServe (conf: JaegerConf) (api: TargetAPI) =

    let logger = api.runtime.logger
    let jaegerProcess = initProcess api.runtime conf.ProcessTags


    let rec running state =
      Alt.choose [
        // maybe because 200ms timeout when shutdown/flush target, so we should not send log to client, just dispose resource and ack ?
        api.shutdownCh ^=> fun ack ->
          let msg = Message.eventInfo "application shut down logging"
          // flush
          ack *<= ()

        RingBuffer.takeAll api.requests ^=> fun logReqMsg ->
          // send log
          running state
      ] :> Job<_>

    running [] // state



/// Create a new YOUR TARGET NAME HERE target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.startServe conf) name


type IThirdStep =
  abstract SetConnectTimeOut: int -> IThirdStep
  abstract SetReadWriteTimeOut: int -> IThirdStep
  abstract Done: unit -> Target.TargetConfBuild<Builder>
and ISecondStep =
  abstract ConfLogLocation: string * string -> IThirdStep
and Builder(conf: AliYunConf, callParent: Target.ParentCallback<_>) =

  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  member x.ConfClient(key, keyId, endpoint) =
    Builder({conf with AccessKey = key; AccessKeyId = keyId; Endpoint = endpoint}, callParent)
    :> ISecondStep

  interface ISecondStep with
    member x.ConfLogLocation(project, logstore) =
      Builder({conf with Project = project; Logstore = logstore}, callParent)
      :> IThirdStep

  interface IThirdStep with
    member x.SetConnectTimeOut(timoutMS) =
      Builder({conf with ClientConnectTimeout = timoutMS}, callParent) :> IThirdStep

    member x.SetReadWriteTimeOut(timoutMS) =
      Builder({conf with ClientReadWriteTimeout = timoutMS}, callParent) :> IThirdStep

    member x.Done () =
      ! (callParent x)

  interface Target.SpecificTargetConf with
      member this.Build(name: string): TargetConf =
        if String.IsNullOrEmpty conf.AccessKey || String.IsNullOrEmpty conf.AccessKeyId || String.IsNullOrEmpty conf.Endpoint then
          failwith "accesskey keyId endpoint is needed"
        else
          create conf name


