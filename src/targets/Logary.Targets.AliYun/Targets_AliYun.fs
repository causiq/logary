module Logary.Targets.AliYun

// Ignore deprecations — if this target breaks it's removed from the code base
#nowarn "44"

open System
open Logary
open Logary.Internals
open Logary.Message
open Logary.Configuration
open Logary.Formatting.Literate

type AliYunConf = 
  {
    AccessKeyId: string
    AccessKey: string
    Endpoint: string   
    ClientConnectTimeout: int  
    ClientReadWriteTimeout: int   
    Project: string
    Logstore: string
  }
  
let empty = {
  AccessKey = "";
  AccessKeyId = "";
  Endpoint = "";
  ClientConnectTimeout = 5000;
  ClientReadWriteTimeout = 10000;
  Project = "";
  Logstore = "";
}


type State = { logClient: Aliyun.Api.LOG.LogClient }

module internal Impl =
  open Hopac
  open Hopac.Infixes
  open Hopac.Extensions
  open Aliyun.Api.LOG.Request;
  open Aliyun.Api.LOG.Data;
  open NodaTime


  let reqestAckJobCreator request =
    match request with
    | Log (msg, ack) ->
      ack *<= ()
    | Flush (ackCh, nack) ->
      ackCh *<= ()

  let ofEpochLocal epoch = 
    (DateTimeOffset.ofEpoch epoch).ToLocalTime()

  let transToAliLogItem hostName logtime request =
    let log = LogItem()
    // time must set , can be anytime
    log.Time <- logtime 
    log.PushBack("Host",hostName)
    match request with
    | Log(msg,ack) ->
        log.PushBack("TimeOffSet",string (ofEpochLocal msg.timestamp))
        log.PushBack("Level",string msg.level)
        log.PushBack("LoggerName",string msg.name)
        let formated =  MessageWriter.verbatim.format msg
        log.PushBack("Msg",formated)
        if formated <> msg.value then log.PushBack("MsgTemplate",msg.value)
        msg |> Message.getAllGauges
        |> Seq.iter (fun (gaugeType, Gauge(value, units)) -> 
           let symbol = Units.symbol units
           let gaugeType = sprintf "%s (%s)" gaugeType symbol
           log.PushBack(gaugeType, value.ToString()))
        
        let context = MessageWriter.contextWriter.format msg
        log.PushBack("_ctx-all", context)
        
    | Flush (ack,nack) ->
        log.PushBack("TimeOffSet",string DateTimeOffset.Now)
        log.PushBack("Level",string LogLevel.Info)
        log.PushBack("LoggerName","Logary.AliYun.FlushLog")
        log.PushBack("Msg","application flush log")

    log

  let loop (conf: AliYunConf)
           (runtime: RuntimeInfo, api: TargetAPI) =
    
    let logger = 
      let pn = PointName [| "Logary"; "AliYun"; "loop" |]
      Logger.apply (setName pn) runtime.logger

    let sendLog (logClient:Aliyun.Api.LOG.LogClient) reqs extra =
      let (host, serviceName, project, logstore) = extra
      let putLogRequest = PutLogsRequest(project,logstore)
      
      let logSendTime = uint32 (SystemClock.Instance.GetCurrentInstant().ToUnixTimeSeconds())
      putLogRequest.Topic <- serviceName // use serviceName as log topic for quick(index default) group/search
      putLogRequest.LogItems <- ResizeArray<_> (Array.map (transToAliLogItem host logSendTime) reqs)

      let res = logClient.PutLogs putLogRequest
      let resHeader = res.GetAllHeaders() |> Seq.map (|KeyValue|) |> Map.ofSeq
      logger.verboseWithBP (eventX "res {header}" >> Message.setField "header" resHeader) 
      >>=. Seq.iterJobIgnore reqestAckJobCreator reqs
    
    let extraInfo = (runtime.host, runtime.service, conf.Project, conf.Logstore)

    let rec running state =
      Alt.choose [
        // maybe because 200ms timeout when shutdown/flush target, so we should not send log to client, just dispose resource and ack ?
        api.shutdownCh ^=> fun ack -> 
          let msg = Message.eventInfo "application shut down logging"
          sendLog state.logClient [|Log(msg , ack)|] extraInfo
        
        RingBuffer.takeAll api.requests ^=> fun logReqMsg ->
          sendLog state.logClient logReqMsg extraInfo
          >>=. running state
      ] :> Job<_>

    running { logClient = new Aliyun.Api.LOG.LogClient(conf.Endpoint,conf.AccessKeyId,conf.AccessKey)}



/// Create a new YOUR TARGET NAME HERE target
[<CompiledName "Create">]
let create conf name = 
  TargetConf.createSimple (Impl.loop conf) name


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
          

