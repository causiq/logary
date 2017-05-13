module Logary.Targets.AliYun

open System
open Logary
open Logary.Target
open Logary.Internals
open Logary.Message

type AliYunConf = 
  {
    AccessKeyId             : string
    AccessKey               : string
    Endpoint                : string   
    ClientConnectTimeout    : int  
    ClientReadWriteTimeout  : int   
    Project                 : string
    Logstore                : string
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


type State = { logClient : Aliyun.Api.LOG.LogClient }

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
      Ch.give ackCh () <|> nack :> Job<_>

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

        match msg.value with
        | Event t ->
          let subject = Formatting.MessageParts.formatTemplate t msg.fields
          log.PushBack("Msg",subject)
          // add origin template for identify in source code if needed
          log.PushBack("Template",t)
        | Gauge (v, u)
        | Derived (v, u) ->
          let gaugeNumber = Value.toDouble v
          let subject = Units.formatWithUnit Units.UnitOrientation.Suffix u v
          log.PushBack("Msg",subject)
          // for making number compare search
          log.PushBack("GaugeNumber",string gaugeNumber)

        msg.context |> Map.iter (fun k v ->
          let k = "_ctx-" + k
          let str = Formatting.MessageParts.formatValue Environment.NewLine 0 v
          log.PushBack(k, str)
        )

        msg.fields |> Map.iter (fun k (Field (v, units)) ->
          let k = "_field-" + PointName.format k
          let str = Formatting.MessageParts.formatValue Environment.NewLine 0 v
          match units with
          | Some units ->
            log.PushBack(k, str + Units.symbol units)
          | _ -> 
            log.PushBack(k, str)
        )
        

    | Flush (ack,nack) ->
        log.PushBack("TimeOffSet",string DateTimeOffset.Now)
        log.PushBack("Level",string LogLevel.Info)
        log.PushBack("LoggerName","Logary.AliYun.FlushLog")
        log.PushBack("Msg","application flush log")

    log

  let loop (conf : AliYunConf)
           (runtime : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) = 
    
    let logger = 
      let pn = PointName [| "Logary"; "AliYun"; "loop" |]
      Logger.apply (setName pn) runtime.logger

    let sendLog (logClient:Aliyun.Api.LOG.LogClient) reqs extra =
      let (host, serviceName, project, logstore) = extra
      let putLogRequest = PutLogsRequest(project,logstore)
      
      let logSendTime = uint32 (SystemClock.Instance.Now.Ticks / NodaConstants.TicksPerSecond)
      putLogRequest.Topic <- serviceName // use serviceName as log topic for quick(index default) group/search
      putLogRequest.LogItems <- ResizeArray<_> (Array.map (transToAliLogItem host logSendTime) reqs)

      let res = logClient.PutLogs putLogRequest
      let resHeader = res.GetAllHeaders() |> Seq.map (|KeyValue|) |> Map.ofSeq
      logger.verboseWithBP (eventX "res {header}" >> Message.setField "header" resHeader) 
      >>=. Seq.iterJobIgnore reqestAckJobCreator reqs
    
    let extraInfo = (runtime.host, runtime.serviceName, conf.Project, conf.Logstore)

    let rec running state =
      Alt.choose [
        // maybe because 200ms timeout when shutdown/flush target, so we should not send log to client, just dispose resource and ack ?
        shutdown ^=> fun ack -> 
          let msg = Message.eventInfo "application shut down logging"
          sendLog state.logClient [|Log(msg , ack)|] extraInfo
        
        RingBuffer.takeAll requests ^=> fun logReqMsg ->
          sendLog state.logClient logReqMsg extraInfo
          >>=. running state
      ] :> Job<_>

    running { logClient = new Aliyun.Api.LOG.LogClient(conf.Endpoint,conf.AccessKeyId,conf.AccessKey)}



/// Create a new YOUR TARGET NAME HERE target
[<CompiledName "Create">]
let create conf name = 
  TargetUtils.stdNamedTarget (Impl.loop conf) name


type IThirdStep = 
  abstract SetConnectTimeOut : int -> IThirdStep
  abstract SetReadWriteTimeOut : int -> IThirdStep
  abstract Done : unit -> FactoryApi.TargetConfBuild<Builder>
and ISecondStep =
  abstract ConfLogLocation : string * string -> IThirdStep
and Builder(conf : AliYunConf, callParent : FactoryApi.ParentCallback<_>) = 

  new(callParent : FactoryApi.ParentCallback<_>) =
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

  interface FactoryApi.SpecificTargetConf with
      member this.Build(name: string): TargetConf = 
        if String.IsNullOrEmpty conf.AccessKey || String.IsNullOrEmpty conf.AccessKeyId || String.IsNullOrEmpty conf.Endpoint then
          failwith "accesskey keyId endpoint is needed"
        else
          create conf name
          

