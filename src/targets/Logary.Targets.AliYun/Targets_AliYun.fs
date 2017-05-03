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


  let timeOffSet timestampTicks =
    DateTimeOffset(DateTime(1970,01,01).AddTicks(timestampTicks))

  let transToAliLogItem hostName request =
    let log = LogItem()
    // time must set , can be anytime
    log.Time <- uint32 (SystemClock.Instance.Now.Ticks / NodaConstants.TicksPerSecond)
    log.PushBack("Host",hostName)
    match request with
    | Log(msg,ack) ->
        log.PushBack("TimeOffSet",(timeOffSet msg.timestampTicks).ToString())
        log.PushBack("Level",msg.level.ToString())
        log.PushBack("LoggerName",msg.name.ToString())
        // log.PushBack("LoggerName",msg.value)
        // log.PushBack("LoggerName",msg.fields)
        // log.PushBack("LoggerName",msg.context)

    | Flush (ack,nack) ->
        log.PushBack("TimeOffSet",DateTimeOffset.Now.ToString())
        log.PushBack("Level",LogLevel.Info.ToString())
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

      // use serviceName as log topic for quick group/search
      putLogRequest.Topic <- serviceName
      putLogRequest.LogItems <- ResizeArray<_> (Array.map (transToAliLogItem host) reqs)
      let res = logClient.PutLogs putLogRequest
      let resHeader = res.GetAllHeaders() |> Seq.map (|KeyValue|) |> Map.ofSeq
      logger.verboseWithBP (eventX "res {header}" >> Message.setField "header" resHeader) 
      >>=. Seq.iterJobIgnore reqestAckJobCreator reqs
    
    let extraInfo = (runtime.host, runtime.serviceName, conf.Project, conf.Logstore)

    let rec running state =
      Alt.choose [
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
  abstract SetConnectTimeOut : int -> FactoryApi.TargetConfBuild<Builder>
  abstract SetReadWriteTimeOut : int -> FactoryApi.TargetConfBuild<Builder>
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
      ! (callParent <| Builder({conf with ClientConnectTimeout = timoutMS}, callParent))

    member x.SetReadWriteTimeOut(timoutMS) =
      ! (callParent <| Builder({conf with ClientReadWriteTimeout = timoutMS}, callParent))
    
    member x.Done () =
      ! (callParent x)

  interface FactoryApi.SpecificTargetConf with
      member this.Build(name: string): TargetConf = 
        if String.IsNullOrEmpty conf.AccessKey || String.IsNullOrEmpty conf.AccessKeyId || String.IsNullOrEmpty conf.Endpoint then
          failwith "accesskey keyId endpoint is needed"
        else
          create conf name
          

