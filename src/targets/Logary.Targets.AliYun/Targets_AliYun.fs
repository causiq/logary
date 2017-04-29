module Logary.Targets.AliYun

open System
open Logary
open Logary.Target
open Logary.Internals


type AliYunConf = 
  {
    AccessKeyId             : string
    AccessKey               : string
    Endpoint                : string   
    //ClientConnectTimeout    : int   
    //ClientReadWriteTimeout  : int   
  }

type State = { logClient : Aliyun.Api.LOG.LogClient }


module internal Impl =
  open Hopac
  open Hopac.Infixes
  open Aliyun.Api.LOG.Request;
  open Aliyun.Api.LOG.Data;
  open NodaTime

  let transLogMsgToAliLogItem (runtime:RuntimeInfo) tmsg : LogItem =
    let log = new LogItem()
    log.Time <- uint32 (SystemClock.Instance.Now.Ticks / NodaConstants.TicksPerSecond)
    log.PushBack("Host",runtime.host)
    log.PushBack("Application",runtime.serviceName)

    match tmsg with
    | Log(msg,ack) ->
        log.PushBack("TimeOffSet",DateTimeOffset(DateTime(1970,01,01).AddTicks(msg.timestampTicks)).ToString())
        log.PushBack("Level",msg.level.ToString())
        log.PushBack("LoggerName",msg.name.ToString())

    | Flush (ack,nack) ->
        log.PushBack("TimeOffSet",DateTimeOffset.Now.ToString())
        log.PushBack("Msg","application flush log")
        log.PushBack("Level",LogLevel.Info.ToString())

    log

  let sendLog (logClient:Aliyun.Api.LOG.LogClient) runtime (logReqMsgs : TargetMessage array) =
    let putLogRequest = new PutLogsRequest("star","star")
    putLogRequest.Topic <- "LogaryJiajunTest"
    //putLogRequest.Source  // 对于 阿里云 时间/ip 一栏中的 IP
    putLogRequest.LogItems <-  ResizeArray<LogItem> (Array.map (transLogMsgToAliLogItem runtime) logReqMsgs)
    printfn "begin logClient.PutLogs(putLogRequest)"
    let r = logClient.PutLogs(putLogRequest)
    printfn "end logClient.PutLogs(putLogRequest)"
    printfn "headers %A" (r.GetAllHeaders())
    Job.unit ()

  let loop (conf : AliYunConf)
           (runtime : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) = 
    
    let rec loop( state : State) =
      printfn "------------------------------------------loop"
      Alt.choose [

        shutdown ^=> fun ack -> 
          printfn "------------------------------------------shutdown"

          let msg = Message.eventInfo "application shut down logging"
          [|Log(msg , ack)|]
          |> sendLog state.logClient runtime
        
        RingBuffer.takeAll requests ^=> fun logReqMsg ->
          //printfn "------------------------------------------take one"
          printfn "------------------------------------------take all %A" logReqMsg.Length
          
          //[| logReqMsg |] 
          logReqMsg
          |> sendLog state.logClient runtime
          >>=. loop(state)
      ] :> Job<_>

    loop { logClient = new Aliyun.Api.LOG.LogClient(conf.Endpoint,conf.AccessKeyId,conf.AccessKey)}



/// Create a new YOUR TARGET NAME HERE target
[<CompiledName "Create">]
let create conf name = 
  printfn "testestest"
  TargetUtils.stdNamedTarget (Impl.loop conf) name




type Builder(conf : AliYunConf option, callParent : FactoryApi.ParentCallback<_>) = 

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(None, callParent)

  member x.SetConf(conf) =
    ! (callParent <| Builder(Some conf, callParent))

  interface FactoryApi.SpecificTargetConf with
      member this.Build(name: string): TargetConf = 
        match conf with
        | None -> failwith "need set aliyun sepcific target conf"
        | _ -> raise (new NotImplementedException())

