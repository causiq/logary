#I "./../../../packages"
#I "bin/Debug"

#r "NodaTime.dll"
#r "Hopac.dll"
#r "Hopac.Core.dll"
#r "Logary.dll"
#r "AliyunLogSDK.dll"
#r "LZ4Sharp.dll"
#r "Google.ProtocolBuffers.dll"
#r "Google.ProtocolBuffers.Serialization.dll"

#load "Targets_AliYun.fs"

open System
open Hopac
open Hopac.Infixes
open Logary
open Logary.Targets
open Logary.Targets.AliYun
open Logary.Configuration
open Logary.Logger
open NodaTime

printfn "begin"


let aliyunConf = { AccessKeyId = "";
                  AccessKey = ""; 
                  Endpoint = "";
                  ClientConnectTimeout = 2000;
                  ClientReadWriteTimeout = 2000;
                  Project = "";
                  Logstore = "";}

let logger = 
    withLogaryManager "testAliYun" (
        withTargets [
            Logary.Targets.AliYun.create(aliyunConf) "AliYunLog"
        ] 
        >> withRules [
            Rule.createForTarget "AliYunLog" |> Rule.setLevel LogLevel.Verbose
        ]
        >> withInternalTargets Verbose [
            Console.create Console.empty "console"
        ]
    ) |> run

let tuple = ("first",2,"Third")
let msg = 
  Message.eventInfo ("Hello World!")
  |> Message.setField "testFrom" "logary"
  |> Message.setField "target" "aliyun-target"
  |> Message.setField "use-ali-sdk" "true"
  |> Message.setContext "list string " ["some";"info";"from";"log"]

let curLogger = Logary.Logging.getCurrentLogger()
let jiajunLogger = logger.getLogger (PointName [| "Logary"; "Aliyun"; "JiaJun"; "TestInScript" |])

msg |> logSimple curLogger
msg |> Message.setLevel Error |> jiajunLogger.logSimple

let timeWaiting = Duration.FromSeconds 5L
Hopac.timeOutMillis 3000 
^=> fun _ ->
    logger.flushPending timeWaiting
^=> fun _ ->
    Hopac.timeOutMillis 5000 
^=> fun _ ->
    logger.shutdown timeWaiting timeWaiting
|> run
|> fun state ->
   printfn "------------------after shutdow => %s %A" Environment.NewLine state

// Login your AliYun sls console (https://sls.console.aliyun.com/). 
// Go to your logstore
// Your events should be visible under logstore search
