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


let aliyunConf = { AccessKeyId = "LTAIRqnMACOUV9sY";
                  AccessKey = "XCMaL1KCMLqmHUW7KqdsEa0eGsACn5"; 
                  Endpoint = "http://cn-hangzhou.log.aliyuncs.com";
                  ClientConnectTimeout = 2000;
                  ClientReadWriteTimeout = 2000;
                  Project = "star";
                  Logstore = "star";}

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

let list = [|1;2;3;4;5|]
let tuple = ("first",2,"Third")
let msg = 
  Message.eventInfo ("Hello World!")
  |> Message.setField "testFrom" "logary"
  |> Message.setField "target" "aliyun-target"
  |> Message.setField "use-ali-sdk" "true"
  |> Message.setContext "list string " ["what";"fuck"]
  |> Message.setContext "int array" list
  |> Message.setContext "tuple" tuple

let curLogger = Logary.Logging.getCurrentLogger()
let jiajunLogger = logger.getLogger (PointName [| "Logary"; "Aliyun"; "JiaJun"; "TestInScript" |])

msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> jiajunLogger.logSimple

let timeWaiting = Duration.FromSeconds 10L
logger.shutdown timeWaiting timeWaiting >>- fun state ->
    printfn "success %A" state.successful
|> run

// Login your AliYun sls console (https://sls.console.aliyun.com/). 
// Go to your logstore
// Your events should be visible under logstore search

printfn "done"
