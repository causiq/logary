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
open Logary
open Logary.Targets
open Logary.Targets.AliYun
open Logary.Configuration
open Logary.Logger

printfn "begin"

let logger = 
    withLogaryManager "testAliYun" (
        withTargets [
            Logary.Targets.AliYun.create(
                { AccessKeyId = "LTAI7sFLFks3bw1N";
                  AccessKey = "p0AttceU3PStM8ZUza5NBLRiusDCVC"; 
                  Endpoint = "http://cn-hangzhou.log.aliyuncs.com";
                  ClientConnectTimeout = 2000;
                  ClientReadWriteTimeout = 2000;
                  Project = "star";
                  Logstore = "star";
                }
            ) "AliYunLog"
        ] 
        >> withRules [
            Rule.createForTarget "AliYunLog" |> Rule.setLevel LogLevel.Verbose
        ]
        >> withInternalTargets Verbose [
            Console.create Console.empty "console"
        ]
    ) |> run

let msg = 
  Message.eventInfo ("Hello World!")
  |> Message.setField "testFrom" "logary"
  |> Message.setField "target" "aliyun-target"
  |> Message.setField "use-ali-sdk" "true"
  |> Message.setContext "ctx array" [|"what";"fuck"|]

let curLogger = Logary.Logging.getCurrentLogger()
let jiajunLogger = logger.getLogger (PointName [| "Logary"; "Aliyun"; "JiaJun"; "TestInScript" |])

msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> logSimple curLogger
msg |> jiajunLogger.logSimple



// Login your AliYun sls console (https://sls.console.aliyun.com/). 
// Go to your logstore
// Your events should be visible under logstore search

printfn "done"