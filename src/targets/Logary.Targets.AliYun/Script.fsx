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
#r "Logary.Targets.AliYun.dll"


open System
open Hopac
open Logary
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
                }
            ) "AliYunLog"
        ] 
        >> withRules [
            Rule.createForTarget "AliYunLog" |> Rule.setLevel LogLevel.Verbose
        ]
    ) |> run

let msg = 
  Message.eventInfo ("Hello World!")
  |> Message.setField "testFrom" "logary"
  |> Message.setField "target" "aliyun-target"
  |> Message.setField "use-ali-sdk" "true"

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