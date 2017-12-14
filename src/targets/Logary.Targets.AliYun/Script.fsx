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

let logm = 
    Config.create "testAliYun" "localhost"
    |> Config.target (Logary.Targets.AliYun.create aliyunConf "AliYunLog")
    |> Config.ilogger (ILogger.Console Verbose) 
    |> Config.build
    |> run
let tuple = ("first",2,"Third")
let msg = 
  Message.eventInfo ("Hello World! from {target}")
  |> Message.setField "target" "aliyun-target"
  |> Message.setContext "tuples" tuple
  |> fun msg -> Message.setContext "msg-itself" msg msg

let curLogger = Logary.Logging.getCurrentLogger()
let jiajunLogger = logm.getLogger (PointName [| "Logary"; "Aliyun"; "JiaJun"; "TestInScript" |])

msg |> logSimple curLogger
msg |> Message.setLevel Error |> jiajunLogger.logSimple

let timeWaiting = Duration.FromSeconds 5L

Hopac.timeOutMillis 3000 ^=> fun _ -> logm.shutdown timeWaiting timeWaiting
|> run
|> fun info ->
   printfn "------------------after shutdow => %s %A %A" Environment.NewLine info

// Login your AliYun sls console (https://sls.console.aliyun.com/). 
// Go to your logstore
// Your events should be visible under logstore search
