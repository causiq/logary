#I "bin/Release/net461"
#r "NodaTime"
#r "Hopac"
#r "Hopac.Core"
#r "Logary"
#r "Aliyun.Api.Log"
#r "LZ4"
#r "Google.Protobuf"
#load "Targets_AliYun.fs"

open Hopac
open Logary
open Logary.Targets.AliYun
open Logary.Configuration

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
    |> Config.ilogger (ILogger.LiterateConsole Verbose)
    |> Config.build
    |> run
let msg =
  Message.eventFormat (Info, "Hello World! from {target}", "aliyun-target")
  |> Message.setContext "some tuple" ("first",2,"Third")
  |> fun msg -> Message.setContext "msg-itself" msg msg
  |> Message.setLevel Error

let curLogger = Log.create "test.log.fsx"

curLogger.log Error (fun _ -> msg) |> run
printfn "send log done!"

logm.shutdown () |> run

printfn "done!"

// Login your AliYun sls console (https://sls.console.aliyun.com/).
// Go to your logstore
// Your events should be visible under logstore search
