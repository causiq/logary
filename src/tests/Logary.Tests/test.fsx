#I "bin/Debug"
#r "Logary.dll"
#r "FsCheck.dll"
#r "Hopac.dll"
#r "NodaTime.dll"

open Logary
open Logary.Configuration
open FsCheck
open System
open Hopac
open Hopac.Infixes
open NodaTime
open Logary.Message

printfn "before"
let registry = 
  Config.create "svc" "localhost" 
  |> Config.ilogger (ILogger.Console Verbose) 
  |> Config.build
  |> run

let logm = Registry.toLogManager registry

let ilog = logm.runtimeInfo.logger
ilog.info (eventX "...")
