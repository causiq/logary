#I "bin/Debug"
#r "Logary.dll"
#r "FsCheck.dll"
#r "Hopac.dll"
#r "NodaTime.dll"

open Logary
open Logary.Configuration
open FsCheck
open System
open System.Collections
open System.Collections.Generic
open Hopac
open Hopac.Infixes
open NodaTime
open Logary.Message
open System.Diagnostics
open Logary.MessageEx
open Logary.MessageTemplates
open Logary.MessageTemplates.Destructure


printfn "before"

Formatting.Format("[{level}] {timestampUtc:u} {message} [{source}]{exceptions}",)



let registry = 
  Config.create "svc" "localhost" 
  |> Config.ilogger (ILogger.LiterateConsole Verbose) 
  |> Config.build
  |> run

let logm = Registry.toLogManager registry

let ilg = logm.runtimeInfo.logger

Logger.logSimple ilg (event Info "hi")
Logger.logSimple ilg (event Info "hi just some test")

