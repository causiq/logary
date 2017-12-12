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


module A =
  let only<'t> (proj: 't -> obj[]) = ()
  let except<'t> (proj: 't -> obj[]) = ()
type A =
  static member Only ([<ParamArray>] props: obj[]) = props
  static member Except ([<ParamArray>] props: obj[]) = props


let a = <@@ A.only<Exception>(fun ex-> [|
  ex.Message;
  ex.StackTrace;
  A.Except(ex.InnerException.StackTrace, ex.Data.Count);
  A.Only(ex.InnerException.Data.Count);
  |])  @@>