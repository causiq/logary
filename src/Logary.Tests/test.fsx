#I "bin/Release/net461"
#r "Logary"
#r "FsCheck"
#r "Hopac"
#r "Hopac.Core"
#r "NodaTime"

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
open Logary.EventProcessing
open Logary.Targets

printfn "before"

let pipe () =
  Events.events
  |> Pipe.chain (fun cont ->
       let mutable firstOccur = true

       fun prev ->
         if firstOccur && prev.level >= LogLevel.Error then firstOccur <- false
         cont prev)

let logm =
  Config.create "svc" "localhost"
  |> Config.target (Targets.LiterateConsole.create Targets.LiterateConsole.empty "nice console")
  |> Config.loggerMinLevel "a.b.*" LogLevel.Fatal
  |> Config.processing (pipe ())
  |> Config.build
  |> run

let ab = logm.getLogger (PointName.parse "a.bxxxx")

ab.info (Message.eventX "ab.info" >> (fun msg -> printfn "invoke %s" msg.value; msg)) // no invoke
logm.switchLoggerLevel ("a.b.*", LogLevel.Info)
ab.info (Message.eventX "ab.info" >> (fun msg -> printfn "invoke %s" msg.value; msg)) // hurry


logm.flushPending() |> run