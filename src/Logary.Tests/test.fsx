#I "bin/Release/net461"
#r "Logary"
#r "FsCheck"
#r "Hopac"
#r "Hopac.Core"
#r "NodaTime"

open FsCheck
open Hopac
open Hopac.Infixes
open Logary
open Logary.Configuration
open Logary.Message
open Logary.MessageTemplates
open Logary.MessageTemplates.Destructure
open Logary.Targets
open NodaTime
open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics

printfn "before"

let pipe () =
  Events.events
//  |> Pipe.chain (fun cont ->
//       let mutable firstOccur = true
//
//       fun prev ->
//         if firstOccur && prev.level >= LogLevel.Error then firstOccur <- false
//         cont prev)
  |> Events.sink [ "bad boy" ]

let logm =
  Config.create "svc" "localhost"
  |> Config.target (Targets.LiterateConsole.create Targets.LiterateConsole.empty "nice console")
  |> Config.target (Targets.BadBoy.create Targets.BadBoy.empty "bad boy")
  |> Config.loggerMinLevel "a.b.*" LogLevel.Fatal
  |> Config.processing (pipe ())
  |> Config.build
  |> run
//run (logm.shutdown())

let ab = logm.getLogger (PointName.parse "a.bxxxx")

ab.fatal (Message.eventX "ab.info" >> (fun msg -> printfn "invoke %s" msg.value; msg)) // no invoke

logm.switchLoggerLevel ("a.b.*", LogLevel.Info)
ab.info (Message.eventX "ab.info" >> (fun msg -> printfn "invoke %s" msg.value; msg)) // hurry


logm.flushPending() |> run