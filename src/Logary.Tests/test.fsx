#I "bin/Debug/net461"
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
open Logary.EventsProcessing
open Logary.Targets


printfn "before"


let logm = 
  Config.create "svc" "localhost" 
  |> Config.target (Targets.LiterateConsole.create Targets.LiterateConsole.empty "nice console")
  |> Config.loggerMinLevel "a.b.*" LogLevel.Fatal
  |> Config.build
  |> run

let ab = logm.getLogger (PointName.parse "a.bxxxx")

type BinaryDU = Binary of data:byte [] * contentType:ContentType
// or
type Binary = { data: byte[]; contentType: ContentType }

let b1 = Binary ([|1uy|], "image/jpeg")
let b2 = {data= [||]; contentType= "image/jpeg"}

ab.fatal (Message.eventX "Got {@pic} DU, {pic}" >> Message.setField "pic" b1) 
ab.fatal (Message.eventX "Got {@pic} Record {pic}" >> Message.setField "pic" b2) 


logm.flushPending() |> run