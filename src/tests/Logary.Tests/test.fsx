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

let ilg = logm.runtimeInfo.logger


type User = 
  {
    id      : int 
    name    : string
    created : DateTime
  }
with
  interface IFormattable with
    member x.ToString (format, provider) =
      sprintf "id => %i, name => %s, created => %A" x.id x.name (x.created.ToShortDateString())

let foo = { id = 999; name = "foo"; created = DateTime.Now}

let tp = ("first",2,foo)

let msg = 
  Message.eventInfo ("Hello World!  {user1} done!")
  |> Message.setName (PointName.parse "logger.name.test")
  |> Message.setField "user" tp
  |> Message.setField "tpList" [tp;tp;]
  |> Message.setContext "tpList " [tp;tp;]
  |> Message.setContext "user" foo



Logger.logSimple ilg msg
ilg.info (eventX "...")





// Registry.shutdown registry |> run