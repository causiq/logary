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
let ex = try let a = Message.templateEvent<int> (Info,"abc") in a 1; exn "" with | e -> e
let msg =
  Message.event Error "here is some exception: {@msg}"
  |> Message.addGauge "Core 1" (Gauge (0.001, Percent))
  |> Message.addGauge "Core 2" (Gauge (0.99, Percent))
  |> Message.addGauge "Core 3" (Gauge (0.473223755, Percent))
  |> Message.setContext "host" "db-001"
  |> Message.setContext "service" "api-web"
  |> Message.setName (PointName.ofList ["a"; "b"; "c"; "d"])
  |> Message.setField "ex" (ex)
  |> Message.addExn (ex)

let msg1 = msg |> Message.setField "msg" msg |> Message.setContext "msg-ctx" msg

let str = MessageWriter.levelDatetimeMessagePathNewLine.format msg1
str


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
  Message.eventInfo ("Hello World!  {user} done!")
  |> Message.setName (PointName.parse "logger.name.test")
  |> Message.setField "user" tp
  |> Message.setField "tpList" [tp;tp;]
  |> Message.setContext "tpList " [tp;tp;]
  |> Message.setContext "user" foo



Logger.logSimple ilg msg
ilg.info (eventX "...")





// Registry.shutdown registry |> run