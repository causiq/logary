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

printfn "before"

let multiGaugeMessage =
  Message.event Info "Processor.% Idle"
  |> Message.addGauge "Core 1" (Gauge (0.001, Percent))
  |> Message.addGauge "Core 2" (Gauge (0.99, Percent))
  |> Message.addGauge "Core 3" (Gauge (0.473223755, Percent))
  |> Message.setContext "host" "db-001"
  |> Message.setContext "service" "api-web"


let timeMessage (nanos : int64) level =
  let value, units = float nanos, Scaled (Seconds, float Constants.NanosPerSecond)
  Message.gaugeMessage "A.B.C.Check" (Gauge(value, units))
  |> Message.setLevel level

let (scaledValue, unitsFormat) = Units.scale  (Scaled (Seconds, float Constants.NanosPerSecond)) 60029379.
scaledValue.ToString("N2")
let msg = timeMessage 133379L Info

let msg = 
  Message.gaugeWithUnit "Revolver" 1.4562 (Div (Seconds, Units.Other "revolution"))
  |> Message.setLevel Info


let (scaledValue, unitsFormat) = Units.scale  (Div (Seconds, Units.Other "revolution")) 1.4562

let str = MessageWriter.levelDatetimeMessagePathNewLine.format msg
str
scaledValue.ToString("N2")

// let ex = exn "something wrong"
// let msg1 =
//   Message.event Error "here is some exception: {@msg}"
//   |> Message.addGauge "Core 1" (Gauge (0.001, Percent))
//   |> Message.addGauge "Core 2" (Gauge (0.99, Percent))
//   |> Message.addGauge "Core 3" (Gauge (0.473223755, Percent))
//   |> Message.setContext "host" "db-001"
//   |> Message.setContext "service" "api-web"
//   |> Message.setName (PointName.ofList ["a"; "b"; "c"; "d"])
//   |> Message.setField "ex" (ex)
//   |> Message.addExn (ex)

// let msg = msg1 |> Message.setField "msg" msg1 

// let str = MessageWriter.levelDatetimeMessagePathNewLine.format msg


let registry = 
  Config.create "svc" "localhost" 
  |> Config.ilogger (ILogger.LiterateConsole Verbose) 
  |> Config.build
  |> run

let logm = Registry.toLogManager registry

let ilg = logm.runtimeInfo.logger

Logger.logSimple ilg (event Info "hi")
Logger.logSimple ilg (event Info "hi just some test")
Logger.logSimple ilg msg


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

let msg2 = 
  Message.eventInfo ("Hello World!  {user} done!")
  |> Message.setName (PointName.parse "logger.name.test")
  |> Message.setField "user" tp
  |> Message.setField "tpList" [tp;tp;]
  |> Message.setContext "tpList " [tp;tp;]
  |> Message.setContext "user" foo

Logger.logWithAck ilg Info (fun _ -> msg) |> run |> run





// Registry.shutdown registry |> run