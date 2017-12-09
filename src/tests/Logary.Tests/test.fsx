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

type X() =
    member this.F([<ParamArray>] args: Object[]) =
        for arg in args do
            printfn "%A" arg


let f ([<ParamArray>] args: Object[]) = ()



open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

module A =
  let only<'t>(proj: 't -> obj[]) = ()
  let except<'t>(proj: 't -> obj[]) = ()

type Projection =
| Projection of string * How
| NotSupport
and How =
| Only of string list list
| Except of string list list

let rec getNames expr =
  match expr with
  | Call (_, method, [Lambda( d, NewArray(_, props))]) -> Projection (d.Type.Name, generateNames method.Name props)
  | _ -> NotSupport
and generateNames methodName exprs =
  exprs 
  |> List.map (fun expr ->
     match expr with
     | Coerce (PropertyGet(Some(outterExpr), prop,_), _) -> generateOutter outterExpr [prop.Name]
     | _ -> [])
  |> fun projs ->
     match methodName with
     | "only" -> Only projs
     | "except" -> Except projs
     | _ -> Only []
and generateOutter outterExpr innerProjs =
  match outterExpr with
  | PropertyGet (Some (expr),prop,_) ->
    generateOutter expr (prop.Name :: innerProjs)
  | _ -> innerProjs

let exnProjection = 
  <@@ A.only<Exception>(fun e -> 
    [|
     e.Message;
     e.StackTrace;
     e.Data;
     e.InnerException.Message;
     e.InnerException.StackTrace
    |]) @@>

let dateExceptCycleReference = <@@ A.except<DateTime>(fun date -> [| date.Date; |]) @@>

getNames dateExceptCycleReference
getNames exnProjection

(*

// output

> getNames dateExceptCycleReference
- ;;
val it : Projection = Projection ("DateTime",Except [["Date"]])

> getNames exnProjection
- ;;
val it : Projection =
  Projection
    ("Exception",
     Only
       [["Message"]; ["StackTrace"]; ["Data"]; ["InnerException"; "Message"];
        ["InnerException"; "StackTrace"]])

*)

type A = A
with 
  member x.only<'t>(proj: 't -> obj[]) = x
  member x.except<'t>(proj: 't -> obj[]) = x

let a = A
let c = <@@ 
  a.except<DateTime>(fun date -> [| date.Date; |])
   .except<Exception>(fun ex -> [|ex.Message; |])
  @@>


type A () =
  member val Id = 1 with get
  member val Age = 1 with get,set
  member x.Name  with set s = ()
  member x.Info  = 1
  static member StaticInfo  = 1

let t = typeof<A>
open System.Reflection
let b = BindingFlags.Public ||| BindingFlags.Instance
t.GetProperties(b) |> Seq.map (fun p -> p.Name)

let f num =
  let a =
    try
      1/num
    with
    | e -> 
      1
  a

