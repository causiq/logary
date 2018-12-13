#I "bin/Release/net461"
#I "../../packages/Hopac/lib/netstandard2.0"
#r "Hopac"
#r "Hopac.Core"
open Hopac

open Hopac.Infixes

let a:Alt<unit> = Alt.prepareJob <| fun () ->
  timeOutMillis 5000 
  |> Job.map (fun _ -> Alt.never ())

let res = 
  a ^-> fun _ -> printf "hahah" 
  <|> 
  timeOutMillis 1000 ^-> fun _ -> printf "timeout"

run res

open Logary
open Logary.Internals
open Logary.Configuration

let logm =
  Config.create "svc" "localhost"
  |> Config.target (Targets.LiterateConsole.create Targets.LiterateConsole.empty "nice console")
  |> Config.target (Targets.BadBoy.create Targets.BadBoy.empty "bad boy")
  |> Config.ilogger (ILogger.LiterateConsole LogLevel.Debug)
  |> Config.loggerMinLevel "a.b.*" Info
  //|> Config.processing (pipe ())
  |> Config.build
  |> run

let ab = logm.getLogger (PointName.parse "a.b")

let xJ: Job<int> =
  job {
    printfn "Ran"
    return raise (exn "Nooo")
  }
let xSJ = Job.supervise ab Policy.exponentialBackoffForever xJ
run xSJ

let r, initD, mult, maxD, maxRetries =
  30u, 2u,    2u,   20u,  4294967295u
min maxD (initD * pown mult (int r))

logm.flushPending() |> run
run (logm.shutdown())

ab.fatal (Message.eventX "ab.info" >> (fun msg -> printfn "invoke %s" msg.value; msg)) // no invoke
logm.switchLoggerLevel ("a.b.*", LogLevel.Info)
ab.info (Message.eventX "ab.info" >> (fun msg -> printfn "invoke %s" msg.value; msg)) // hurry