module ZipkinExample

open Logary
open Logary.Target
open Logary.Configuration.Config

[<EntryPoint>]
let main argv =
  let logary =
    confLogary "Intelliplan Logary Example"
    |> withRules
      [ Rules.forAny "console"
      ; Rules.forAny "restkin"
      ; Rules.forAny "debugger" ]
    |> withTargets
      [ Console.create Console.ConsoleConf.Default "console"
      ; RestKin.create RestKin.RestKinConf.Default "restkin"
      ; Debugger.create (Debugger.DebuggerConf.Default) "debugger" ]
    |> validateLogary
    |> runLogary

  let logger = "logary.restkin.example" |> Registry.getLogger logary.registry |> Async.RunSynchronously

  let res = Log.time logger "math.calculation" <| fun _ -> 1 + 1
  "this was cool" |> Log.info logger
  "http.outbound_requests" |> Log.incr logger
  "http.queue_length" |> Log.decr logger

  System.Console.ReadKey true |> ignore

  logary |> shutdownLogary |> Async.RunSynchronously |> ignore

  0 // return an integer exit code
