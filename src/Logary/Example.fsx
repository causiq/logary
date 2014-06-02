
#r "bin/Release/FSharp.Actor.dll"
#r "bin/Release/Intelliplan.Logary.dll"

open Logary
open Logary.Configuration.Config
open Logary.Rules
open Logary.Targets
open Logary.Target.Console

open System.Text.RegularExpressions

let logary =
  confLogary "tests"
  |> withRules [ Rule.Create(Regex(@"My\.App\..*"), "console", (fun line -> not <| line.path.Contains("TooVerbose")), LogLevel.Info) ]
  |> withTargets [ confTarget "console" (create ConsoleConf.Default) ]
  |> validateLogary
  |> runLogary

let logger = "a.b.c.d" |> Registry.getLogger logary.registry |> Async.RunSynchronously

"user sign-in"
|> Log.debugStrTag "authentication"
|> Log.setDatas [
  "user_id",     "34778"
  "token_thumb", "deaf1245"
]
|> Log.log logger

"my.app.web.authentication.signin" |> Log.incr logger
"my.app.web.authentication.session_timeout" |> Log.incr logger

"my.app.fulfillment.order_shipped" |> Log.incr logger
("my.infra.queues.length", 10.) ||> Log.decrBy logger

let calculate_meaning () =
  for i in 1..10 do
    System.Threading.Thread.Sleep(200)
  42

let the_answer = Log.time logger "earth.machine.meaning_of_everything" calculate_meaning

logary |> shutdownLogary |> Async.RunSynchronously
