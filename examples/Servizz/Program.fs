module Servizz.Program

open Hopac
open Logary
open Logary.Targets
open Logary.Configuration
open Logary.Adapters.Facade
open System
open System.Threading

let logger = Logging.getLoggerByName "Servizz.Program"

type Speed = Fast | Slow | Other of speed:int
type Event =
| UserLoggedIn of name:string * roles:string list
| UserLoggedOut of name:string * speedAtTimeOfLeaving: Speed

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  use logary =
    withLogaryManager "Servizz.Program" (
      withTargets [ LiterateConsole.create LiterateConsole.empty "console" ]
      >> withRules [ Rule.createForTarget "console" ])
    |> run

  // Initialise Libryy so it logs to Logary (proper)
  LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logary

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName [| "Libryy" |])
  let librryLogger = LoggerAdapter.createGeneric logger
  
  let specialEvent = Message.templateEvent<Event>(Info, "Special event {@Event}")
  logger.logSimple (specialEvent (UserLoggedIn ("adam", [ "admin" ])))
  logger.logSimple (specialEvent (UserLoggedIn ("haf", [ "authenticatedUser"; "powerUser" ])))
  logger.logSimple (specialEvent (UserLoggedIn ("mavnn", [ "general" ])))

  logger.logSimple (specialEvent (UserLoggedOut ("adam", Fast)))
  logger.logSimple (specialEvent (UserLoggedOut ("haf", Slow)))
  logger.logSimple (specialEvent (UserLoggedOut ("mavnn", (Other 10))))

  let gotLibryyResultEvent = Message.templateEvent<int> (Debug, "Got {LibryyResult} from Libryy")
  let logLibryyResult result = gotLibryyResultEvent result |> logger.logSimple

  let workResult = Libryy.Core.work librryLogger
  logLibryyResult workResult

  let simpleWorkExnResult = Libryy.Core.generateAndLogExn librryLogger
  logLibryyResult simpleWorkExnResult

  let staticWorkResult = Libryy.Core.staticWork()
  logLibryyResult staticWorkResult

  mre.Wait()
  0
