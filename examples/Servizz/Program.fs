module Servizz.Program

open Hopac
open Logary
open Logary.Targets
open Logary.Configuration
open Logary.Adapters.Facade
open System
open System.Threading

let logger = Logging.getLoggerByName "Servizz.Program"

type MyDu = UserLoggedIn of name:string | UserLoggedOut of name:string

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
  
  let gotLibryyResultEvent = Message.templateEvent<int, MyDu> (Debug, "Got {LibryyResult} from Libryy, {@Event}")
  let logLibryyResult result event = gotLibryyResultEvent result event |> logger.logSimple

  let workResult = Libryy.Core.work librryLogger
  logLibryyResult workResult (UserLoggedIn "adam")

  let simpleWorkExnResult = Libryy.Core.generateAndLogExn librryLogger
  logLibryyResult simpleWorkExnResult (UserLoggedIn "haf")

  let staticWorkResult = Libryy.Core.staticWork()
  logLibryyResult staticWorkResult (UserLoggedIn "mavnn")

  mre.Wait()
  0
