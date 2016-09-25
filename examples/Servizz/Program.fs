module Servizz.Program

open Hopac
open Logary
open Logary.Targets
open Logary.Configuration
open Logary.Adapters.Facade
open System
open System.Threading

let logger = Logging.getLoggerByName "Servizz.Program"

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  use logary =
    withLogaryManager "Servizz.Program" (
      withTargets [ Console.create Console.empty "console" ]
      >> withRules [ Rule.createForTarget "console" ])
    |> run

  // Initialise Libryy so it logs to Logary (proper)
  LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logary

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName [| "Libryy" |])
  let librryLogger = LoggerAdapter.createGeneric logger

  let workResult = Libryy.Core.work librryLogger
  Message.eventDebug "Got {workResult} from Libryy" |> Message.setField "workResult" workResult |> logger.logSimple

  let simpleWorkExnResult = Libryy.Core.simpleWorkThatCatchesAndLogsAnErrorAndException librryLogger
  Message.eventDebug "Got {simpleWorkExnResult} from Libryy" |> Message.setField "simpleWorkExnResult" simpleWorkExnResult |> logger.logSimple

  let staticWorkResult = Libryy.Core.staticWork()
  Message.eventDebug "Got {staticWorkResult} from Libryy" |> Message.setField "staticWorkResult" staticWorkResult |> logger.logSimple

  mre.Wait()
  0
