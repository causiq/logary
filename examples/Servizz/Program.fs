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
      withTargets [ LiterateConsole.create LiterateConsole.empty "literate" ]
      >> withRules [ Rule.createForTarget "literate" ])
    |> run

  // for the statics:
  LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logary

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName [| "Libryy" |])
  let libryyLogger = LoggerAdapter.createGeneric logger

  // Do lots of libryy work
  for i = 0 to 10 do
    Libryy.Core.work libryyLogger |> ignore

  mre.Wait()
  0
