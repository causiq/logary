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

  for i = 0 to 100 do
    Libryy.Core.work (LoggerAdapter.createGeneric logger) |> ignore

  mre.Wait()
  0
