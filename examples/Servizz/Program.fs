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

  // for the statics:
  LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logary

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName [| "Libryy" |])
  let res = Libryy.Core.work (LoggerAdapter.createGeneric logger)

  mre.Wait()
  0
