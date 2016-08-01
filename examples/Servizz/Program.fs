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
      withTargets [ Console.create Console.empty (PointName.ofSingle "console") ]
      >> withRules [ Rule.createForTarget (PointName.ofSingle "console") ])
    |> run

  let libLogger = LogaryFacadeAdapter.createGeneric logger
  let res = Libryy.Core.work libLogger

  mre.Wait()
  0