module Logary.ConsoleApp.Program

open System
open Hopac
open Logary
open Logary.Message
open Logary.Configuration
open Logary.Targets

[<EntryPoint>]
let main argv =
  use mre = new System.Threading.ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  let logary =
    Config.create "Logary.ConsoleApp" "laptop"
    |> Config.target (LiterateConsole.create LiterateConsole.empty "console")
    |> Config.ilogger (ILogger.Console Debug)
    |> Config.build
    |> run

  let logger = logary.getLogger "Logary.HelloWorld"

  logger.info (eventX "Hello world")

  mre.Wait()
  0