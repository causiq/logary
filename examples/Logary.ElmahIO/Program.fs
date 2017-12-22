#if INTERACTIVE
#I "bin/Release"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "NodaTime.dll"
#r "Logary.dll"
#r "Logary.Riemann.dll"
#endif

open System
open NodaTime
open Hopac
open Logary
open Logary.Configuration
open Logary.EventsProcessing
open Logary.Targets
open Logary.Targets.ElmahIO
open System.Threading

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  let logary =
    let elmahioConf =
      { logId = Guid.Parse(Environment.GetEnvironmentVariable("ELMAH_IO_LOG_ID")) 
        apiKey = "api key form elmah io"}

    Config.create "Logary.ElmahIO" "localhost"
    |> Config.targets [
        Console.create Console.empty "console"
        ElmahIO.create elmahioConf "elmah.io"
      ] 
    |> Config.processing (Events.events |> Events.sink ["console";"elmah.io";])
    |> Config.build
    |> run

  let logger =
    logary.getLogger (PointName [| "Logary"; "Samples"; "main" |])

  Message.templateFormat("{userName} logged in", [| "haf" |])
  |> Logger.logSimple logger

  Message.eventFormat (Info, "{userName} logged in", [| "adam" |])
  |> Logger.logSimple logger

  mre.Wait()
  0
