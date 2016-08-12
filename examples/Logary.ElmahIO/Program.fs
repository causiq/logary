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
open Logary.Targets


open System.Threading

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  use logary =
    withLogaryManager "Logary.ElmahIO" (
      withTargets [
        Console.create Console.empty "console"
        ElmahIO.create { logId = "GUID_HERE" } "elmah.io"
      ] >>
      withRules [
        Rule.createForTarget "console"
        Rule.createForTarget "elmah.io"
      ]
      >> run
    )
    |> run

  let logger =
    logary.getLogger (PointName [| "Logary"; "Samples"; "main" |])

  Message.templateFormat "{userName} logged in" [| "haf" |]
  |> Logger.logSimple logger

  Message.eventFormat (Info, "{userName} logged in", [| "adam" |])
  |> Logger.logSimple logger

  mre.Wait()
  0
