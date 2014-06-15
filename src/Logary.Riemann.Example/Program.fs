// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System

open System.Text.RegularExpressions

open Logary
open Logary.Configuration

open Logary.Target
open Logary.Logging
open Logary.Targets

let runLogary serviceName fConf =
  fConf (confLogary serviceName)
  |> validateLogary
  |> runLogary

let alt () =
  let logary =
    runLogary "Riemann.Example" (
      withTargets [
        Riemann.create (Riemann.RiemannConf.Default) "riemann"
        Console.create (Console.ConsoleConf.Default) "console"
      ] >>
      withRules [
        Rule.Create(Regex(@".*"), "riemann", (fun _ -> true), LogLevel.Verbose)
        Rule.Create(Regex(@".*"), "console", (fun _ -> true), LogLevel.Verbose)
      ]
    )
  ()

[<EntryPoint>]
let main argv =
  use logary =
    withLogary "Riemann.Example" (
      withTargets [
        Riemann.create (Riemann.RiemannConf.Create(tags = ["riemann-health"])) "riemann"
        Console.create (Console.ConsoleConf.Default) "console"
      ] >>
      withRules [
        Rule.Create(Regex(@".*"), "riemann", (fun _ -> true), LogLevel.Verbose)
        Rule.Create(Regex(@".*"), "console", (fun _ -> true), LogLevel.Verbose)
      ]
    )

  let logger = logary.GetLogger "Riemann.Example"
  ("disk /", 0.456) ||> Log.gauge logger

  0 // return an integer exit code
