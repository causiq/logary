module Logary.Tests.Registry

open Hopac
open Swensen.Unquote
open Fuchu
open TestDSL
open Fac

open System
open System.IO
open System.Text.RegularExpressions

open NodaTime

open Logary
open Logary.Targets
open Logary.Configuration

// framework API tests

[<Tests>]
let registry =
  testList "Registry" [
    yield testCase "getLogger" <| fun _ ->
      Fac.withLogary <| fun logary out err ->
        let logger = (pnp "a.b.c.d") |> Registry.getLogger logary.registry |> Job.Global.run
        let logger' = (pnp "a.b.c.d") |> (logary |> asLogManager).getLogger
        (because "logging normally" <| fun () ->
          "Hello world" |> Logger.info logger
          "Goodbye cruel world" |> Logger.fatal logger'
          logary |> finaliseLogary
          out.ToString(), err.ToString())
        |> theTuple
          (fun first second ->
            first |> should contain "Hello world" |> thatsIt
            second |> should contain "Goodbye cruel world" |> thatsIt)
        |> thatsIt

    yield testCase "after shutting down no logging happens" <| fun _ ->
      Fac.withLogary <| fun logary out err ->
        let logger = (pnp "a.b.c.d") |> Registry.getLogger logary.registry |> Job.Global.run
        (because "logging something, then shutting down" <| fun () ->
          "hi there" |> Logger.info logger
          logary |> Config.shutdownSimple |> Job.Global.run |> ignore
          "after shutdown" |> Logger.info logger
          out.ToString())
        |> should contain "hi there"
        |> should_not contain "after shutdown"
        |> thatsIt
    ]