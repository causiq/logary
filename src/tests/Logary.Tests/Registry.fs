module Logary.Tests.Registry

open System
open System.IO
open System.Text.RegularExpressions
open Fuchu
open Hopac
open Hopac.Infixes
open Swensen.Unquote
open TestDSL
open Fac
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
        let logger = pnp "a.b.c.d" |> Registry.getLogger logary.registry |> run
        let logger' = pnp "a.b.c.d" |> (logary |> asLogManager).getLogger
        (because "logging normally" <| fun () ->
          Message.eventInfo "Hello world" |> Logger.log logger |> run
          Message.eventFatal "Goodbye cruel world" |> Logger.log logger' |> run
          logary |> finaliseLogary
          out.ToString(), err.ToString())
        |> theTuple
          (fun first second ->
            first |> should contain "Hello world" |> thatsIt
            second |> should contain "Goodbye cruel world" |> thatsIt)
        |> thatsIt

    yield testCase "after shutting down no logging happens" <| fun _ ->
      Fac.withLogary <| fun logary out err ->
        let logger = (pnp "a.b.c.d") |> Registry.getLogger logary.registry |> run
        (because "logging something, then shutting down" <| fun () ->
          Message.eventInfo "hi there" |> Logger.log logger |> run
          logary |> Config.shutdownSimple |> run |> ignore
          Message.eventInfo "after shutdown" |> Logger.log logger |> run
          out.ToString())
        |> should contain "hi there"
        |> shouldNot contain "after shutdown"
        |> thatsIt
    ]