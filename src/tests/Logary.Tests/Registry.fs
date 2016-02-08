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
          "Hello world" |> Logger.info logger |> run
          "Goodbye cruel world" |> Logger.fatal logger' |> run
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
          "hi there" |> Logger.info logger |> run
          logary |> Config.shutdownSimple |> run |> ignore
          printfn "unit test back in control"
          let didLog =
            Alt.choose [
              timeOut (TimeSpan.FromMilliseconds 20.0) ^->. false
              ("after shutdown" |> Logger.info logger) ^->. true
            ] |> run
          Assert.Equal("should be false", false, didLog)
          logary |> finaliseLogary
          out.ToString())
        |> should contain "hi there"
        |> shouldNot contain "after shutdown"
        |> thatsIt
    ]