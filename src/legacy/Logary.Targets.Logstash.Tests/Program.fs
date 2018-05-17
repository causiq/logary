module Program

open System
open NodaTime
open Expecto
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Targets
open Logary.Internals
open Logary.Tests
open System.Globalization
open System.Threading

let raisedExn msg =
  let e = ref None: exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

let now = Message.setUTCTicks (DateTime(2017,11,11).Ticks)

[<Tests>]
let target =
  testList "logstash" [
    TargetBaseline.basicTests "Logstash" (Logstash.create Logstash.empty) false

    testCase "serialise" <| fun _ ->
      let e1 = raisedExn "darn"
      let e2 = raisedExn "actual exn"

      let subject =
        Message.eventWarn "Testing started"
        |> Message.setName (PointName.ofArray [| "a"; "b"; "c" |])
        |> Message.setField "data-key" "data-value"
        |> Message.setField "tags" [ "integration" ]
        |> Message.setField "e" e1
        |> Message.setContext "service" "tests"
        |> Message.addExn e2
        |> now
        |> Logstash.serialise
        |> fun str -> str.Replace(@"\r\n", @"\n")  // test for different platform

      Expect.stringStarts subject """{"name":"a.b.c","value":"Testing started","level":"warn","timestamp":1510358400000000000,"context":{"_fields.data-key":"data-value","_fields.e":"System.ApplicationException: darn\n""" "should serialise to proper message"
      Expect.stringContains subject "\"service\":\"tests\"" "should contains service context info"
      Expect.stringContains subject "_logary.errors" "should contains errors"
    ]

[<EntryPoint>]
let main argv =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig argv
