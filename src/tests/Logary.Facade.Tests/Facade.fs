module Logary.Facade.Tests

open System
open Fuchu
open Logary.Facade
open ExpectoPatronum

[<Tests>]
let tests =
  let msg = Message.event Info "hi {name}"

  testList "generic" [
    testProperty "DateTime" <| fun (dt : DateTime) ->
      let ticks = dt |> DateTime.timestamp |> DateTime.ticksUTC
      let recreated = DateTime(ticks, DateTimeKind.Utc).Ticks
      Expect.equal recreated (dt.Ticks) "should equal on ticks after conversion"

    testProperty "DateTimeOffset" <| fun (ts : DateTimeOffset) ->
      let ticks = ts |> DateTimeOffset.timestamp |> DateTimeOffset.ticksUTC
      let recreated = DateTimeOffset(ticks, TimeSpan.Zero).Ticks
      Expect.equal recreated (ts.Ticks) "should equal after conversion"

    testCase "event" <| fun _ ->
      Message.event Info "hi {name}" |> ignore

    testCase "gauge" <| fun _ ->
      Message.gauge 5L "Scalar" |> ignore

    testCase "setSingleName" <| fun _ ->
      let msg = msg |> Message.setSingleName "Logary.Other"
      Expect.equal msg.name [| "Logary"; "Other" |] "Should have name set"

    testCase "setName" <| fun _ ->
      let msg = msg |> Message.setName [| "Logary"; "Other" |]
      Expect.equal msg.name [| "Logary"; "Other" |] "Should have name set"

    testCase "setFieldValue" <| fun _ ->
      let msg = msg |> Message.setFieldValue "fv" 33
      Expect.equal msg.fields.["fv"] (box 33) "Should have field"

    testCase "setTimestamp" <| fun _ ->
      let now = DateTimeOffset.UtcNow
      let msg = msg |> Message.setTimestamp (now |> DateTimeOffset.timestamp)
      Expect.equal msg.timestamp (now |> DateTimeOffset.timestamp) "Should have timestamp set"

    testCase "setLevel" <| fun _ ->
      Expect.equal msg.level Info "Initially Info"
      let msg = msg |> Message.setLevel Warn
      Expect.equal msg.level Warn "Should have Warn level now"

    testCase "ConsoleTarget logSimple" <| fun _ ->
      let logger = ConsoleWindowTarget(Debug) :> Logger
      Message.event Info "Aliens descended" |> logger.logSimple

    testProperty "Loggers.create" <| fun level ->
      Targets.create level |> ignore

    testProperty "Targets.create and logSimple" <| fun level ->
      let logger = Targets.create level
      Message.event Debug "Hi {name}" |> logger.logSimple
  ]