module Logary.Tests.Message

open System
open Expecto
open Expecto.Flip
open FsCheck
open Logary
open NodaTime

[<Tests>]
let tests =
  testList "message" [
    testList "event" [
      testCase "c'tor" <| fun _ ->
        let mid = Id.create()
        let m = Model.Event("Hello world", Some (money Currency.EUR 1.), 1L, mid, PointName.parse "a.b.c", Info)
        m.received <- Some 2L
        m.setFieldValues(Map [ "a", Value.Str "b" ])
        m.setContextValues(Map [ "b", Value.Str "c" ])
        m.addExn (raisedExn "testing event message")
        m.setGauge("Startup time", Duration.FromSeconds 1.23)
        m.tag "panther"

        m.level
          |> Expect.equal "Right level" Info
        m.event
          |> Expect.equal "Has 'Hello world' message" "Hello world"
        m.monetaryValue
          |> Expect.equal "Eq 1 EUR" (Some (money Currency.EUR 1.))
        m.timestamp
          |> Expect.equal "Eq 1" 1L
        m.received
          |> Expect.equal "Eq 2" (Some 2L)
        m.id
          |> Expect.equal "Message id" mid
        m.name
          |> Expect.equal "a.b.c" (PointName.parse "a.b.c")
        m.fields.["a"]
          |> Expect.equal "has 'a' field" (Value.Str "b")
        m.context.["b"]
          |> Expect.equal "has 'b' field" (Value.Str "c")
        m.error
          |> Expect.isNone "Should have no error info attached"
        m.targets
          |> Expect.isEmpty "Should have no targets"
        m.error
          |> Expect.isSome "Has an error value"
        m.error.Value.message
          |> Expect.equal "Has the message" (Some "testing error message")
        m.counterConf
          |> Expect.isNone "Has no counter conf configured by default"
        m.gauges.["Startup time"]
          |> Expect.equal "Has the right startup time" (Gauge.ofDuration (Duration.ofSeconds 1.23))
        m.hasTag "panther"
          |> Expect.isTrue "Has the 'panther' tag"

      testCase "c'tor generates MessageId" <| fun () ->
        let m = Model.Event "e"
        m.id.isZero
          |> Expect.isFalse "Should have generated an Id"
    ]

    testList "gauge" [
      testCase "c'tor" <| fun _ ->
        let acc = Gauge (Value.Float 11.4, U.Div(U.Metres, U.Pow(U.Seconds, 2.0))) // acc
        let m = Model.GaugeMessage (acc, Map [ "gauge_name", "Car.Speedometer"; "measurement", "dv/dt" ])
        m.gauge
          |> Expect.equal "Equals the gauge" acc
        m.gauges
          |> Expect.isEmpty "Has no other gauges"
    ]

    testList "histogram" []

    testList "identify user" []

    testList "set user prop" []

    testList "base" [
      testCase "tags" <| fun () ->
        let gm = Model.GaugeMessage(Gauge.ofSeconds 30, Map [ "gauge_name", "db"; "measurement", "request_seconds" ])
        let tags =
          Arb.generate<NonEmptyString>
            |> Gen.sample 0 5
            |> List.distinct
            |> List.map (fun (NonEmptyString name) -> name)

        for tag in tags do
          gm.tag tag

        let found = gm.getAllTags() |> List.ofSeq

        found.Length
          |> Expect.equal  "Should get same length after add tags" tags.Length
        found
          |> Expect.containsAll "Isomorphic mapping" tags
        tags
          |> List.forall gm.hasTag
          |> Expect.isTrue "Should have all tags given to it as seen by `getTag`."

      testCase "addExn" <| fun _ ->
        let error = ArgumentNullException("e2")
        let m = Model.Event "Unhandled exception message"
        m.addExn error

        m.error
          |> Expect.isSome "Has an ErrorInfo set"
        m.error.Value.inner
          |> Expect.isNone "inner is None"

        m.error.Value.message
          |> Expect.isSome "Has a message"
        m.error.Value.message.Value
          |> Expect.equal "Has the right message" "e2"

        m.error.Value.errorType
          |> Expect.isSome "Has an error type"
        m.error.Value.errorType.Value
          |> Expect.equal "Has the right error type" "System.ArgumentNullException"

        m.error.Value.stackTrace.frames
          |> Expect.isNonEmpty "Has stack frames, at least one"
    ]
  ]
  |> testLabel "logary"