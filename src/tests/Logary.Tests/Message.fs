module Logary.Tests.Message

open Fuchu
open Fuchu.FuchuFsCheck
open FsCheck
open Logary.Utils.Chiron
open Logary

let inline roundTrip (m : 'a) : 'a =
  m |> Json.serialize |> Json.format |> Json.parse |> Json.deserialize

[<Tests>]
let tests =
  testList "Serialization" [
      testCase "Can round trip fields" <| fun () ->
        let f = Field (Logary.String "hello", None)
        Assert.Equal("field round trip", f, roundTrip f)

      testProperty "Can round trip all fields" <| fun (f : Field) ->
        f = roundTrip f

      testCase "Can round trip a specific message 1" <| fun () ->
        let m = {
            name      = PointName ["pn"]
            value     = Event "Let's see"
            fields    = Map [
                          PointName ["pn"; "pn2"], Field (Logary.String "hello world", None)
                        ]
            session   = Logary.Bool true
            context   = Map.empty
            level     = Debug
            timestamp = 0L
          }
        Assert.Equal("roundtrip", m, roundTrip m)

      testCase "Can round trip a specific message 2" <| fun () ->
        let m = {
            name      = PointName ["pn"]
            value     = Event "Let's see"
            fields    = Map [
                          PointName ["errors"], Field (Logary.Array [
                            (Logary.String "hello world 1")
                            (Logary.String "hello world 2")
                          ], None)
                        ]
            session   = Logary.Bool true
            context   = Map.empty
            level     = Debug
            timestamp = 0L
          }
        Assert.Equal("roundtrip", m, roundTrip m)

      testProperty "Serialization of message can round trip" <| fun (message : Message) ->
        let message' = { message with value = Event "hello world" }

        message' = roundTrip message'
    ]
