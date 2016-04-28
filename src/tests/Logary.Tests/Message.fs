module Logary.Tests.Message

open Fuchu
open Fuchu.FuchuFsCheck
open FsCheck
open Logary.Utils.Chiron
open Logary

let inline roundTrip (m : 'a) : 'a =
  m |> Json.serialize |> Json.format |> Json.parse |> Json.deserialize

type LogaryArbs () =
  static member string () =
    Arb.Default.NonEmptyString ()
    |> Arb.convert (fun (NonEmptyString s) -> s) (fun s -> NonEmptyString s)
  static member PointName () =
    gen {
        let! length = Gen.choose (1, 5)
        let! names = Gen.listOfLength length (Arb.Default.NonEmptyString().Generator)
        return PointName (List.map (fun (NonEmptyString s) -> s) names |> List.toArray)
    } |> Arb.fromGen

let config = { Config.Default with Arbitrary = [typeof<LogaryArbs>] }

[<Tests>]
let tests =
  testList "Serialization" [
      testCase "Can round trip fields" <| fun () ->
        let f = Field (Logary.String "hello", None)
        Assert.Equal("field round trip", f, roundTrip f)

      testPropertyWithConfig config "Can round trip all fields" <| fun (f : Field) ->
        f = roundTrip f

      testCase "Can round trip a specific message 1" <| fun () ->
        let m = {
            name      = PointName [|"pn"|]
            value     = Event "Let's see"
            fields    = Map [
                          PointName [|"pn"; "pn2"|], Field (Logary.String "hello world", None)
                        ]
            context   = Map.empty
            level     = Debug
            timestamp = 0L
          }
        Assert.Equal("roundtrip", m, roundTrip m)

      testCase "Can round trip a specific message 2" <| fun () ->
        let m = {
            name      = PointName [|"pn"|]
            value     = Event "Let's see"
            fields    = Map [
                          PointName [|"errors"|], Field
                            (Logary.Array [
                                (Logary.String "hello world 1")
                                (Logary.String "hello world 2")
                            ], None)
                        ]
            context   = Map.empty
            level     = Debug
            timestamp = 0L
          }
        Assert.Equal("roundtrip", m, roundTrip m)

      testPropertyWithConfig config "Serialization of message can round trip" <| fun (message : Message) ->
        message = roundTrip message
    ]
