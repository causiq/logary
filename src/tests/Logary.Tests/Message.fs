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
        let! rawNames = Gen.listOfLength length (Arb.Default.NonEmptyString().Generator)
        let names =
          rawNames
          |> List.map (fun (NonEmptyString s) -> s)
          |> List.map (fun s -> s.Replace('.', 'a'))
          |> List.toArray
        return PointName names
    } |> Arb.fromGen
  // not all floats round trip exactly, we don't care
  static member SafeFloat () =
    Arb.Default.Int32()
    |> Arb.convert float int32
  static member Value () =
    let rec inner depth =
      gen {
        let! i = Gen.choose (1, 9)
        match i with
        | 1 ->
          let! str = LogaryArbs.string().Generator
          return String str
        | 2 ->
          let! b = Arb.generate<bool>
          return Bool b
        | 3 ->
          let! f = LogaryArbs.SafeFloat().Generator
          return Float f
        | 4 ->
          let! i64 = Arb.generate<int64>
          return Int64 i64
        | 5 ->
          let! bi = Arb.generate<bigint>
          return BigInt bi
        | 6 ->
          let! bytes = Arb.generate<byte []>
          let! ct = Arb.generate<ContentType>
          return Binary (bytes, ct)
        | 7 ->
          let! (a, b) = Arb.generate<int64 * int64>
          return Fraction (a, b)
        | 8 ->
          if depth <= 3 then
            let! (PointName names) = LogaryArbs.PointName().Generator
            let! values =
              [for i in 1..Array.length names -> inner (depth + 1)]
              |> List.fold (fun vs v -> gen {
                let! value = v
                let! values = vs
                return value::values
              }) (Gen.constant [])

            let o =
              values
              |> Seq.zip names
              |> Map.ofSeq
            return Object o
          else
            let! str = Arb.generate<string>
            return String str
        | 9 ->
          if depth <= 3 then
            let! i = Gen.choose (1, 5)
            let! values =
              [for i in 1..i -> inner (depth + 1)]
              |> List.fold (fun vs v -> gen {
                let! value = v
                let! values = vs
                return value::values
              }) (Gen.constant [])
            return Array values
          else
            let! str = Arb.generate<string>
            return String str
        | _ ->
          let! str = Arb.generate<string>
          return String str

        }
    inner 0


let config = { Config.Default with Arbitrary = [typeof<LogaryArbs>] }

[<Tests>]
let tests =
  testList "Serialization" [
      testCase "Can round trip fields" <| fun () ->
        let f = Field (Logary.String "hello", None)
        Assert.Equal("field round trip", f, roundTrip f)

      testPropertyWithConfig { config with MaxTest = 5 } "Can round trip all fields" <| fun (f : Field) ->
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

      testPropertyWithConfig { config with MaxTest = 1 } "Serialization of message can round trip" <| fun (message : Message) ->
        message = roundTrip message
    ]
