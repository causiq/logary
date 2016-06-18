module Logary.Tests.Message

open Fuchu
open Fuchu.FuchuFsCheck
open FsCheck
open System
open NodaTime
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
    inner 0 |> Arb.fromGen


let config = { Config.Default with Arbitrary = [typeof<LogaryArbs>] }

let exampleJson = """{"context":{"service":"qvitoo.com","logger":"ErrorLogger","user":{"id":"0d11fb5eb21342e99c1c032e338d2dc6","email":"unverified"}},"name":["qvitoo","com","ErrorLogger"],"messageId":"b0c2832666bebdbefc9e1478b363dbcaaf6c1b82a0a1957bf82f3db9bf031299","value":{"event":"Window onerror"},"fields":{"errors":[{"type":"error","status":502,"xhr":{},"originalEvent":{"isTrusted":true}}]},"level":"error","timestamp":1461937540536000000}"""

type DU =
  | A
  | B

[<Tests>]
let tests =
  testList "serialization" [
    testCase "can round trip fields" <| fun () ->
      let f = Field (Logary.String "hello", None)
      Assert.Equal("field round trip", f, roundTrip f)

    testPropertyWithConfig config "can round trip all fields" <| fun (f : Field) ->
      f = roundTrip f

    testCase "can add Map<string, string> field" <| fun _ ->
      Message.eventError "Hi"
      |> Message.setField "data" (Map ["a", "b"])
      |> ignore

    testCase "can add value from Map<string, string>" <| fun _ ->
      Value.ofObject (Map ["a", "b"]) |> ignore

    testCase "can add value from boxed Map<string, string>" <| fun _ ->
      Value.ofObject (Map ["a", "b"] |> box) |> ignore

    testCase "can add value from boxed Map<string, Map<string, string>>" <| fun _ ->
      Value.ofObject (Map ["a", box (Map ["c", "b"])] |> box) |> ignore

    testCase "can create value from DU" <| fun _ ->
      A
      |> Value.ofObject
      |> ignore

    testCase "can deserialize exampleJson" <| fun () ->
      let (m : Message) = exampleJson |> Json.parse |> Json.deserialize
      Assert.Equal("example json", typeof<Message>, m.GetType())

    testCase "can round trip a specific message 1" <| fun () ->
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
