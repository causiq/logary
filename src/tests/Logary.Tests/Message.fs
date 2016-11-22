module Logary.Tests.Message

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck
open Hopac
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

let getValueAndUnit (msg : Message) =
  match msg.value with
  | Gauge (value, units) -> value, units
  | Derived (_, _) -> Tests.failtestf "Unexpected Gauge value"
  | Event _ -> Tests.failtestf "Unexpected Event value"

[<Tests>]
let tests =
  testList "serialization" [
    testCase "can round trip fields" <| fun () ->
      let f = Field (Logary.String "hello", None)
      Expect.equal (roundTrip f) f "Field round trip."

    testCase "can tag" <| fun () ->
      let subject = 
        Message.event Debug "A debug message"
        |> Message.tag "web"
      Expect.isTrue (Message.hasTag "web" subject) "Should be tagged with 'web'."

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

    testCase "can add tags" <| fun _ ->
      let subject =
        Message.event Debug "Hi"
        |> Message.tag "b"
        |> Message.tag "a"
      Expect.equal subject.context.["tags"] (Array [ String "a"; String "b" ]) "Should have tags"

    testCase "can deserialize exampleJson" <| fun () ->
      let (m : Message) = exampleJson |> Json.parse |> Json.deserialize
      Expect.equal (m.GetType()) typeof<Message> "Example JSON."

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
      Expect.equal (roundTrip m) m "roundtrip"

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
      Expect.equal (roundTrip m) m "Roundtripping."

    testCase "DateTimeOffset.timestamp" <| fun () ->
      DateTimeOffset.UtcNow.timestamp |> ignore

    testCase "Message.time can be called" <| fun () ->
      let res, msg =
        Message.time (PointName [| "Tests.Message.time" |]) (fun () -> 367) ()

      let value, units = getValueAndUnit msg

      Expect.equal res 367 "should have correct return value"
      Expect.equal units (Scaled(Seconds, 1e9))  "correctly scaled unit (nanos)"

    testCase "PointName.setEnding" <| fun _ ->
      let pn = PointName [| "A"; "B" |] |> PointName.setEnding "C"
      Expect.equal pn (PointName [| "A"; "B"; "C" |]) "Should have set the ending"

    testCase "Message.timeAsync can be called" <| fun _ ->
      let slow () = async { do! Async.Sleep 10
                            return 357 }
      let res, msg = Message.timeAsync (PointName.parse "A.B.C") slow () |> Async.RunSynchronously

      let value, units = getValueAndUnit msg

      Expect.equal res 357 "Should have value"
      Expect.equal units (Scaled(Seconds, 1e9))  "correctly scaled unit (nanos)"

    testCase "Message.timeJob can be called" <| fun _ ->
      let slow () = job { do! timeOutMillis 10
                          return 357 }
      let res, msg = Message.timeJob (PointName.parse "A.B.C") slow () |> run

      let value, units = getValueAndUnit msg

      Expect.equal res 357 "Should have value"
      Expect.equal units (Scaled(Seconds, 1e9))  "correctly scaled unit (nanos)"

    testPropertyWithConfig config "Serialization of message can round trip" <| fun (message : Message) ->
      message = roundTrip message

    testProperty "DateTimeOffset" <| fun (ts : DateTimeOffset) ->
      let ticks = ts.timestamp |> DateTimeOffset.ticksUTC
      let recreated = DateTimeOffset(ticks, TimeSpan.Zero).Ticks
      Expect.equal recreated (ts.Ticks) "should equal after conversion"

  ]
