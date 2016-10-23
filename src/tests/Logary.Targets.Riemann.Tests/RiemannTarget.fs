module Logary.Tests.Riemann

open System
open System.Collections.Generic
open System.IO
open Expecto
open ProtoBuf
open Logary.Targets.Riemann
open Logary.Riemann.Messages

let roundtrip<'a when 'a : equality> (x : 'a) =
  use ms = new MemoryStream()
  Serializer.Serialize<'a>(ms, x)
  ms.Seek(0L, SeekOrigin.Begin) |> ignore
  let res = Serializer.Deserialize<'a> ms
  sprintf "deserialised output of %s should equal input" (typeof<'a>.Name) |> ignore
  Expect.equal("should equal after roundtrip", res, x)

let list (items : 'a list) =
  let l = List<'a>()
  List.iter l.Add items
  l

[<Tests>]
let tests =
  testList "serialisation and deserialisation" [
    testCase "Msg" <| fun _ ->
      roundtrip <| Msg(true, "", List<_>(), Query("are you there?"), List<_>())

    testCase "State" <| fun _ ->
      roundtrip <| State(1L, "st", "svc", "hst", "desc", false, list ["t1"; "t2"], 2.)

    testCase "Event" <| fun _ ->
      roundtrip <| Event.createDouble(1337., 1L, "st", "svc", "hst", "desc", list ["t1"; "t2"], 2.f, [])

    testCase "Query" <| fun _ ->
      roundtrip <| Query("hi")
    ]
