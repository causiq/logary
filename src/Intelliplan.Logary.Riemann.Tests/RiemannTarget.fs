module Logary.Riemann.Tests

open System
open System.Collections.Generic
open System.IO

open Fuchu
open Swensen.Unquote

open ProtoBuf

open Logary.Target.Riemann

let roundtrip (x : 'a) =
  use ms = new MemoryStream()
  Serializer.Serialize<'a>(ms, x)
  ms.Seek(0L, SeekOrigin.Begin) |> ignore
  let res = Serializer.Deserialize<'a>(ms)
  sprintf "deserialised output of %s should equal input" (typeof<'a>.Name) |> ignore
  test <@ res = x @>

let list (items : 'a list) =
  let l = List<'a>()
  List.iter l.Add items
  l

[<Tests>]
let tests =
  testList "serialisation and deserialisation" [
    testCase "Msg" <| fun _ ->
      roundtrip <| Msg(true, "", List<_>(), Query("are you there?"), List<_>())
    testCase "Event" <| fun _ ->
      roundtrip <| { time = 1L; state = "st"; service = "svc"; host = "hst";
                     description = "desc"; tags = list ["t1"; "t2"]; ttl = 0.; metric_f = 3. }
    testCase "State" <| fun _ ->
      roundtrip <| { time = 1L; state = "st"; service = "svc"; host = "hst";
                     description = "desc"; tags = list ["t1"; "t2"]; ttl = 0.; metric_f = 3. }
    testCase "Query" <| fun _ ->
      roundtrip <| Query("hi")
    ]

[<EntryPoint>]
let main args = defaultMainThisAssembly args