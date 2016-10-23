module Logary.Tests.Internals

open System.Collections.Generic
open Expecto
open Logary.Internals

[<Tests>]
let maps =
  testList "Map.ofObject" [
    testCase "converts primitive to empty map" <| fun _ ->
      let subject = Map.ofObject 3
      Expect.equal subject Map.empty "Should be empty map."

    testProperty "passes Map<string,obj> through unchanged" <| fun (extra : Map<string, obj>) ->
      let data = [ "a", [ "b", box 3; "c", box extra ] |> Map.ofList |> box ]
                 |> Map.ofList
                 |> box
      let subject = Map.ofObject data
      Expect.equal subject (data :?> Map<string, obj>) "Should equal input."

    testCase "converts null to empty map" <| fun _ ->
      let subject = Map.ofObject Unchecked.defaultof<obj>
      Expect.equal subject Map.empty "Should be empty map."

    testProperty "given int keys" <| fun (map : Map<int, string>) ->
      let subject : Map<string, obj> = Map.ofObject (Dictionary<int, string>(map))
      Expect.equal (subject |> Map.fold (fun acc k v -> acc |> Map.add (int k) (v :?> string)) Map.empty)
                   map
                   "should contain mapped ints"

    testCase "converts System.Collections.Generic.Dictionary<,> to map with equivalent keys" <| fun _ ->
      let toDic l = (Dictionary<_, _>(l |> Map.ofList))
      let subject = Map.ofObject (toDic ["a", box 4; "b", box 4M])
      Expect.equal subject
                   (["a", box 4; "b", box 4M] |> Map.ofList)
                   "Should equal map."

    testCase "given repeated keys" <| fun _ ->
      let raw   = [ KeyValuePair<_, _>(4, "hi there" |> box)
                    KeyValuePair<_, _>(4, "second value" |> box) ]
      let input = raw :> seq<KeyValuePair<_, _>>
      let subject = Map.ofObject (box input)
      Expect.equal (unbox subject.["4"]) "second value" "should have second item"

    testProperty "given list<KVP<string, string>>" <| fun kvs ->
      let raw : KeyValuePair<string, string> list = kvs
      let input = raw :> seq<KeyValuePair<_, _>>
      Map.ofObject (box input) |> ignore

    testProperty "given array<KVP<string, string>>" <| fun kvs ->
      let raw : KeyValuePair<string, string> array = kvs
      let input = raw :> seq<KeyValuePair<_, _>>
      Map.ofObject (box input) |> ignore

    testProperty "given seq<string * string>" <| fun kvs ->
      let raw : (string * string) list = kvs
      let input = raw :> seq<string * string>
      Map.ofObject (box input) |> ignore
    ]
