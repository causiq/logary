module Logary.Tests.Internals

open System.Collections.Generic
open Fuchu
open Logary.Internals

[<Tests>]
let maps =
  testList "Map.fromObject" [
    testCase "converts primitive to empty map" <| fun _ ->
      let subject = Map.fromObject 3
      Assert.Equal("shold be empty map", Map.empty, subject)

    testProperty "passes Map<string,obj> through unchanged" <| fun (extra : Map<string, obj>) ->
      let data = [ "a", [ "b", box 3; "c", box extra ] |> Map.ofList |> box ]
                 |> Map.ofList
                 |> box
      let subject = Map.fromObject data
      Assert.equal subject (data :?> Map<string, obj>) "should equal input"

    testCase "converts null to empty map" <| fun _ ->
      let subject = Map.fromObject Unchecked.defaultof<obj>
      Assert.Equal("should be empty map", Map.empty, subject)

    testProperty "given int keys" <| fun (map : Map<int, string>) ->
      let subject : Map<string, obj> = Map.fromObject (Dictionary<int, string>(map))
      Assert.Equal("should contain mapped ints",
                   map,
                   subject |> Map.fold (fun acc k v -> acc |> Map.add (int k) (v :?> string)) Map.empty)

    testCase "converts System.Collections.Generic.Dictionary<,> to map with equivalent keys" <| fun _ ->
      let toDic l = (Dictionary<_, _>(l |> Map.ofList))
      let subject = Map.fromObject (toDic ["a", box 4; "b", box 4M])
      Assert.Equal("should equal map",
                   ["a", box 4; "b", box 4M] |> Map.ofList,
                   subject)

    testCase "given repeated keys" <| fun _ ->
      let raw   = [ KeyValuePair<_, _>(4, "hi there" |> box)
                    KeyValuePair<_, _>(4, "second value" |> box) ]
      let input = raw :> seq<KeyValuePair<_, _>>
      let subject = Map.fromObject (box input)
      Assert.Equal("should have second item", "second value", unbox subject.["4"])

    testProperty "given list<KVP<string, string>>" <| fun kvs ->
      let raw : KeyValuePair<string, string> list = kvs
      let input = raw :> seq<KeyValuePair<_, _>>
      Map.fromObject (box input) |> ignore

    testProperty "given array<KVP<string, string>>" <| fun kvs ->
      let raw : KeyValuePair<string, string> array = kvs
      let input = raw :> seq<KeyValuePair<_, _>>
      Map.fromObject (box input) |> ignore

    testProperty "given seq<string * string>" <| fun kvs ->
      let raw : (string * string) list = kvs
      let input = raw :> seq<string * string>
      Map.fromObject (box input) |> ignore
    ]
