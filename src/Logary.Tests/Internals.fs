module Logary.Tests.LoggerExtensions

open System.Collections.Generic

open Fuchu

open Logary.Internals

[<Tests>]
let maps =
  testList "extension tests" [
    testCase "converts primitive to empty map" <| fun _ ->
      let subject = Map.fromObj 3
      Assert.Equal("shold be empty map", Map.empty, subject)
    testCase "converts null to empty map" <| fun _ ->
      let subject = Map.fromObj Unchecked.defaultof<obj>
      Assert.Equal("should be empty map", Map.empty, subject)
    testCase "given invalid key" <| fun _ ->
      let toDic l = (Dictionary<int, _>(l |> Map.ofList))
//      let subject : Map<int, obj> = Map.fromObj (toDic [4, box 4; 3, box 4M])
      // TODO
      ()
    testCase "converts System.Collections.Generic.Dictionary<,> to map with equivalent keys" <| fun _ ->
      let toDic l = (Dictionary<_, _>(l |> Map.ofList))
      let subject = Map.fromObj (toDic ["a", box 4; "b", box 4M])
      Assert.Equal("should equal map",
                   ["a", box 4; "b", box 4M] |> Map.ofList,
                   subject)
    ]
