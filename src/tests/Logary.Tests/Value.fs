module Logary.Tests.Value

open System
open System.Collections.Generic
open Fuchu
open Logary
open FsCheck

module Assert = ExpectoPatronum.Expect

[<Tests>]
let valueTests =
  testList "value tests" [
    testCase "converting boxed int array" <| fun _ ->
      Assert.equal (Value.ofObject (box [| 1; 2; 3 |]))
                   (Array [ Int64 1L; Int64 2L; Int64 3L ])
                   "should convert boxed array of ints to Value.Array of Int64"

    testCase "converting null array" <| fun _ ->
      let arr = [|null|]
      Value.ofObject arr |> ignore

    testProperty "converting null IEnumerable<>" <| fun (aList : list<_>)  ->
      let a = aList :>  IEnumerable<obj>
      Value.ofObject a |> ignore

    testProperty "converting arbitrary array" <| fun (arr : _[]) ->
      Value.ofObject arr |> ignore

    testProperty "converting arbitrary object" <| fun (a : System.Object) ->
      Value.ofObject a |> ignore

    testProperty "converting arbitrary FSharp list" <| fun (a : list<_>) ->
      Value.ofObject a |> ignore

    testProperty "converting arbitrary System.Collections.Generic.List" <| fun (a : List<_>) ->
      Value.ofObject a |> ignore
    
    testProperty "converting arbitrary list" <| fun (a : list<KeyValuePair<string, obj>>) ->
      Value.ofObject a |> ignore
  ]

  
  