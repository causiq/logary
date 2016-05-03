module Logary.Tests.Value

open Fuchu
open Logary

module Assert = ExpectoPatronum.Expect

[<Tests>]
let valueTests =
  testList "value tests" [
    testCase "converting boxed int array" <| fun _ ->
      Assert.equal (Value.ofObject (box [| 1; 2; 3 |]))
                   (Array [ Int64 1L; Int64 2L; Int64 3L ])
                   "should convert boxed array of ints to Value.Array of Int64"

    testProperty "converting arbitrary array" <| fun (arr : _[]) ->
      Value.ofObject arr |> ignore
  ]