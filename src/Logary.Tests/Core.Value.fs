module Logary.Tests.Value

open Logary
open System
open Expecto

let tests =
  [ testPropertyWithConfig fsc "Value"
    <| fun (value: Value) ->
      match value with
      | Value.Float f ->
        Expect.isNotNaN f "Should be a number"
        Expect.isNotInfinity f "Should be a real number"
        Expect.isNotPositiveInfinity f "Should be a real number"
        Expect.isNotNegativeInfinity f "Should be a real number"
      | _ -> ()

      testList "Value :> IFormattable |> string"
        ([ Value.Float 62., "62"
           Value.Int64 84598L, "84598"
           Value.BigInt 1024I, "1024"
           Value.Fraction(2L, 5L), "2/5" ]
         |> List.map (fun (v, e) -> (v :> IFormattable).ToString("G", Culture.invariant), e)
         |> List.map (fun (actual, expected) ->
            testCase actual (fun () -> Expect.equal actual expected "Should have correct result"))) ]
