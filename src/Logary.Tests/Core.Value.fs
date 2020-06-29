module Logary.Tests.Value

open Logary
open System
open Expecto
open Expecto.Flip

[<Tests>]
let tests =
  testList "Value" [
    testPropertyWithConfig fsc "Value" <| fun (value: Value) ->
      match value with
      | Value.Float f ->
        f |> Expect.isNotNaN "Should be a number"
        f |> Expect.isNotInfinity "Should be a real number"
        f |> Expect.isNotPositiveInfinity "Should be a real number"
        f |> Expect.isNotNegativeInfinity "Should be a real number"
      | _ -> ()

    testList "Value :> IFormattable |> string" (
       [ Value.Float 62., "62"
         Value.Int64 84598L, "84598"
         Value.BigInt 1024I, "1024"
         Value.Fraction(2L, 5L), "2/5" ]
       |> List.map (fun (v, e) -> (v :> IFormattable).ToString("G", Culture.invariant), e)
       |> List.map (fun (actual, expected) ->
          testCase actual (fun () ->
            actual |> Expect.equal "Should have correct result" expected)))

  ]
  |> testLabel "logary"
