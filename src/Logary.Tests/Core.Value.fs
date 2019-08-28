module Logary.Tests.Value

open Logary
open Expecto

let tests = [
  testCase "Float" <| fun () -> ignore (Float 3.)
  testCase "Int64" <| fun () -> ignore (Int64 3L)
  testCase "BigInt" <| fun () -> ignore (BigInt 3I)
  testPropertyWithConfig fsc "Value" <| fun (value: Value) ->
    match value with
    | Float f ->
      Expect.isNotNaN f "Should be a number"
      Expect.isNotInfinity f "Should be a real number"
      Expect.isNotPositiveInfinity f "Should be a real number"
      Expect.isNotNegativeInfinity f "Should be a real number"
    | _ ->
      ()
    true
]

