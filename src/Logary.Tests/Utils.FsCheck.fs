module Logary.Tests.FsCheck

open Expecto
open FsCheck

let tests = [
  testPropertyWithConfig fsc "NormalFloat" <| fun (NormalFloat f) ->
    Expect.isNotNaN f "Should be a number"
    Expect.isNotInfinity f "Should be a real number"
    Expect.isNotPositiveInfinity f "Should be a real number"
    Expect.isNotNegativeInfinity f "Should be a real number"
    true
]
