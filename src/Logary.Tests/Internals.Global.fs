module Logary.Tests.Global

open Expecto
open Expecto.Flip
open Logary
open Logary.Internals

[<Tests>]
let tests =
  testList "global" [
    testCase "NullLogger level" <| fun _ ->
      NullLogger.instance.level
        |> Expect.equal "should be Fatal" Fatal
  ]
  |> testLabel "logary"

