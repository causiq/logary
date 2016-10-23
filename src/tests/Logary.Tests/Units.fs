module Logary.Tests.Units

open Logary
open Expecto

[<Tests>]
let tests =
  testList "units" [
    testList "scaling" [
      testList "s" [
        testCase "0.0001 s" <| fun _ ->
          Expect.equal (Units.scale Seconds 0.0001) (100., "µs")
                      "Should be scaled to 100 µs"
        testCase "0.1 s" <| fun _ ->
          Expect.equal (Units.scale Seconds 0.1) (100., "ms")
                      "Should be scaled to 100 ms"

        testCase "1 s" <| fun _ ->
          Expect.equal (Units.scale Seconds 1.) (1., "s")
                      "Should be scaled to 1 s"

        testCase "10 s" <| fun _ ->
          Expect.equal (Units.scale Seconds 10.) (10., "s")
                      "Should be scaled to 10 s"

        testCase "60 s" <| fun _ ->
          Expect.equal (Units.scale Seconds 60.) (1., "min")
                      "Should be scaled to 1 min"

        testCase "100 s" <| fun _ ->
          let value, units = Units.scale Seconds 100.
          Expect.floatEqual value 1.666666667 None
                            "Should be scaled to 1 2/3 min"
          Expect.equal units "min" "Should be scaled to minutes."
      ]
    ]
  ]
