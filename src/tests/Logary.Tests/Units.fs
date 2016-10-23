module Logary.Tests.Units

open Logary
open Expecto

[<Tests>]
let tests =
  testList "units" [
    testList "scaling" [
      testList "s" [
        testCase "1 s" <| fun _ ->
          Expect.equal (Units.scaleFull Seconds 1.) (1., "s")
                      "Should be scaled to 1 s"
        testCase "10 s" <| fun _ ->
          Expect.equal (Units.scaleFull Seconds 10.) (10., "s")
                      "Should be scaled to 10 s"
        testCase "60 s" <| fun _ ->
          Expect.equal (Units.scaleFull Seconds 60.) (1., "min")
                      "Should be scaled to 1 min"
        testCase "100 s" <| fun _ ->
          Expect.equal (Units.scaleFull Seconds 100.) (1.33333333333333, "min")
                      "Should be scaled to 1 1/3 min"
      ]
    ]
  ]
