module Logary.Tests.Units

open Logary
open Expecto

[<Tests>]
let tests =
  testList "units" [
    testList "scaling" [
      testList "s" [
        testCase "0.0000001 s" <| fun _ ->
          Expect.equal (Units.scale Seconds 0.0000001) (100., "ns")
                      "Should be scaled to 100 ns"

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

      testList "bits" [
        testCase "1 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 1.) (1., "bit")
                      "Should be passed through (1 bit)"

        testCase "100 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 100.) (100., "bit")
                       "Should be passed through (100 bit)"

        testCase "10000 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 10000.) (10., "kbit")
                       "Should be scaled to 10 kbit"

        testCase "1 000 000 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 1000000.) (1., "Mbit")
                       "Should be scaled to 1 Mbit"

        testCase "10 000 000 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 10000000.) (10., "Mbit")
                       "Should be scaled to 10 Mbit"

        testCase "1 000 000 000 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 1000000000.) (1., "Gbit")
                       "Should be scaled to 10 Gbit"

        testCase "2 500 000 000 000 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 2500000000000.) (2.5, "Tbit")
                       "Should be scaled to 2.5 Tbit"

        testCase "2 000 000 000 000 000 bit" <| fun _ ->
          Expect.equal (Units.scale Bits 2000000000000000.) (2., "Pbit")
                       "Should be scaled to 2. Pbit"

        testCase "2 000 000 000 000 000 000 bit" <| fun _ ->
          let value, units = Units.scale Bits 2000000000000000000.
          Expect.floatEqual value 2000. None "Should be scaled to 2 000 Pbit"
          Expect.equal units "Pbit" "Should be scaled to 2 000 Pbit"
      ]
    ]
  ]
