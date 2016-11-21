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
          Expect.floatEqual value 2. None "Should be scaled to 2. Ebit"
          Expect.equal units "Ebit" "Should be scaled to 2. Ebit"
      ]

      testList "bytes" [
        testCase "1 byte" <| fun _ ->
          Expect.equal (Units.scale Bytes 1.) (1., "B") "Should scale 1<->1 bytes"

        testCase "10 bytes" <| fun _ ->
          Expect.equal (Units.scale Bytes 10.) (10., "B") "Should not scale"

        testCase "1024 bytes" <| fun _ ->
          Expect.equal (Units.scale Bytes 1024.) (1., "KiB") "Should scale to KiB"

        testCase "(2^10)^2 bytes" <| fun _ ->
          Expect.equal (Units.scale Bytes (1024. * 1024.)) (1., "MiB") "Should scale to MiB"

        testCase "(2^10)^3 bytes" <| fun _ ->
          Expect.equal (Units.scale Bytes (1024. * 1024. * 1024.)) (1., "GiB")
                       "Should scale to GiB"

        testCase "(2^10)^4 bytes" <| fun _ ->
          Expect.equal (Units.scale Bytes (1024. * 1024. * 1024. * 1024.)) (1., "TiB")
                       "Should scale to TiB"
      ]

      testCase "scalars are not scaled" <| fun _ ->
        Expect.equal (Units.scale Scalar 12345678.) (12.345678, "M")
                     "Should not present a unit for Scalars"

      testCase "others are not scaled" <| fun _ ->
        Expect.equal (Units.scale (Other "reqs") 12345678.) (12345678., "reqs")
                     "Should not present a unit for Scalars"

      testCase "Percent are always scalled x100 and presented with a % symbol" <| fun _ ->
        Expect.equal (Units.scale Percent 0.1246) (12.46, "%")
                     "Percents are scaled properly"

      testCase "'Scaled' unit with a 1. scale is not actually scaled" <| fun _ ->
        let actual = Units.scale (Scaled (Percent, 1.)) 0.123
        let expected = 12.3, "%"
        Expect.equal actual expected "Should handle non-scaled Scaled"

      testCase "'Scaled' unit by 1/10" <| fun _ ->
        let actual = Units.scale (Scaled (Percent, 0.1)) 0.123
        // if 12.3% is had been scaled by 0.1, then the true value is this:
        let expected = 123., "%"
        Expect.equal actual expected "Should handle 0.1x scale"

      testList "SI thousands-units multiples" [
        yield!
          [ 1., 1., ""
            10., 10., ""
            100., 100., ""
            1000., 1., "k"
            1024., 1.024, "k"
            2345., 2.345, "k"
            1000000., 1., "M"
            1234000333., 1.234000333, "G"
            1234000333444., 1.234000333444, "T"
            1234000333444555., 1.234000333444555, "P"
            1234000333444555666., 1.234000333444555666, "E"
            1234000333444555666777., 1.234000333444555666777, "Z"
          ]
          |> List.collect (fun (value, expectedf, prefix) ->
          [ Metres; Amperes; Kelvins; Moles; Candelas; Watts; Hertz ] |> List.map (fun units ->
          testCase (sprintf "scaling %f %A" value units) (fun _ -> 
            let actualf, actualu = Units.scale units value
            let expectedu = sprintf "%s%s" prefix (Units.symbol units)
            Expect.equal actualu expectedu "Should properly format the unit"
            Expect.floatEqual actualf expectedf None "Should properly scale the value to the unit"
          )))
      ]

    ]
  ]
