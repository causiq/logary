module Logary.Tests.Units

open Logary
open Fuchu
open ExpectoPatronum

[<Tests>]
let tests =
  testList "units" [
    testList "scaling" [
      testCase "1 s" <| fun _ ->
        let value, units = 1., Seconds
        let scaled = Units.scaleFull units value
        Expect.equal scaled (1., "s") "1 s"
    ]
  ]