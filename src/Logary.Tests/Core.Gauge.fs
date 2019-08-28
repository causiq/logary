module Logary.Tests.Gauge

open Expecto
open Expecto.Flip
open Logary

let tests = [
  testCase "formatWithUnit" <| fun () ->
    let f = Gauge.format (Gauge (Float 2.34, Units.Days))
    f |> Expect.equal "Should format # days properly" "2.34 days"
]