module Logary.Tests.Gauge

open Expecto
open Expecto.Flip
open Logary

[<Tests>]
let tests =
  testList "gauge" [
    testList "to string" [
      testCase "days" <| fun () ->
        Gauge(Value.Float 2.34, U.Days).ToString()
          |> Expect.equal "Should format # days properly" "2.34 days"

      testCase "micro seconds" <| fun () ->
        Gauge(Value.Float 2.34, U.Scaled(U.Seconds, 1_000_000.)).ToString()
          |> Expect.equal "Should format # days properly" "2.34 µs"
    ]

    testCase "time" <| fun _ ->
      let timeFun = Gauge.time id
      let g, res = timeFun 100

      res |> Expect.equal "Has right result" 100

      match g.unit with
      | U.Scaled (U.Seconds, _) ->
        ()
      | other ->
        failtestf "Expected U.Scaled (U.Seconds, _), but got %A" other

    testCase "timeExec" <| fun _ ->
      let g, res = Gauge.timeExec (fun () -> 100)

      res |> Expect.equal "Has right result" 100

      match g.unit with
      | U.Scaled (U.Seconds, _) ->
        ()
      | other ->
        failtestf "Expected U.Scaled (U.Seconds, _), but got %A" other
  ]
  |> testLabel "logary"