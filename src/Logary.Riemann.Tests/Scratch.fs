module Scratch


open Logary
open Logary.Metrics

open NodaTime

let now = SystemClock.Instance.Now
let ms s = Duration.FromMilliseconds s
let mFromTime instant = mkMeasure "logary.tests.fn1" 1. |> setTimestamp instant
  
let plus t1 t2 = t1 + t2
let minus t1 t2 = t1 - t2

module Sut =
  let tsdelta = function
    | [] | [ _ ] -> failwith "this function is undefined to empty or single item lists"
    | m1 :: m2 :: ms as all ->
      let d = minus (timestamp m2) (timestamp m1)
      m1 |> setInt64 (d.Ticks) |> setUnit Ticks

// tests

open Fuchu

open Sut

[<Tests>]
let scratch =
  testList "use cases" [
    testCase "measuring function times" <| fun _ ->
      tsdelta ([ now; plus now (ms 45L) ] |> List.map mFromTime)
      ()
    testCase "measuring instantaneous value" <| fun _ ->
      ()
    testCase "measuring value change from previous value" <| fun _ ->
      ()
    testCase "requests per second" <| fun _ ->
      ()
    testCase "" <| fun _ ->
      ()




    ]