module Logary.Tests.Constants

open Expecto
open Logary

[<Tests>]
let tests =
  testList "constants" [
    testList "floats" (
      [ Constants.SecondsPerTick, 0.0000001, "SecondsPerTick"
        Constants.MillisPerTick, 0.0001, "MillisPerTick"
        Constants.MicrosPerTick, 0.1, "MicrosPerTick"
      ]
      |> List.map (fun (actual, expected, name) ->
          testCase (sprintf "ensuring constant '%s'" name) <| fun () ->
            Expect.equal actual expected "Constant should not change"
      )
    )

    testList "int64s" (
      [ Constants.NanosPerTick, 100L, "NanosPerTick"
        Constants.NanosPerMicro, 1000L, "NanosPerMicro"
        Constants.NanosPerMilli, 1000000L, "NanosPerMilli"
        Constants.NanosPerSecond, 1000000000L, "NanosPerSecond"
        Constants.NanosPerMinute, 60000000000L, "NanosPerMinute"
        Constants.TicksPerMinute, 600000000L, "TicksPerMinute"
        Constants.TicksPerSecond, 10000000L, "TicksPerSecond"
        Constants.TicksPerMilli, 10000L, "TicksPerMilli"
        Constants.TicksPerMicro, 10L, "TicksPerMicro"
      ]
      |> List.map (fun (actual, expected, name) ->
          testCase (sprintf "ensuring constant '%s'" name) <| fun () ->
            Expect.equal actual expected "Constant should not change"
      )
    )
  ] |> testLabel "logary"
