module Program

open Fuchu
open System
open System.IO
open Hopac
open Logary
open Logary.Targets.InfluxDb

[<Tests>]
let transformToMessage =
  testList "converting a Logary Message to a InfluxDb Message" [

    testCase "Simplest Valid Point (measurement + field)" <| fun _ ->
      let metricValue =
        Message.metric (PointName.parse "hpc-c-001.windows.cpu % usage") 
                       (Value.Float 0.33)
      Assert.StringContains("should contain value",
        "0.33i",
        msgToString metricValue)

    testCase "With timestamp" <| fun _ ->

      let metricValue =
        Message.metric (PointName.parse "")
                       (Value.Float 0.22)
        |> Message.setNanoEpoch (1435362189575692182L)


      Assert.StringContains("should have a timestamp when included in the message",
        "1435362189575692182",
        msgToString metricValue)
  ]

[<EntryPoint>]
let main argv = Tests.defaultMainThisAssembly argv