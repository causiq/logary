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
                       (Value.Float 0.33m)
      Assert.StringContains("should contain value",
        "0.33i",
        msgToString metricValue)
  ]

[<EntryPoint>]
let main argv = Tests.defaultMainThisAssembly argv