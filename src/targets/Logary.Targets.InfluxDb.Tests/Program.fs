module Program

open Fuchu
open System
open System.IO
open Hopac
open Logary
open Logary.Targets.InfluxDb

[<Tests>]
let lineProtocol =
  testList "syntax for line protocol" [
    // measurement[,tag_key1=tag_value1...] field_key=field_value[,field_key2=field_value2] [timestamp]

    testCase "serialise timestamp" <| fun _ ->
      Assert.equal (Serialisation.serialiseTimestamp 1439587925L)
                   "1439587925"
                   "Should be identical to Logary"

    testCase "for example 1" <| fun _ ->
      let msg =
        Message.metric (PointName.ofSingle "measurement") (Float 12.)
        |> Message.setNanoEpoch 1439587925L
      let subject = Serialisation.serialiseMessage msg
      Assert.equal subject "measurement value=12 1439587925"
                   "should serialise correctly"

    testCase "for example" <| fun _ ->
      let msg =
        Object
          ([ "value", Float 12.
             "otherVal", Float 21. ] |> Map.ofList)
        |> Message.metric (PointName.ofSingle "measurement")
        |> Message.contextValue "foo" (String "bar")
        |> Message.contextValue "bat" (String "baz")
        // Timestamps must be in Unix time and are assumed to be in nanoseconds
        |> Message.setNanoEpoch 1439587925L

      let subject = Serialisation.serialiseMessage msg

      Assert.equal subject
                   "measurement,bat=baz,foo=bar otherVal=21,value=12 1439587925"
                   "should equal"

    testCase "Simplest Valid Point (measurement + field + ts)" <| fun _ ->
      let msg =
        Message.metric (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.setNanoEpoch 1435362189575692182L

      Assert.equal (Serialisation.serialiseMessage msg)
                   "disk_free value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "With Tags + ts" <| fun _ ->
      let msg =
        Message.metric (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.contextValue "hostname" (String "server01")
        |> Message.contextValue "disk_type" (String "SSD")
        |> Message.setNanoEpoch 1435362189575692182L

      Assert.equal (Serialisation.serialiseMessage msg)
                   "disk_free,disk_type=SSD,hostname=server01 value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "Multiple Fields" <| fun _ ->
      let msg =
        Message.metric (PointName.ofSingle "disk_free") (
          Object ([ "free_space", Int64 442221834240L
                    "disk_type", String "SSD" ] |> Map.ofList))
        |> Message.setNanoEpoch 1435362189575692182L

      Assert.equal (Serialisation.serialiseMessage msg)
                   "disk_free disk_type=\"SSD\",free_space=442221834240i 1435362189575692182"
                   "should equal"
  ]

[<EntryPoint>]
let main argv = Tests.defaultMainThisAssembly argv