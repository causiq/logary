module Logary.Targets.InfluxDb.Tests.LineProtocol
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
        |> Message.setContext "foo" "bar"
        |> Message.setContextValue "bat" (String "baz")
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
        |> Message.setContext "hostname" "server01"
        |> Message.setContext "disk_type" "SSD"
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

    testCase "Escaping Commas and Spaces" <| fun _ ->
      let msg =
        Message.metric (PointName.ofSingle "total disk free") (Int64 442221834240L)
        |> Message.setContextValue "volumes in,computer" (String "/net,/home,/")
        |> Message.setNanoEpoch 1435362189575692182L

      Assert.equal (Serialisation.serialiseMessage msg)
                   @"total\ disk\ free,volumes\ in\,computer=/net\,/home\,/ value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "Escaping Equals Signs" <| fun _ ->   
      let msg =
        Message.metric (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.setContextValue "a=b" (String "x=z")
        |> Message.setNanoEpoch 1435362189575692182L

      Assert.equal (Serialisation.serialiseMessage msg)
                   @"disk_free,a\=b=x\=z value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "With Backslash in Tag Value" <| fun _ ->
      Tests.skiptest "TODO"

      let msg =
        Message.metric (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.setContextValue "path" (String @"C:\Windows")
        |> Message.setNanoEpoch 1435362189575692182L

      Assert.equal (Serialisation.serialiseMessage msg)
                   @"disk_free,path=C:\Windows value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "Escaping Field Key" <| fun _ ->
      Tests.skiptest "TODO"

      let msg =
        Message.metric (PointName.ofSingle "disk_free") (
          Object ([ "value", Int64 442221834240L
                    "working directories", String @"C:\My Documents\Stuff for examples,C:\My Documents" ]
                  |> Map.ofList))
        |> Message.setNanoEpoch 1435362189575692182L

      Assert.equal (Serialisation.serialiseMessage msg)
                   @"disk_free value=442221834240i,working\ directories=""C:\My Documents\Stuff for examples,C:\My Documents"""
                   "should equal"

    testCase "Showing all escaping and quoting behavior" <| fun _ ->
      Tests.skiptest "TODO"

      // TODO: type out test from https://docs.influxdata.com/influxdb/v0.10/write_protocols/write_syntax/
  ]