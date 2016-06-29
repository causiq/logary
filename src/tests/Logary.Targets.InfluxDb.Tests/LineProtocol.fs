module Logary.Targets.InfluxDb.Tests.LineProtocol
open Fuchu
open System
open System.IO
open Hopac
open Logary
open Logary.Targets.InfluxDb
open TestHelpers

type myObj = {foo : string; number : int}

[<Tests>]
let lineProtocol =
  testList "syntax for line protocol" [
    // measurement[,tag_key1=tag_value1...] field_key=field_value[,field_key2=field_value2] [timestamp]

    testCase "serialise timestamp" <| fun _ ->
      stringEqual (Serialisation.serialiseTimestamp 1439587925L)
                   "1439587925"
                   "Should be identical to Logary"

    testCase "for example 1" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "measurement") (Float 12.)
        |> Message.setNanoEpoch 1439587925L
      let subject = Serialisation.serialiseMessage msg
      stringEqual subject "measurement value=12 1439587925"
                   "should serialise correctly"

    testCase "for example" <| fun _ ->
      let msg =
        Object
          ([ "value", Float 12.
             "otherVal", Float 21. ] |> Map.ofList)
        |> Message.gauge (PointName.ofSingle "measurement")
        |> Message.setContext "foo" "bar"
        |> Message.setContextValue "bat" (String "baz")
        // Timestamps must be in Unix time and are assumed to be in nanoseconds
        |> Message.setNanoEpoch 1439587925L

      let subject = Serialisation.serialiseMessage msg

      stringEqual subject
                   "measurement,bat=baz,foo=bar otherVal=21,value=12 1439587925"
                   "should equal"

    testCase "Simplest Valid Point (measurement + field + ts)" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   "disk_free value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "With Tags + ts" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.setContext "hostname" "server01"
        |> Message.setContext "disk_type" "SSD"
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   "disk_free,disk_type=SSD,hostname=server01 value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "Multiple Fields" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "disk_free") (
          Object ([ "free_space", Int64 442221834240L
                    "disk_type", String "SSD" ] |> Map.ofList))
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   "disk_free disk_type=\"SSD\",free_space=442221834240i 1435362189575692182"
                   "should equal"

    testCase "Escaping Commas and Spaces" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "total disk free") (Int64 442221834240L)
        |> Message.setContextValue "volumes in,computer" (String "/net,/home,/")
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   @"total\ disk\ free,volumes\ in\,computer=/net\,/home\,/ value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "Escaping Equals Signs" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.setContextValue "a=b" (String "x=z")
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   @"disk_free,a\=b=x\=z value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "With Backslash in Tag Value" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "disk_free") (Int64 442221834240L)
        |> Message.setContextValue "path" (String @"C:\Windows")
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   @"disk_free,path=C:\Windows value=442221834240i 1435362189575692182"
                   "should equal"

    testCase "Escaping Field Key" <| fun _ -> 
      let msg =
        Message.gauge (PointName.ofSingle "disk_free") (
          Object ([ "value", Int64 442221834240L
                    "working directories", String @"C:\My Documents\Stuff for examples,C:\My Documents" ]
                  |> Map.ofList))
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   @"disk_free value=442221834240i,working\ directories=""C:\My Documents\Stuff for examples,C:\My Documents"" 1435362189575692182"
                   "should equal"

    testCase "Showing all escaping and quoting behavior" <| fun _ ->
      let msg =
        Message.gauge (PointName.ofSingle "\"measurement with quotes\"") (
          Object ([ @"field_key\\\\", String "string field value, only \" need be quoted"]
                  |> Map.ofList))
        |> Message.setContextValue "tag key with spaces" (String "tag,value,with\"commas\"")
        |> Message.setContextValue "tag key with spaces" (String "tag,value,with\"commas\"")
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg)
                   @"""measurement\ with\ quotes"",tag\ key\ with\ spaces=tag\,value\,with""commas"" field_key\\\\=""string field value, only \"" need be quoted"" 1435362189575692182"
                   "should equal"
   
    testCase "Simple event template gets logged as tag" <| fun _ ->
      let msg =
        Message.eventInfo "Template"      
        |> Message.setName (PointName.ofSingle "Meassurement")
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg) 
                  @"Meassurement,Event=Template value=1i 1435362189575692182"
                  "should equal"

    testCase "Simple event fields gets logged as fields" <| fun _ ->
      let msg =
        Message.eventInfo "Template"
        |> Message.setField "Field1" "value1"
        |> Message.setField "Field2" 2L
        |> Message.setName (PointName.ofSingle "Meassurement")
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg) 
                  @"Meassurement,Event=Template Field1=""value1"",Field2=2i,value=1i 1435362189575692182"
                  "should equal"

    testCase "Simple event complex field gets logged as fields" <| fun _ ->
     
      let obj = {foo = "bar"; number = 1}
      let msg =
        Message.eventInfo "Template"
        |> Message.setFieldsFromObject obj
        |> Message.setName (PointName.ofSingle "Meassurement")
        |> Message.setNanoEpoch 1435362189575692182L

      stringEqual (Serialisation.serialiseMessage msg) 
                  @"Meassurement,Event=Template foo=""bar"",number=1i,value=1i 1435362189575692182"
                  "should equal"
  ]