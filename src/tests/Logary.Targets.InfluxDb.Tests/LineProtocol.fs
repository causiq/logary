module Logary.Targets.InfluxDb.Tests.LineProtocol

open Expecto
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
    testList "gauges" [
      testCase "Serialise timestamp" <| fun _ ->
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

      testCase "For example" <| fun _ ->
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

    ]

    testList "with tagging" [
      let msg =
        let n = PointName.parse
        let pn = PointName [| "Processor"; "% Utilisation" |]
        Message.gaugeWithUnit pn Percent (Int64 1L)
        |> Message.setContext "host" "my-host-001"
        |> Message.setFieldValue "Core 1" (Field (Float 0.03, Some Percent))
        |> Message.setFieldValue "Core 2" (Field (Float 0.06, Some Percent))
        |> Message.setFieldValue "Core 3" (Field (Float 0.139, Some Percent))
        |> Message.setNanoEpoch 1435362189575692182L

      yield testCase "when the fields are the values" <| fun _ ->
        let actual = Serialisation.serialiseMessage msg
        let expected =
          @"Processor.%\ Utilisation,unit=%,host=my-host-001 Core\ 1=0.03,Core\ 2=0.06,Core\ 3=0.139,value=1i 1435362189575692182"
        Expect.equal actual expected "Should have both the fields as values, and the default value"

      yield testCase "with suppress point value" <| fun _ ->
        let actual =
          msg
          |> Message.tag KnownLiterals.SuppressPointValue
          |> Serialisation.serialiseMessage
        let expected =
          @"Processor.%\ Utilisation,unit=%,host=my-host-001 Core\ 1=0.03,Core\ 2=0.06,Core\ 3=0.139 1435362189575692182"
        Expect.equal actual expected "Should have both the fields as values, and the default value"
    ]

    testList "events" [
      testCase "Simple event template gets logged as field" <| fun _ ->
        let msg =
          Message.eventInfo "Hej {name}"
          |> Message.setName (PointName.ofSingle "Corp.Svc.Host.Sample")
          |> Message.setField "name" "haf"
          |> Message.setContextValue "service" (String "Corp Svc")
          |> Message.setContextValue "service_version" (String "v2.0.2-e43562")
          |> Message.setContextValue "host" (String "www-002.example.com")
          |> Message.setContextValue "dc" (String "ams3")
          |> Message.setNanoEpoch 1435362189575692182L

        stringEqual (Serialisation.serialiseMessage msg) 
                    @"event_info,dc=ams3,host=www-002.example.com,service=Corp\ Svc,service_version=v2.0.2-e43562 pointName=""Corp.Svc.Host.Sample"",name=""haf"",event=""Hej {name}"",value=1i 1435362189575692182"
                    "should equal"

      testCase "Simple event fields gets logged as fields" <| fun _ ->
        let msg =
          Message.eventInfo "A template with {Field1} interpolated"
          |> Message.setField "Field1" "value1"
          |> Message.setField "Field2" 2L
          |> Message.setName (PointName.ofSingle "my_measurement")
          |> Message.setNanoEpoch 1435362189575692182L

        stringEqual (Serialisation.serialiseMessage msg) 
                    @"event_info pointName=""my_measurement"",Field1=""value1"",Field2=2i,event=""A template with {Field1} interpolated"",value=1i 1435362189575692182"
                    "should equal"

      testCase "Simple event complex field gets logged as fields" <| fun _ ->
      
        let obj = {foo = "bar"; number = 1}
        let msg =
          Message.eventInfo "Template"
          |> Message.setFieldsFromObject obj
          |> Message.setName (PointName.ofSingle "Measurement")
          |> Message.setNanoEpoch 1435362189575692182L

        stringEqual (Serialisation.serialiseMessage msg) 
                    @"event_info pointName=""Measurement"",foo=""bar"",number=1i,event=""Template"",value=1i 1435362189575692182"
                    "should equal"
    ]
  ]
