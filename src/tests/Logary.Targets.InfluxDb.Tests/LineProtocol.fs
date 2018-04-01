module Logary.Targets.InfluxDb.Tests.LineProtocol

open Expecto
open Expecto.Flip
open System
open System.IO
open Hopac
open Logary
open Logary.Tests
open Logary.Targets.InfluxDb

type MyObj = {foo: string; number: int}

// Structure:
// https://docs.influxdata.com/influxdb/v1.5/write_protocols/line_protocol_reference/
// <measurement>[,<tag_key>=<tag_value>[,<tag_key>=<tag_value>]] <field_key>=<field_value>[,<field_key>=<field_value>] [<timestamp>]

// Indexing in Influx:
// - InfluxDb tags: indexed
// - InfluxDb fields: not indexed

// Logary -> Influx
//  - context -> tag
//  - tag -> tag
//  - Gauge -> field
//  - field -> field

[<Tests>]
let lineProtocol =
  testList "syntax for line protocol" [
    testList "gauges" [
      testCase "Serialise timestamp" <| fun _ ->
        Serialisation.serialiseTimestamp 1439587925L
          |> Expect.equal
              "Should be identical to Logary"
              "1439587925"

      testCase "for example 1" <| fun _ ->
        Message.gauge PointName.empty "measurement" (Int64 12L)
          |> Message.setNanoEpoch 1439587925L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should serialise correctly"
              "measurement value=12 1439587925"

      testCase "For example" <| fun _ ->
        Message.gauges (PointName.parse "measurement") [
          "value", Float 12.
          "otherVal", Float 21.
        ]
          |> Message.setContext "foo" "bar"
          |> Message.setContext "bat" "baz"
          // Timestamps must be in Unix time and are assumed to be in nanoseconds
          |> Message.setNanoEpoch 1439587925L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should serialise properly"
              "measurement,bat=baz,foo=bar otherVal=21,value=12 1439587925"

      testCase "Simplest Valid Point (measurement + field + ts)" <| fun _ ->
        // sensor name is empty, measurement name is disk_free
        Message.gaugei "" "disk_free" 442221834240L
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Equals the right value"
              "disk_free value=442221834240i 1435362189575692182"

      testCase "With Tags + ts" <| fun _ ->
        Message.gaugei "" "disk_free" 442221834240L
          |> Message.setContext "hostname" "server01"
          |> Message.setContext "disk_type" "SSD"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Serialises as intended"
              "disk_free,disk_type=SSD,hostname=server01 value=442221834240i 1435362189575692182"

      testCase "Multiple Fields" <| fun _ ->
        let harddrive = PointName.parse "/dev/sda"
        Message.gaugeWithUnit harddrive "free_space" (Gauge (Int64 442221834240L, Bytes))
          |> Message.setField "disk_type" "SSD"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              "/dev/sda disk_type=\"SSD\",free_space=442221834240i 1435362189575692182"

      testCase "Escaping Commas and Spaces" <| fun _ ->
        Message.gauge PointName.empty "total disk free" (Int64 442221834240L)
          |> Message.setContext "volumes in,computer" "/net,/home,/"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"total\ disk\ free,volumes\ in\,computer=/net\,/home\,/ value=442221834240i 1435362189575692182"

      testCase "Escaping Equals Signs" <| fun _ ->
        Message.gauge PointName.empty "disk_free" (Int64 442221834240L)
          |> Message.setContext "a=b" "x=z"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"disk_free,a\=b=x\=z value=442221834240i 1435362189575692182"

      testCase "With Backslash in Tag Value" <| fun _ ->
        Message.gauge PointName.empty "disk_free" (Int64 442221834240L)
          |> Message.setContext "path" @"C:\Windows"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"disk_free,path=C:\Windows value=442221834240i 1435362189575692182"

      testCase "Escaping Field Key" <| fun _ ->
        Message.gauge PointName.empty "disk_free" (Int64 442221834240L)
          |> Message.setContext "working directories" @"C:\My Documents\Stuff for examples,C:\My Documents"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"disk_free value=442221834240i,working\ directories=""C:\My Documents\Stuff for examples,C:\My Documents"" 1435362189575692182"

      testCase "Showing all escaping and quoting behavior" <| fun _ ->
        let sensor = PointName.empty
        let datapoint = "\"measurement with quotes\""
        Message.gauge sensor datapoint (Int64 1L)
          |> Message.setField @"field_key\\\\" "string field value, only \" need be quoted"
          |> Message.setContext "tag key with spaces" "tag,value,with\"commas\""
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"""measurement\ with\ quotes"",tag\ key\ with\ spaces=tag\,value\,with""commas"" field_key\\\\=""string field value, only \"" need be quoted"",value=1i 1435362189575692182"

    ]

    testList "with tagging" [
      let msg =
        let pn = PointName [| "Processor"; "% Utilisation" |]
        Message.gaugeWithUnit pn "Total" (Gauge (Float 1., Percent))
        |> Message.setContext "host" "my-host-001"
        |> Message.addGauges [
            "Core 1", Gauge (Float 0.03, Percent)
            "Core 2", Gauge (Float 0.06, Percent)
            "Core 3", Gauge (Float 0.139, Percent)
        ]
        |> Message.setNanoEpoch 1435362189575692182L

      yield testCase "when the fields are the values" <| fun _ ->
        Serialisation.serialiseMessage msg
          |> Expect.equal
              "Should have both the fields as values, and the default value"
              @"Processor.%\ Utilisation,unit=%,host=my-host-001 Core\ 1=0.03,Core\ 2=0.06,Core\ 3=0.139,value=1 1435362189575692182"

      yield testCase "with suppress point value" <| fun _ ->
        Serialisation.serialiseMessage msg
          |> Expect.equal
              "Should have both the fields as values, and the default value"
              @"Processor.%\ Utilisation,unit=%,host=my-host-001 Core\ 1=0.03,Core\ 2=0.06,Core\ 3=0.139 1435362189575692182"
    ]

    testList "events" [
      testCase "Simple event template gets logged as field" <| fun _ ->
        Message.eventInfo "Hej {name}"
          |> Message.setName (PointName.ofSingle "Corp.Svc.Host.Sample")
          |> Message.setField "name" "haf"
          |> Message.setContext "service" "Corp Svc"
          |> Message.setContext "service_version" "v2.0.2-e43562"
          |> Message.setContext "host" "www-002.example.com"
          |> Message.setContext "dc" "ams3"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"event_info,dc=ams3,host=www-002.example.com,service=Corp\ Svc,service_version=v2.0.2-e43562 pointName=""Corp.Svc.Host.Sample"",name=""haf"",event=""Hej {name}"",value=1i 1435362189575692182"

      testCase "Simple event fields gets logged as fields" <| fun _ ->
        Message.eventInfo "A template with {Field1} interpolated"
          |> Message.setField "Field1" "value1"
          |> Message.setField "Field2" 2L
          |> Message.setNameStr "my_sensor"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"event_info pointName=""my_sensor"",Field1=""value1"",Field2=2i,event=""A template with {Field1} interpolated"",value=1i 1435362189575692182"

      testCase "Simple event complex field gets logged as fields" <| fun _ ->
        let obj = {foo = "bar"; number = 1}
        Message.eventInfo "Template"
          |> Message.setFieldsFromObject obj
          |> Message.setNameStr "Measurement"
          |> Message.setNanoEpoch 1435362189575692182L
          |> Serialisation.serialiseMessage
          |> Expect.equal
              "Should equal"
              @"event_info pointName=""Measurement"",foo=""bar"",number=1i,event=""Template"",value=1i 1435362189575692182"
    ]
  ]
