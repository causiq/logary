module Logary.Targets.InfluxDb.Tests.LineProtocol

open Expecto
open Expecto.Flip
open System
open System.IO
open Hopac
open Logary
open Logary.Message
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
        Serialise.timestamp 1439587925L
          |> sprintf "%O"
          |> Expect.equal
              "Should be identical to Logary"
              "1439587925"

      testCase "for example 1" <| fun _ ->
        gauge PointName.empty "measurement" (Int64 12L)
          |> setNanoEpoch 1439587925L
          |> Serialise.message
          |> Expect.equal
              "Should serialise correctly"
              "measurement value=12 1439587925"

      testCase "For example" <| fun _ ->
        gauges (PointName.parse "measurement") [
          "value", Float 12.
          "otherVal", Float 21.
        ]
          |> setContext "foo" "bar"
          |> setContext "bat" "baz"
          // Timestamps must be in Unix time and are assumed to be in nanoseconds
          |> setNanoEpoch 1439587925L
          |> Serialise.message
          |> Expect.equal
              "Should serialise properly"
              "measurement,bat=baz,foo=bar otherVal=21,value=12 1439587925"

      testCase "Simplest Valid Point (measurement + field + ts)" <| fun _ ->
        // sensor name is empty, measurement name is disk_free
        gaugei "" "disk_free" 442221834240L
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Equals the right value"
              "disk_free value=442221834240i 1435362189575692182"

      testCase "With Tags + ts" <| fun _ ->
        gaugei "" "disk_free" 442221834240L
          |> setContext "hostname" "server01"
          |> setContext "disk_type" "SSD"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Serialises as intended"
              "disk_free,disk_type=SSD,hostname=server01 value=442221834240i 1435362189575692182"

      testCase "Multiple Fields" <| fun _ ->
        let harddrive = PointName.parse "/dev/sda"
        gaugeWithUnit harddrive "free_space" (Gauge (Int64 442221834240L, Bytes))
          |> setField "disk_type" "SSD"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              "/dev/sda disk_type=\"SSD\",free_space=442221834240i 1435362189575692182"

      testCase "Escaping Commas and Spaces" <| fun _ ->
        gauge PointName.empty "total disk free" (Int64 442221834240L)
          |> setContext "volumes in,computer" "/net,/home,/"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"total\ disk\ free,volumes\ in\,computer=/net\,/home\,/ value=442221834240i 1435362189575692182"

      testCase "Escaping Equals Signs" <| fun _ ->
        gauge PointName.empty "disk_free" (Int64 442221834240L)
          |> setContext "a=b" "x=z"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"disk_free,a\=b=x\=z value=442221834240i 1435362189575692182"

      testCase "With Backslash in Tag Value" <| fun _ ->
        gauge PointName.empty "disk_free" (Int64 442221834240L)
          |> setContext "path" @"C:\Windows"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"disk_free,path=C:\Windows value=442221834240i 1435362189575692182"

      testCase "Escaping Field Key" <| fun _ ->
        gauge PointName.empty "disk_free" (Int64 442221834240L)
          |> setContext "working directories" @"C:\My Documents\Stuff for examples,C:\My Documents"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"disk_free value=442221834240i,working\ directories=""C:\My Documents\Stuff for examples,C:\My Documents"" 1435362189575692182"

      testCase "Showing all escaping and quoting behavior" <| fun _ ->
        let sensor = PointName.empty
        let datapoint = "\"measurement with quotes\""
        gauge sensor datapoint (Int64 1L)
          |> setField @"field_key\\\\" "string field value, only \" need be quoted"
          |> setContext "tag key with spaces" "tag,value,with\"commas\""
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"""measurement\ with\ quotes"",tag\ key\ with\ spaces=tag\,value\,with""commas"" field_key\\\\=""string field value, only \"" need be quoted"",value=1i 1435362189575692182"

    ]

    testList "with tagging" [
      let msg =
        let pn = PointName [| "Processor"; "% Utilisation" |]
        gaugeWithUnit pn "Total" (Gauge (Float 1., Percent))
        |> setContext "host" "my-host-001"
        |> addGauges [
            "Core 1", Gauge (Float 0.03, Percent)
            "Core 2", Gauge (Float 0.06, Percent)
            "Core 3", Gauge (Float 0.139, Percent)
        ]
        |> setNanoEpoch 1435362189575692182L

      yield testCase "when the fields are the values" <| fun _ ->
        Serialise.message msg
          |> Expect.equal
              "Should have both the fields as values, and the default value"
              @"Processor.%\ Utilisation,unit=%,host=my-host-001 Core\ 1=0.03,Core\ 2=0.06,Core\ 3=0.139,value=1 1435362189575692182"

      yield testCase "with suppress point value" <| fun _ ->
        Serialise.message msg
          |> Expect.equal
              "Should have both the fields as values, and the default value"
              @"Processor.%\ Utilisation,unit=%,host=my-host-001 Core\ 1=0.03,Core\ 2=0.06,Core\ 3=0.139 1435362189575692182"
    ]

    testList "events" [
      testCase "Simple event template gets logged as field" <| fun _ ->
        eventInfo "Hej {name}"
          |> setNameStr "Corp.Svc.Host.Sample"
          |> setField "name" "haf"
          |> setContext "service" "Corp Svc"
          |> setContext "service_version" "v2.0.2-e43562"
          |> setContext "host" "www-002.example.com"
          |> setContext "dc" "ams3"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"event_info,dc=ams3,host=www-002.example.com,service=Corp\ Svc,service_version=v2.0.2-e43562 pointName=""Corp.Svc.Host.Sample"",name=""haf"",event=""Hej {name}"",value=1i 1435362189575692182"

      testCase "Simple event fields gets logged as fields" <| fun _ ->
        eventInfo "A template with {Field1} interpolated"
          |> setField "Field1" "value1"
          |> setField "Field2" 2L
          |> setNameStr "my_sensor"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"event_info pointName=""my_sensor"",Field1=""value1"",Field2=2i,event=""A template with {Field1} interpolated"",value=1i 1435362189575692182"

      testCase "Simple event complex field gets logged as fields" <| fun _ ->
        let obj = {foo = "bar"; number = 1}
        eventInfo "Template"
          |> setFieldsFromObject obj
          |> setNameStr "Measurement"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message
          |> Expect.equal
              "Should equal"
              @"event_info pointName=""Measurement"",foo=""bar"",number=1i,event=""Template"",value=1i 1435362189575692182"
    ]
  ]
