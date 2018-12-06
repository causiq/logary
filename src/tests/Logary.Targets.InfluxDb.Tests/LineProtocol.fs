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
// - InfluxDb tags: indexed, can only be key-value pairs/strings
// - InfluxDb fields: not indexed

// Logary -> Influx
//  - context -> tag
//  - tag -> tag
//  - Gauge -> field
//  - field -> field

[<Tests>]
let lineProtocol =
  testList "line protocol" [
    testList "serialisation" [
      testCase "timestamp" <| fun _ ->
        Serialise.timestamp 1439587925L
          |> sprintf "%O"
          |> Expect.equal
              "Should be identical to Logary"
              "1439587925"
    ]

    testList "gauges" [
      testCase "gauge = measurement, 1 scalar float gauge" <| fun _ ->
        gauge PointName.empty "measurement" (Float 3.141592654)
          |> setNanoEpoch 1439587925L
          |> Serialise.message Constants.AllowedInfluxTags
          |> Expect.equal
              "Should serialise correctly"
              @"measurement,level=debug,tags=gauge value=3.141592654,value_unit=""units"" 1439587925"

      testCase "gauge = measurement, space, 1 scalar float gauge" <| fun _ ->
        gauge PointName.empty "io ops" (Float 3.141592654)
          |> setNanoEpoch 1439587925L
          |> Serialise.message Constants.AllowedInfluxTags
          |> Expect.equal
              "Should serialise correctly"
              @"io\ ops,level=debug,tags=gauge value=3.141592654,value_unit=""units"" 1439587925"

      testCase "sensor = measurement, 1 scalar int64 gauge" <| fun _ ->
        gaugei (PointName [| "disk_free" |]) "" 442221834240L
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message Constants.AllowedInfluxTags
          |> Expect.equal
              "Equals the right value"
              @"disk_free,level=debug,tags=gauge value=442221834240i,value_unit=""units"" 1435362189575692182"

      testCase "gauge = measurement, 1 bytes int64 gauge, 2 string contexts, 1 uint16 context" <| fun _ ->
        gaugeWithUniti PointName.empty "disk_free" Bytes 442221834240L
          |> setContext "hostname" "server01"
          |> setContext "disk_type" "SSD"
          |> setContext "rack_no" 2us
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message (Constants.AllowedInfluxTags |> Set.union (Set [ "disk_type"; "rack_no" ]))
          |> Expect.equal
              "Serialises as intended"
              @"disk_free,disk_type=SSD,hostname=server01,level=debug,rack_no=2,tags=gauge value=442221834240i,value_f=""411.85 GiB"",value_unit=""bytes"" 1435362189575692182"

      testCase "sensor = measurement, 1 bytes int64 gauge, 1 string contexts, 1 uint16 context, 1 field" <| fun _ ->
        let harddrive = PointName.parse "/dev/sda"
        gaugeWithUniti harddrive "disk_free" Bytes 442221834240L
          |> setContext "hostname" "server01"
          |> setField "disk_type" "SSD"
          |> setContext "rack_no" 2us
          |> setLevel Warn
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message (Constants.AllowedInfluxTags |> Set.add "rack_no")
          |> Expect.equal
              "Serialises as intended"
              @"/dev/sda.disk_free,hostname=server01,level=warn,rack_no=2,tags=gauge disk_type=""SSD"",value=442221834240i,value_f=""411.85 GiB"",value_unit=""bytes"" 1435362189575692182"

      testList "escaping" [
        testCase "commas, spaces" <| fun _ ->
          gauge PointName.empty "total di,sk free" (Int64 442221834240L)
            |> setContext "volumes in,computer" "/net,/ho=me,/"
            |> setNanoEpoch 1435362189575692182L
            |> Serialise.message (Constants.AllowedInfluxTags |> Set.add "volumes in,computer")
            |> Expect.equal
                "Should equal"
                @"total\ di\,sk\ free,level=debug,tags=gauge,volumes\ in\,computer=/net\,/ho\=me\,/ value=442221834240i,value_unit=""units"" 1435362189575692182"

        testCase "equal signs" <| fun _ ->
          gauge PointName.empty "disk_free" (Int64 442221834240L)
            |> setContext "a=b" "x=z"
            |> setNanoEpoch 1435362189575692182L
            |> Serialise.message (Constants.AllowedInfluxTags |> Set.add "a=b")
            |> Expect.equal
                "Should equal"
                @"disk_free,a\=b=x\=z,level=debug,tags=gauge value=442221834240i,value_unit=""units"" 1435362189575692182"

        testCase "backslashes" <| fun _ ->
          gauge PointName.empty "disk_free" (Int64 442221834240L)
            |> setContext "path" @"C:\Windows"
            |> setNanoEpoch 1435362189575692182L
            |> Serialise.message Constants.AllowedInfluxTags
            |> Expect.equal
                "Should equal"
                @"disk_free,level=debug,path=C:\Windows,tags=gauge value=442221834240i,value_unit=""units"" 1435362189575692182"

        testCase "two gauges, field key, gauge as field" <| fun _ ->
          gauges (PointName [| "disk" |])
            [ "/dev/sda free", (Int64 442221834240L)
              "io ops", Int64 200L ]
            |> setField "working directories" @"C:\My Documents\Stuff for examples,C:\My Documents"
            |> setNanoEpoch 1435362189575692182L
            |> Serialise.message Constants.AllowedInfluxTags
            |> Expect.equal
                "Should equal"
                @"disk,level=debug,tags=gauge /dev/sda\ free=442221834240i,/dev/sda\ free_unit=""units"",io\ ops=200i,io\ ops_unit=""units"",working\ directories=""C:\My Documents\Stuff for examples,C:\My Documents"" 1435362189575692182"

        testCase "banana split and cava" <| fun _ ->
          let sensor = PointName.empty
          let datapoint = "\"measurement with quotes\""
          gauge sensor datapoint (Int64 1L)
            |> setField @"field_key\\\\" "string field value, only \" need be quoted"
            |> setContext "tag key with spaces" "tag,value,with\"commas\""
            |> setNanoEpoch 1435362189575692182L
            |> Serialise.message Constants.AllowedInfluxTags
            |> Expect.equal
                "Should equal"
                @"""measurement\ with\ quotes"",level=debug,tags=gauge field_key\\\\=""string field value, only \"" need be quoted"",tag\ key\ with\ spaces=""tag,value,with\""commas\"""",value=1i,value_unit=""units"" 1435362189575692182"
      ]

      testList "advanced" [
        let gauges =
          [ "velocity", Gauge (Float 56.151161131, Div (Metres, Seconds))
            "acceleration", Gauge (Float 0.3, Div (Div (Metres, Seconds), Seconds)) ]

        let alternatives =
          [ "gauge c'tor", gaugesWithUnits (PointName.parse "Car-9B325M") gauges
            "event c'tor", eventDebug "Car-9B325M" |> addGauges gauges ]

        for testN, mbase in alternatives do
          yield testCase (sprintf "%s sensor = measurement, 1 float m s^-1 gauge, 1 float m s^-2 gauge, 2 string contexts" testN) <| fun _ ->
            mbase
              |> setContext "driverId" "haf"
              |> setContext "tenantId" "821"
              |> setNanoEpoch 1435362189575692182L
              |> Serialise.message (Constants.AllowedInfluxTags |> Set.union (Set [ "driverId"; "tenantId" ]))
              |> Expect.equal
                  "Should serialise properly"
                  @"Car-9B325M,driverId=haf,level=debug,tags=gauge,tenantId=821 acceleration=0.3,acceleration_f=""0.30 m/s/s"",velocity=56.151161131,velocity_f=""56.15 m/s"" 1435362189575692182"
      ]
    ]

    testList "tagging" [
      testCase "two tags" <| fun _ ->
        gaugesWithUnits
          (PointName [| "Processor"; "% Utilisation" |])
          [ "Total", Gauge (Float 1., Percent)
            "Core 1", Gauge (Float 0.03, Percent)
            "Core 2", Gauge (Float 0.06, Percent)
            "Core 3", Gauge (Float 0.139, Percent)
          ]
          |> setContext "host" "host-001"
          |> tag "urgent"
          |> tag "w  t"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message Constants.AllowedInfluxTags
          |> Expect.equal
              "Should have both the fields as values, and the default value"
              @"Processor.%\ Utilisation,host=host-001,level=debug,tags=gauge\,urgent\,w\ \ t Core\ 1=0.03,Core\ 1_f=""3.00 %"",Core\ 1_unit=""percent"",Core\ 2=0.06,Core\ 2_f=""6.00 %"",Core\ 2_unit=""percent"",Core\ 3=0.139,Core\ 3_f=""13.90 %"",Core\ 3_unit=""percent"",Total=1,Total_f=""100.00 %"",Total_unit=""percent"" 1435362189575692182"
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
          |> Serialise.message (Constants.AllowedInfluxTags |> Set.add "service_version")
          |> Expect.equal
              "Should equal"
              @"Corp.Svc.Host.Sample,dc=ams3,host=www-002.example.com,level=info,service=Corp\ Svc,service_version=v2.0.2-e43562 event=""Hej {name}"",name=""haf"",value=1i 1435362189575692182"

      testCase "Simple event fields gets logged as fields" <| fun _ ->
        eventInfo "A template with {Field1} interpolated"
          |> setField "Field1" "value1"
          |> setField "Field2" 2L
          |> setNameStr "my_sensor"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message Constants.AllowedInfluxTags
          |> Expect.equal
              "Should equal"
              @"my_sensor,level=info Field1=""value1"",Field2=2i,event=""A template with {Field1} interpolated"",value=1i 1435362189575692182"

      testCase "Simple event complex field gets logged as fields" <| fun _ ->
        let obj = {foo = "bar"; number = 1}
        eventInfo "Template"
          |> setFieldsFromObject obj
          |> setNameStr "Measurement"
          |> setNanoEpoch 1435362189575692182L
          |> Serialise.message Constants.AllowedInfluxTags
          |> Expect.equal
              "Should equal"
              @"Measurement,level=info event=""Template"",foo=""bar"",number=1i,value=1i 1435362189575692182"
    ]
  ]
  |> testLabel "influxdb"