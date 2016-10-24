module Logary.Zipkin.Tests

open Expecto
open System
open System.Net
open Logary.Zipkin.Thrift
open Logary.Zipkin

[<Tests>]
let tests =
  testList "serialisation" [
    testCase "annotation" <| fun _ ->
      let annotation = new Annotation("value", new DateTime(2016, 10, 1), new IPEndPoint(IPAddress.Loopback, 1233))
      let thrifted = ThriftExtensions.ToThrift(annotation)

      Expect.isTrue thrifted.__isset.host "Serialized annotation host not set"
      // System.BitConverter.ToInt32(IPAddress.Loopback.GetAddressBytes(), 0)
      Expect.equal 16777343 thrifted.Host.Ipv4 "IPv4 serialises"
      Expect.equal (uint16 annotation.Endpoint.Port) (uint16 thrifted.Host.Port) "Ports should equal"
      Expect.isTrue thrifted.__isset.value "Serialized annotation value not set"
      Expect.equal annotation.Value thrifted.Value "Value equals"
      Expect.isTrue thrifted.__isset.timestamp "Serialized annotation timestamp not set"
      let epochTicks = (new DateTime(1970, 1, 1)).Ticks
      Expect.equal (int64 (float (annotation.Timestamp.Ticks - epochTicks) / float AnnotationConstants.TicksPerMicosecond))
                   thrifted.Timestamp
                   "Timestamp equal"


    testCase "span" <| fun _ ->
      let annotation = new Annotation("value", DateTime.Now, new IPEndPoint(IPAddress.Any, 2222))
      let traceId = new TraceHeader(123UL, 234UL, Nullable<_> 345UL, true)
      let span = new Span(traceId, new IPEndPoint(IPAddress.Loopback, 1111), "service", "name")
      span.Record(annotation);

      let thrifted = ThriftExtensions.ToThrift(span);
      let host = thrifted.Annotations.[0].Host;

      Expect.equal "service" host.ServiceName "Service name should be 'service'"
      Expect.isTrue thrifted.__isset.name "Serialized span name not set"
      Expect.equal "name" thrifted.Name "Name should be 'name'"
      Expect.isFalse thrifted.__isset.binary_annotations "Serialized span binary annotations should not be set"
      Expect.isTrue thrifted.__isset.trace_id "Serialized span trace_id not set"
      Expect.equal 123L thrifted.TraceId "Trace id equals 123"
      Expect.isTrue thrifted.__isset.id "Serialized span id not set"
      Expect.equal 234L thrifted.Id "Item is should be 234"
      Expect.isTrue thrifted.__isset.parent_id "Serialized span parent_id not set"
      Expect.equal 345L thrifted.ParentId "Parent Id should be 345"
      Expect.isTrue thrifted.Debug "Debug should be true"
  ]

[<EntryPoint>]
let main args =
  Tests.runTestsInAssembly defaultConfig args
