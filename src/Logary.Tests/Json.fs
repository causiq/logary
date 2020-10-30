module Logary.Tests.Json

open System
open System.Text
open Expecto
open Expecto.Flip
open Expecto.Logging
open Expecto.Logging.Message
open Logary.Model
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema
open NodaTime
open NodaTime.Text

let logger = Log.create "Logary.Tests.Json"

open Logary
open Logary.Trace
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Json
module E = Encode
module D = Decode


let testEncode suffix (encoder: JsonEncoder<'a>) =
  let testName = sprintf "encode %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  testPropertyWithConfig fsc testName <| fun (value: 'a) ->
  encoder value |> ignore

let ftestEncode suffix (encoder: JsonEncoder<'a>) =
  let testName = sprintf "encode %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  ftestPropertyWithConfig fsc testName <| fun (value: 'a) ->
  encoder value |> ignore

let testDecode suffix (decoder: JsonDecoder<'a>) sampleJSON =
  let testName = sprintf "decode %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  testCase testName <| fun () ->
    decoder (sampleJSON ())
      |> JsonResult.getOrThrow
      |> ignore

let ftestDecode suffix (decoder: JsonDecoder<'a>) sampleJSON =
  let testName = sprintf "decode %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  ftestCase testName <| fun () ->
    decoder (sampleJSON ())
      |> JsonResult.getOrThrow
      |> ignore

let buildRoundtrip (encoder: JsonEncoder<'a>, decoder: JsonDecoder<'a>) =
  fun (value: 'a) ->
    let mutable s = "before encode", sprintf "Original CLR value: %A" value
    try
      let encoded = encoder value
      s <- "before decode", sprintf "  Original CLR value: %A\nEncoded value: %A" value encoded
//      printfn "2 => %s" (encoded |> Json.formatWith JsonFormattingOptions.SingleLine)
      let decoded = decoder encoded
      s <- "before JsonResult.getOrThrow", sprintf "  Original CLR value: %A\nEncoded value: %A\nDecoded value: %A" value encoded decoded
      let result = JsonResult.getOrThrow decoded
      ignore result
    with e ->
      let step, message = s
      failtestf "Failed to roundtrip '%s' at step %s.%s\n  Got error:\n%O" typeof<'a>.FullName step message e

let testRoundtrip suffix (encoder: JsonEncoder<'a>, decoder: JsonDecoder<'a>) =
  let testName = sprintf "roundtrip %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  testPropertyWithConfig fsc testName (buildRoundtrip (encoder, decoder))

let ftestRoundtrip suffix (encoder: JsonEncoder<'a>, decoder: JsonDecoder<'a>) =
  let testName = sprintf "roundtrip %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  ftestPropertyWithConfig fsc testName (buildRoundtrip (encoder, decoder))

[<Tests>]
let tests =
  let toEncode, toDecode, toRoundtrip = ResizeArray<_>(), ResizeArray<_>(), ResizeArray<_>()

  let pairWithJSON suffix encode decode sampleJSON =
    toEncode.Add (testEncode suffix encode)
    toRoundtrip.Add (testRoundtrip suffix (encode, decode))
    if Option.isSome sampleJSON then toDecode.Add (testDecode suffix decode sampleJSON.Value)

  //let fpairWithJSON suffix encode decode sampleJSON =
  //  toEncode.Add (ftestEncode suffix encode)
  //  toRoundtrip.Add (ftestRoundtrip suffix (encode, decode))
  //  if Option.isSome sampleJSON then toDecode.Add (ftestDecode suffix decode sampleJSON.Value)

  let pair encode decode =
    pairWithJSON None encode decode None

  let namedPair suffix encode decode =
    pairWithJSON (Some suffix) encode decode None

  // TODO: support control characters in FsParsec; this test hangs .Net Core:
  // pairWithJSON (Some " problematic") E.eventMessage (D.eventMessage clock) None
  namedPair " = SpanId" E.spanId D.spanId
  namedPair " = TraceId" E.traceId D.traceId
  namedPair "Hex" E.idHex D.idHex
  namedPair "Hex" E.spanIdHex D.spanIdHex
  pair E.currency D.currency
  pair E.errorInfo D.errorInfo
  pair E.errorInfos D.errorInfos
  pair E.gauge D.gauge
  pair E.gaugeMessage (D.gaugeMessage MonotonicClock.getTimestamp)
  pair E.histogramMessage (D.histogramMessage MonotonicClock.getTimestamp)
  pair E.idB64 D.idB64
  pair E.identifyUserMessage (D.identifyUserMessage MonotonicClock.getTimestamp)
  pair E.kind D.kind
  pair E.level D.level
  pair E.moduleInfo D.moduleInfo
  pair E.pointName D.pointName
  pair E.resource D.resource
  pair E.setUserPropertyMessage (D.setUserPropertyMessage MonotonicClock.getTimestamp)
  pair E.forgetUserMessage (D.forgetUserMessage MonotonicClock.getTimestamp)
  pair E.spanContext D.spanContext
  pair E.spanKind D.spanKind
  pair E.spanLink D.spanLink
  pair E.stackTrace D.stackTrace
  pair E.units D.units
  pair E.value D.value
  pair E.spanMessage (D.spanMessage MonotonicClock.getTimestamp)

  testList "json" [
    testList "encode" (List.ofSeq toEncode)
    testList "decode" (List.ofSeq toDecode)
    testList "roundtrip" (List.ofSeq toRoundtrip)
    testList "tdd" [
      testPropertyWithConfigStdGen (879933225, 296761504) fsc "roundtrip Logary.Currency" (buildRoundtrip (E.currency, D.currency))
      testRoundtrip None (E.units, D.units)
      testPropertyWithConfig fsc "can generate Model.Event" <| fun (e: Model.Event) -> ignore e

      testDecode None (D.eventMessage MonotonicClock.getTimestamp) (fun () ->
        Json.parse """{"type":"event","event":"Hello world"}""" |> JsonResult.getOrThrow)

      testDecode (Some " simplest") (D.eventMessage MonotonicClock.getTimestamp) (fun () ->
        Json.parse """{"type":"event","event":"Hello world 2", "timestamp":"2010-02-03T04:55:00Z"}""" |> JsonResult.getOrThrow)

      testDecode (Some " w ts") (D.eventMessage MonotonicClock.getTimestamp) (fun () ->
        E.eventMessage (Model.Event("Hello world 3")))

      // TODO: support control characters in FsParsec; this test hangs .Net Core inside F# Core
      //ptestDecode (Some " eventmessage.1.json") (D.eventMessage clock) (fun () -> File.ReadAllText "sample-data/eventmessage.1.json" |> Json.parse |> JsonResult.getOrThrow)

      testCase "decode logary message" <| fun () ->
        let res =
          Json.parse """{"type":"event","event":"Hello world 2", "timestamp":"2010-02-03T04:55:00Z"}"""
            |> JsonResult.bind D.logaryMessage
            |> JsonResult.map (fun m -> m.getAsOrThrow<EventMessage>())
            |> JsonResult.getOrThrow

        res.event
          |> Expect.equal "Has event" "Hello world 2"

      testCase "decode Logary JS traceId" <| fun () ->
        let res =
          Json.parse "\"0a96d6647c56adc2f4b655ecf3fc0fb3\""
            |> JsonResult.bind D.traceId
            |> JsonResult.getOrThrow

        res
          |> Expect.equal "Can decode trace id" (TraceId.ofString "0a96d6647c56adc2f4b655ecf3fc0fb3")

      testCase "decode Logary JS spanContext" <| fun () ->
        let res =
          Json.parse """{ "traceId": "0a96d6647c56adc2f4b655ecf3fc0fb3", "spanId": "5107a84585f23e64", "traceFlags": 1 }"""
            |> JsonResult.bind D.spanContext
            |> JsonResult.getOrThrow

        res.traceId
          |> Expect.equal "Has trace id" (TraceId.ofString "0a96d6647c56adc2f4b655ecf3fc0fb3")

        res.spanId
          |> Expect.equal "Has span id" (SpanId.ofString "5107a84585f23e64")

        res.flags
          |> Expect.equal "Has trace flags" SpanFlags.Sampled

      testCase "decode monetaryValue SEK" <| fun () ->
        """{"amount":1195,"currency":"SEK"}"""
          |> Json.parse
          |> JsonResult.bind D.money
          |> JsonResult.getOrThrow
          |> Expect.equal "Parses to 1195,00 kr" (money Currency.SEK 1195.)

      testCase "decode Logary JS event message" <| fun () ->
        let res =
          Json.parse """[{"event":"Foobar purchased","monetaryValue":{"amount":20,"currency":"EUR"},"error":null,"level":3,"timestamp":"1598451184415000000","fields":{},"context":{},"name":["with-nextjs","IndexPage"],"type":"event","templated":{"message":"Foobar purchased","consumed":[],"remaining":[]},"id":"cvTF3jralDI9861r1uR7Sg=="},{"event":"User clicked \"{cssSelector}\"","monetaryValue":null,"error":null,"level":3,"timestamp":"1598451184417000000","fields":{"cssSelector":"html body div#__next div#layout button#purchase.primary"},"context":{},"name":["with-nextjs","plugins","browser","onClick"],"type":"event","templated":{"message":"User clicked \"html body div#__next div#layout button#purchase.primary\"","consumed":[{"key":"cssSelector","value":"html body div#__next div#layout button#purchase.primary"}],"remaining":[]},"id":"cvTF3jralDI9861r1uR7Sg=="}]"""
            |> JsonResult.bind D.messageBatch
            |> JsonResult.map (fun m -> m |> Array.map (fun mm -> mm.getAsOrThrow<EventMessage>()) |> Array.head)
            |> JsonResult.getOrThrow

        res.event
          |> Expect.equal "Should be 'Foobar purchased'" "Foobar purchased"

        res.monetaryValue.Value
          |> Expect.equal "Is worth 20 EUR" (money Currency.EUR 20.)

        res.name
          |> Expect.equal "Has valid name" (PointName [| "with-nextjs"; "IndexPage" |])

        res.level
          |> Expect.equal "Has Info level" Info

      testCase "decode Logary JS span message" <| fun () ->
        let res =
          Json.parse """[ { "spanContext": { "traceId": "0a96d6647c56adc2f4b655ecf3fc0fb3", "spanId": "5107a84585f23e64", "traceFlags": 1 }, "label": "documentFetch", "level": 3, "kind": 0, "status": { "code": 0 }, "events": [ { "event": "fetchStart", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027284000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "fetchStart", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" }, { "event": "domainLookupStart", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027332000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "domainLookupStart", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" }, { "event": "domainLookupEnd", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027332000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "domainLookupEnd", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" }, { "event": "connectStart", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027333000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "connectStart", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" }, { "event": "connectEnd", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027333000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "connectEnd", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" }, { "event": "requestStart", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027333000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "requestStart", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" }, { "event": "responseStart", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027688000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "responseStart", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" }, { "event": "responseEnd", "monetaryValue": null, "error": null, "level": 3, "timestamp": "1598479027688000000", "fields": {}, "context": {}, "name": [], "type": "event", "templated": { "message": "responseEnd", "consumed": [], "remaining": [] }, "id": "cvTF3jralDI9861r1uR7Sg==" } ], "attrs": { "component": "document-load" }, "context": {}, "started": "1598479027284000000", "type": "span", "finished": "1598479027688000000", "id": "cvTF3jralDI9861r1uR7Sg==" } ]"""
            |> JsonResult.bind D.messageBatch
            |> JsonResult.map (fun m -> m |> Array.map (fun mm -> mm.getAsOrThrow<SpanMessage>()) |> Array.head)
            |> JsonResult.getOrThrow

        res.context.traceId
          |> Expect.equal "Has right trace id" (TraceId.ofString "0a96d6647c56adc2f4b655ecf3fc0fb3")

        res.context.spanId
          |> Expect.equal "Has right span id" (SpanId.ofString "5107a84585f23e64")

        res.context.flags
          |> Expect.equal "Has the sampled flag" SpanFlags.Sampled

        res.label
          |> Expect.equal "Has a 'documentFetch' label" "documentFetch"

        res.level.asInt
          |> Expect.equal "Logged with level 3 (info)" 3

        res.kind
          |> Expect.equal "Internal SpanKind" SpanKind.Internal

        res.events.Count
          |> Expect.equal "Has eight events" 8

      testPropertyWithConfig fsc "roundtrip SpanContext" <| fun (sc: SpanContext) ->
        sc
          |> E.spanContext
          |> D.spanContext
          |> JsonResult.getOrThrow
          |> ignore

      testCase "roundtrip Id to base64" <| fun () ->
        let id = Id.create()
        id.isZero |> Expect.isFalse "Non zero"
        let id' = E.idB64 id |> D.idB64 |> JsonResult.getOrThrow
        id |> Expect.equal "Matches id'" id'


      testCase "pretty print Logary JS span message" <| fun () ->
        let mId = Id.ofBase64String "hUHk5sAImztkm18PlRNRrw=="
        let traceId = TraceId.create()
        let spanId = SpanId.create()
        let parentSpanId = Some (SpanId.create())
        let span =
          SpanMessage(
            "GET /", id, 1234L,
            SpanContext(traceId, spanId, parentSpanId, SpanFlags.Sampled),
            SpanKind.Server,
            ResizeArray<_>(),
            Map.empty,
            ResizeArray<_>(),
            Some (SpanStatus.create(SpanStatusCode.Error, "Cannot do, resource already exists", SpanStatusSource.User)),
            id,
            mId,
            PointName [| "Logary Analytics"; "Prod" |],
            LogLevel.Warn,
            Map.empty |> Map.add "appId" (Value.Str "LA-35710335")
          )

        span.finished <- Some 4321L

        span.context.traceId
          |> Expect.equal "Has passed trace id" traceId

        span.context.spanId
          |> Expect.equal "Has passed span id" spanId

        span.context.parentSpanId
          |> Expect.equal "Has passed parent span id" parentSpanId

        span.context.isSampled
          |> Expect.isTrue "Is sampled"

        (span.elapsed, Duration.Zero)
          |> Expect.isGreaterThan "Zero"

        let json =
          span
            |> E.spanMessage

        let subject =
          json
            |> function Object m -> m | _ -> failwith "Unexpected type"

        let expectStringProp prop expectedString subject =
          subject
            |> Expect.Json.hasFieldX "Has expected property" prop
            |> Expect.Json.isStringX (sprintf "Expected prop %s to be a string" prop)
            |> Expect.equal (sprintf "Expected prop %s to have string equal" prop) expectedString

        let expectIntProp prop expectedInt subject =
          subject
            |> Expect.Json.hasFieldX "Has expected property" prop
            |> Expect.Json.isNumberX (sprintf "Expected prop %s to be a string" prop) int
            |> Expect.equal (sprintf "Expected prop %s to have string equal" prop) expectedInt

        subject
          |> expectStringProp "type" "span"

        subject
          |> expectIntProp "timestamp" 1234

        subject
          |> JsonObject.tryFind "flags"
          |> Expect.isNone "'flags' property is inside 'spanContext' not parent"

        subject
          |> expectStringProp "id" (mId.toBase64String())

        subject
          |> expectStringProp "name" (span.name.ToString())

        subject
          |> expectStringProp "level" "info"

        subject
          |> Expect.Json.hasFieldX "Has context" "context"
          |> Expect.Json.isObjectX "Is object"
          |> Expect.Json.hasFieldX "Has appId" "appId"
          |> Expect.Json.isStringX "appId is a string"
          |> Expect.equal "Has the right value" "LA-35710335"

        subject
          |> expectIntProp "kind" SpanKind.Server.asInt


        // span context
        let spanContext =
          subject
            |> Expect.Json.hasFieldX "Has 'spanContext' property"
                                     "spanContext"
            |> Expect.Json.isObjectX "Is object at this location"

        spanContext
          |> Expect.Json.hasFieldX "Has 'flags' property in spanContext"
                                   "flags"
          |> Expect.Json.isNumberX "flags" int
          |> Expect.equal "'flags' property is inside 'spanContext'" 1

        spanContext
          |> expectStringProp "traceId" (span.context.traceId.toBase64String())

        spanContext
          |> expectStringProp "parentSpanId" (span.context.parentSpanId.Value.toBase64String())

        spanContext
          |> expectStringProp "spanId" (span.context.spanId.toBase64String())


        // roundtrip
        let roundtripped =
          json
            |> D.spanMessage (fun () -> 1234L)
            |> JsonResult.getOrThrow

        roundtripped
          |> Expect.equal "Eq the span as it were before serialising" span

        let roundtrippedJSON =
          roundtripped
            |> E.spanMessage

        let messageDescriptor = sprintf "original:\n%s\n\nrountripped:\n%s" (Json.formatWith JsonFormattingOptions.Pretty json) (Json.formatWith JsonFormattingOptions.Pretty roundtrippedJSON)

        roundtrippedJSON
          |> Expect.equal (sprintf "Back to JSON:\n%s" messageDescriptor) json

        //printfn "subject=%s" (Json.formatWith JsonFormattingOptions.Pretty json)
    ]

    testList "deserialise timestamp" [
      // by using NodaTime, which uses Ticks internally, we'll lose two significant digits
      let now = 1234L
      let getTS () = now

      testCase "zero => now instead" <| fun () ->
        D.timestamp getTS (Number "0")
          |> JsonResult.getOrThrow
          |> Expect.equal "of Number uses now instead" now

      testCase "accepts Number" <| fun () ->
        D.timestamp getTS (Number "1234")
          |> JsonResult.getOrThrow
          |> Expect.equal "of Number" now

      testCase "accepts ISO8601 w second resolution" <| fun () ->
        D.timestamp getTS (String "1970-01-01T00:00:00Z")
          |> JsonResult.getOrThrow
          |> Expect.equal "of Number" 0L

      testCase "accepts ISO8601 w nanosecond resolution, parses to BCL Tick resolution" <| fun () ->
        D.timestamp getTS (String "1970-01-01T00:00:00.000004507Z")
          //                                                ^^ least significant digits parsed by NodaTime
          //                                                  ^^ these are 10x and 1x nanoseconds, skipped by NodaTime
          |> JsonResult.getOrThrow
          |> Expect.equal "of Number" 4500L

      testCase "drift corrects to logary clock" <| fun () ->
        D.numericTimestamp now (1233L, 1233L) (Number "1235")
          |> JsonResult.getOrThrow
          |> Expect.equal "of higher Number" now

        D.numericTimestamp now (1233L, 1233L) (Number "1232")
          |> JsonResult.getOrThrow
          |> Expect.equal "of lower Number" now

      testCase "lower bound accepted" <| fun () ->
        D.numericTimestamp now (1233L, 1235L) (Number "1233")
          |> JsonResult.getOrThrow
          |> Expect.equal "of Number" 1233L

      testCase "upper bound accepted" <| fun () ->
        D.numericTimestamp now (1233L, 1235L) (Number "1235")
          |> JsonResult.getOrThrow
          |> Expect.equal "of Number" 1235L

      testProperty "cannot crash" <| fun (now, minTS, maxTS, received: int64) ->
        D.numericTimestamp now (minTS, maxTS) (Number (received.ToString()))
          |> JsonResult.getOrThrow
          |> ignore

      testCase "deserialise unix epoch in ms" <| fun () ->
        let nowMS = 1602367531000L // ms
        let nowNS = nowMS * 1_000_000L
        let minTS = nowNS - Duration.FromDays(365).ToInt64Nanoseconds()
        let maxTS = nowNS + Duration.FromDays(365).ToInt64Nanoseconds()
        D.numericTimestamp nowNS (minTS, maxTS) (Number (nowMS.ToString()))
          |> JsonResult.getOrThrow
          |> Expect.equal "The current TS" nowNS
    ]

    testList "offset date time" [
      yield testCase "ISO8601" <| fun () ->
        let expected = DateTimeOffset.Parse("2018-08-01T01:23:45Z")
        String "2018-08-01T01:23:45Z"
          |> Json.Decode.dateTimeOffset
          |> JsonResult.getOrThrow
          |> Expect.equal "Parses to the right date time offset" expected

      yield testCase "InstantPattern.Create with nine decimals" <| fun () ->
        let p = InstantPattern.Create("yyyy-MM-dd'T'HH:mm:ss.FFFFFFFFF'Z'", Culture.invariant)
        "2019-02-01T14:45:50.123456789Z"
          |> p.Parse
          |> fun r -> r.Success
          |> Expect.isTrue "Succeeded"

      let baseI = Instant.FromUtc(2019, 02, 01, 14, 45, 50)
      let zuluSamples =
        [ "2019-02-01T14:45:50.123456789Z", 123456789L
          "2019-02-01T14:45:50.12345678Z", 123456780L
          "2019-02-01T14:45:50.1234567Z", 123456700L
          "2019-02-01T14:45:50.123456Z", 123456000L
          "2019-02-01T14:45:50.12345Z", 123450000L
          "2019-02-01T14:45:50.1234Z", 123400000L
          "2019-02-01T14:45:50.123Z", 123000000L
          "2019-02-01T14:45:50.12Z", 120000000L
          "2019-02-01T14:45:50.1Z", 100000000L
          "2019-02-01T14:45:50Z", 0L
        ]
        |> List.map (fun (i, o) -> i, baseI + Duration.FromNanoseconds o)

      let testInstant (i, e: Instant) =
        testCase (sprintf "instant %s parses correctly" i) <| fun () ->
          String i
            |> Json.Decode.instant
            |> JsonResult.getOrThrow
            |> Expect.equal "Can be gotten as a Instant, accurately" e

      let testDTOZulu (i, e: Instant) =
        testCase (sprintf "date time offset %s parses correctly" i) <| fun () ->
          let e = e.ToDateTimeOffset()
          String i
            |> Json.Decode.dateTimeOffset
            |> JsonResult.getOrThrow
            |> Expect.equal "Parses to the right date time offset" e

      yield! zuluSamples |> List.map testInstant
      yield! zuluSamples |> List.map testDTOZulu

      let baseO = baseI.WithOffset(Offset.FromHours 1)
      let offsetSamples =
        [ "2019-02-01T15:45:50.123456789+01:00", 123456789L
          "2019-02-01T15:45:50.123456789+01", 123456789L
          "2019-02-01T15:45:50.12345678+01:00", 123456780L
          "2019-02-01T15:45:50.1234567+01:00", 123456700L
          "2019-02-01T15:45:50.123456+01:00", 123456000L
          "2019-02-01T15:45:50.12345+01:00", 123450000L
          "2019-02-01T15:45:50.1234+01:00", 123400000L
          "2019-02-01T15:45:50.123+01:00", 123000000L
          "2019-02-01T15:45:50.12+01:00", 120000000L
          "2019-02-01T15:45:50.1+01:00", 100000000L
          "2019-02-01T15:45:50+01:00", 0L
          "2019-02-01T15:45:50+01", 0L
        ]
        |> List.map (fun (i, o) -> i, baseO + Duration.FromNanoseconds o)

      let testDTOPlus (i, o: OffsetDateTime) =
        testCase (sprintf "offset date time %s parses correctly" i) <| fun () ->
          String i
            |> Json.Decode.offsetDateTime
            |> JsonResult.getOrThrow
            |> Expect.equal "Parses to the right date time offset" o

      yield! offsetSamples |> List.map testDTOPlus
    ]

    testList "matches JSON schema" [
      let rec printErrorInner (indent: int, sb: StringBuilder) (ve: ValidationError) =
        let em = ve.GetType().GetMethod("GetExtendedMessage", Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Instance).Invoke(ve, [||]) :?> string
        sb.AppendLine(System.String.Format("{0}{1}", System.String(' ', indent), em)) |> ignore
        if ve.ChildErrors.Count > 0 then
          for innerVE in ve.ChildErrors do
            ignore (printErrorInner (indent + 2, sb) innerVE)
        sb

      and printErrors (ve: ValidationError) =
        let sb = printErrorInner (0, StringBuilder()) ve
        sb.ToString()

      // load schema
      let resolver = JSchemaUrlResolver()
      let schema = JSchema.Parse("""{"$ref": "https://app.logary.tech/schemas/logary-message.schema.json"}""", resolver)
      let schemaMerged = JSchema.Parse("""{"$ref": "https://app.logary.tech/schemas/logary-message.merged.schema.json"}""", resolver)

      let jsonEncodingMatchesSchema (m: LogaryMessageBase) =
        let jsonStr = E.logaryMessageBase m |> Json.formatWith JsonFormattingOptions.Pretty
        let jToken = JToken.Parse(jsonStr)

        // validate json
        let mutable errors = ResizeArray<ValidationError>() :> System.Collections.Generic.IList<_>
        let mutable ok = true
        for schema, isMerged in [ schema, false; schemaMerged, true ] do
          if not (jToken.IsValid(schema, &errors)) then
            let errors = errors |> Seq.map printErrors |> String.concat "\n\n"
            logger.info (
              eventX "Failed {mergedOrNot} schema:\n{schema} validation with errors:\n{errors}\nJSON:\n  {json}"
              >> setField "mergedOrNot" (if isMerged then "merged" else "normalised")
              >> setField "errors" errors
              >> setField "schema" schema
              >> setField "json" jsonStr)
            ok <- false
          else
            ok <- ok && true
        ok

      yield testPropertyWithConfig fsc "any LogaryMessage" jsonEncodingMatchesSchema
//      yield etestPropertyWithConfig (39313555, 296810949) fsc "JSON is valid against more than one schema from 'oneOf'"  jsonEncodingMatchesSchema
//      yield etestPropertyWithConfig (471357911, 296803816) fsc "GaugeMessage crashes Chiron" jsonEncodingMatchesSchema
    ]


  ]
  |> testLabel "logary"