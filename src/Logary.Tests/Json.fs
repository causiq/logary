module Logary.Tests.Json

//open System.IO
open System
open Expecto
open Expecto.Flip
open Expecto.Logging
open NodaTime
open NodaTime.Text
let logger = Log.create "Logary.Tests.Json"

open Logary
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

  let clock = MonotonicClock.instance

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
  pair E.gaugeMessage (D.gaugeMessage clock)
  pair E.histogramMessage (D.histogramMessage clock)
  pair E.idEncoder D.idDecoder
  pair E.identifyUserMessage (D.identifyUserMessage clock)
  pair E.kind D.kind
  pair E.level D.level
  pair E.moduleInfo D.moduleInfo
  pair E.pointName D.pointName
  pair E.resource D.resource
  pair E.setUserPropertyMessage (D.setUserPropertyMessage clock)
  pair E.spanContext D.spanContext
  pair E.spanKind D.spanKind
  pair E.spanLink D.spanLink
  pair E.stackTrace D.stackTrace
  pair E.units D.units
  pair E.value D.value

  testList "json" [
    testList "encode" (List.ofSeq toEncode)
    testList "decode" (List.ofSeq toDecode)
    testList "roundtrip" (List.ofSeq toRoundtrip)
    testList "tdd" [
      testPropertyWithConfigStdGen (879933225, 296761504) fsc "roundtrip Logary.Currency" (buildRoundtrip (E.currency, D.currency))
      testRoundtrip None (E.units, D.units)
      testPropertyWithConfig fsc "can generate Model.Event" <| fun (e: Model.Event) -> ignore e
      testDecode None (D.eventMessage clock) (fun () -> Json.parse """{"event":"Hello world"}""" |> JsonResult.getOrThrow)
      testDecode (Some " simplest") (D.eventMessage clock) (fun () -> Json.parse """{"event":"Hello world 2", "timestamp":"2010-02-03T04:55:00Z"}""" |> JsonResult.getOrThrow)
      testDecode (Some " w ts") (D.eventMessage clock) (fun () -> E.eventMessage (Model.Event("Hello world 3")))
      // TODO: support control characters in FsParsec; this test hangs .Net Core inside F# Core
      //ptestDecode (Some " eventmessage.1.json") (D.eventMessage clock) (fun () -> File.ReadAllText "sample-data/eventmessage.1.json" |> Json.parse |> JsonResult.getOrThrow)
    ]

    testList "offset date time" [
      yield testCase "ISO8601" <| fun () ->
        let expected = DateTimeOffset.Parse("2018-08-01T01:23:45Z")
        Json.String "2018-08-01T01:23:45Z"
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
          Json.String i
            |> Json.Decode.instant
            |> JsonResult.getOrThrow
            |> Expect.equal "Can be gotten as a Instant, accurately" e

      let testDTOZulu (i, e: Instant) =
        testCase (sprintf "date time offset %s parses correctly" i) <| fun () ->
          let e = e.ToDateTimeOffset()
          Json.String i
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
          Json.String i
            |> Json.Decode.offsetDateTime
            |> JsonResult.getOrThrow
            |> Expect.equal "Parses to the right date time offset" o

      yield! offsetSamples |> List.map testDTOPlus
    ]

  ]
  |> testLabel "logary"