module Logary.Tests.Json_2

open Expecto
//open Expecto.Flip
open Logary
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Json
module E = Encode
module D = Decode

let testEncode suffix (encoder: JsonEncoder<'a>) =
  let testName = sprintf "decode %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  testPropertyWithConfig fsc testName <| fun (value: 'a) ->
  encoder value
    |> ignore

let testDecode suffix (decoder: JsonDecoder<'a>) sampleJSON =
  let testName = sprintf "decode %s%s" typeof<'a>.FullName (suffix |> Option.defaultValue "")
  testCase testName <| fun () ->
    decoder sampleJSON
      |> JsonResult.getOrThrow
      |> ignore

let buildRoundtrip (encoder: JsonEncoder<'a>, decoder: JsonDecoder<'a>) =
  fun (value: 'a) ->
    let mutable s = "before encode", sprintf "Original CLR value: %A" value
    try
      let encoded = encoder value
      s <- "before decode", sprintf "  Original CLR value: %A\nEncoded value: %A" value encoded
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

  let pair encode decode =
    pairWithJSON None encode decode None

  let namedPair suffix encode decode =
    pairWithJSON (Some suffix) encode decode None

  let clock = MonotonicClock.instance

  pair E.currency D.currency
  pair E.errorInfo D.errorInfo
  pair E.errorInfos D.errorInfos
  pair E.eventMessage (D.eventMessage clock)
  pair E.gauge D.gauge
  pair E.gaugeMessage (D.gaugeMessage clock)
  pair E.histogramMessage (D.histogramMessage clock)
  pair E.idEncoder D.idDecoder
  namedPair "Hex" E.idHex D.idHex
  pair E.identifyUserMessage (D.identifyUserMessage clock)
  pair E.kind D.kind
  pair E.level D.level
  pair E.moduleInfo D.moduleInfo
  pair E.pointName D.pointName
  pair E.resource D.resource
  pair E.setUserPropertyMessage (D.setUserPropertyMessage clock)
  pair E.spanContext D.spanContext
  namedPair " = SpanId" E.spanId D.spanId
  namedPair "Hex" E.spanIdHex D.spanIdHex
  pair E.spanKind D.spanKind
  pair E.spanLink D.spanLink
  pair E.stackTrace D.stackTrace
  namedPair " = TraceId" E.traceId D.traceId
  pair E.units D.units
  pair E.value D.value

  testList "json" [
    testList "encode" (List.ofSeq toEncode)
    testList "decode" (List.ofSeq toDecode)
    testList "roundtrip" (List.ofSeq toRoundtrip)
    testList "tdd" [
      testPropertyWithConfigStdGen (879933225, 296761504) fsc "roundtrip Logary.Currency" (buildRoundtrip (E.currency, D.currency))
      testRoundtrip None (E.units, D.units)
    ]
  ]
  |> testLabel "logary"