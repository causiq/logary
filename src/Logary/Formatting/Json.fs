namespace Logary.Formatting

open Logary
open Logary.Internals

module internal JsonDecode =
  open Chiron
  open Chiron.Operators
  module D = Json.Decode
  open JsonTransformer

  module JsonResult =
    let inline orElse (a2bR: JsonFailure -> JsonResult<'a>) (aR: JsonResult<'a>): JsonResult<'a> =
      match aR with
      | JPass a -> JsonResult.pass a
      | JFail x -> a2bR x

    let foldBind (folder: 'state -> 'item -> JsonResult<'state>) (state: 'state) (xsJ: seq<_>): JsonResult<_> =
      Seq.fold (fun aJ xJ -> aJ |> JsonResult.bind (fun a -> folder a xJ))
               (JsonResult.pass state)
               xsJ

  module Decoder =
    let inline orElse (f2s2aR: JsonFailure -> Decoder<'s,'a>) (s2aR: Decoder<'s,'a>): Decoder<'s,'a> =
      fun s ->
        s2aR s
        |> JsonResult.orElse (fun f -> f2s2aR f s)

  let name: Decoder<Json, PointName> =
    let ofArray =
      PointName.ofArray <!> D.arrayWith D.string
    let ofString =
      PointName.parse <!> D.string
    ofArray |> Decoder.orElse (fun _ -> ofString)

  let value: Decoder<Json, string> =
    Optics.get Optics.Json.String_

  let context: Decoder<Json, HashMap<string, obj>> =
    let rec outer (): Decoder<Json, HashMap<string, obj>> =
      function
      | Object values ->
        decodeKvps HashMap.empty values
      | otherwise ->
        let jsonType = JsonMemberType.ofJson otherwise
        JsonResult.fail (SingleFailure (TypeMismatch (JsonMemberType.Object, jsonType)))

    and decodeKvps state: Decoder<JsonObject, _> =
      fun jsonObj ->
        JsonObject.toPropertyList jsonObj |> JsonResult.foldBind (fun (acc: HashMap<string, obj>) ->
            function
            | key, vJ ->
              decodeValues vJ |> JsonResult.map (fun v ->
              acc |> HashMap.add key v)
            )
            state

    and decodeValues: Decoder<Json, obj> =
      function
      | Json.Object values ->
        // consider tail-recursive alternative
        decodeKvps HashMap.empty values |> JsonResult.map box

      | Json.String value ->
        JsonResult.pass (box value)

      | Json.Array values ->
        let folder (vs: obj list) (json: Json) =
          decodeValues json |> JsonResult.map (fun v -> v :: vs)
        values
        |> JsonResult.foldBind folder []
        |> JsonResult.map box

      | Json.Number _ as num ->
        D.float num
        |> JsonResult.map box

      | Json.False ->
        JsonResult.pass (box false)

      | Json.True ->
        JsonResult.pass (box true)

      | Json.Null ->
        JsonResult.pass (box None)

    outer ()

  let level: Decoder<Json, LogLevel> =
    LogLevel.ofString <!> Optics.get Optics.Json.String_

  /// JS/JSON native:
  /// $ 1351700038292387123
  /// => 1351700038292387000 // nanos are truncated
  /// $ Date.now()
  /// => 1351700038292 // ms
  let timestampOfMs: Decoder<Json, EpochNanoSeconds> =
    D.int64 |> Decoder.map ((*) 1_000_000L)

  let timestampOfMicroS: Decoder<Json, EpochNanoSeconds> =
    D.int64 |> Decoder.map ((*) 1_000L)

  let timestampOfNs: Decoder<Json, EpochNanoSeconds> =
    D.int64

  let timestampOfDto: Decoder<Json, EpochNanoSeconds> =
    D.dateTimeOffset |> Decoder.map (fun dto -> dto.timestamp)

  let timestamp: Decoder<Json, EpochNanoSeconds> =
    timestampOfMs |> Decoder.orElse (fun _ -> timestampOfDto)

  let private create name value context level ts =
    { name = name
      value = value
      context = context
      level = level
      timestamp = ts }

  let decodeObject =
     //|> JsonDecode.Decoder.orElse (fun _ -> D.requiredInline JsonDecode.value "message")
     //|> JsonDecode.Decoder.orElse (fun _ -> D.requiredInline JsonDecode.value "template")
         create
     <!> D.requiredInline name "name" // "logger", "source"
     <*> D.requiredInline value "value" // "message", "template"
     <*> D.requiredInline context "context" // "fields", "values in object"
     <*> D.requiredInline level "level" // "severity"
     <*> D.requiredInline timestamp "timestamp" // "time", "instant"

/// See JsonHelper.fs
module Json =
  open Chiron
  open Chiron.Operators
  open Inference
  module D = Json.Decode
  module E = Json.Encode

  let encode (data: obj): Json =
    data |> JsonHelper.toJsonTypeShape Global.jsonEncoderRegistry

  let formatWith options (data: obj): string =
    encode data
    |> Json.formatWith options

  let format (data: obj) =
    formatWith JsonFormattingOptions.Compact data

  let decode =
    Json.Decode.jsonObjectWith JsonDecode.decodeObject