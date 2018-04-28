namespace Logary.Formatting

open Logary
open Logary.Internals

/// A module for decoding arbitrary JSON data into Message values.
module internal JsonDecode =
  open Logary.Internals.Chiron
  open Logary.Internals.Chiron.Operators
  module D = Json.Decode
  open JsonTransformer
  open Logary

  /// Module with extensions to Chiron
  module JsonResult =
    let inline orElse (a2bR: JsonFailure -> JsonResult<'a>) (aR: JsonResult<'a>): JsonResult<'a> =
      match aR with
      | JPass a -> JsonResult.pass a
      | JFail x -> a2bR x

    let foldBind (folder: 'state -> 'item -> JsonResult<'state>) (state: 'state) (xsJ: seq<'item>): JsonResult<'state> =
      Seq.fold (fun aJ xJ -> aJ |> JsonResult.bind (fun a -> folder a xJ))
               (JsonResult.pass state)
               xsJ

  /// Module with extensions to Chiron
  module Decoder =
    let inline orElse (f2s2aR: JsonFailure -> Decoder<'s,'a>) (s2aR: Decoder<'s,'a>): Decoder<'s,'a> =
      fun s ->
        s2aR s
        |> JsonResult.orElse (fun f -> f2s2aR f s)

  /// Decodes Message's `name` **values** (not property-name-property-value pair)
  /// from a `Json`.
  let name: Decoder<Json, PointName> =
    let ofArray =
      PointName.ofArray <!> D.arrayWith D.string
    let ofString =
      PointName.parse <!> D.string
    ofArray |> Decoder.orElse (fun _ -> ofString)

  /// Decodes Message's value from a `Json`.
  let value: Decoder<Json, string> =
    Optics.get Optics.Json.String_

  let private addField prefix acc key v =
    acc |> HashMap.add ((if prefix then KnownLiterals.FieldsPrefix else "") + key) v

  let private addFieldKVP prefix acc (KeyValue (key, v)) =
    addField prefix acc key v

  /// Decodes a JsonObject into a CLR HashMap<string, obj> value that can be
  /// put into the context of the Message
  let rec private decodeMap topLevel initialState: Decoder<JsonObject, HashMap<string, obj>> =
    let foldIntoMap (acc: HashMap<string, obj>) (key, vJ) =
      decodeValue vJ |> JsonResult.map (addField topLevel acc key)
    JsonObject.toPropertyList
    >> JsonResult.foldBind foldIntoMap initialState

  /// Decodes (boxes) a single JSON value into a CLR value. Mutually recursive
  /// with `decodeMap` since JSON values are recursive themselves.
  and private decodeValue: Decoder<Json, obj> =
    function
    | Json.Object values ->
      // consider tail-recursive alternative
      decodeMap false HashMap.empty values |> JsonResult.map box

    | Json.String value ->
      JsonResult.pass (box value)

    | Json.Array values ->
      let foldIntoarray (vs: obj list) (vJ: Json) =
        decodeValue vJ |> JsonResult.map (fun v -> v :: vs)

      values
      |> JsonResult.foldBind foldIntoarray []
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

  let context: Decoder<Json, HashMap<string, obj>> =
    Optics.get Optics.Json.Object_
    >> JsonResult.bind (decodeMap true HashMap.empty)

  /// Decodes the Message's `level` from a `Json` value.
  let level: Decoder<Json, LogLevel> =
    LogLevel.ofString <!> Optics.get Optics.Json.String_

  /// Decodes the Message timestamp from a `Json` value, assuming that it's a
  /// number and that that number is the number of milliseconds since the Unix
  /// Epoch. This is the most likely representation of the epoch when the
  /// timestamp comes from JS.
  ///
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

  let private foldJsonObject (m: Message, remainder: JsonObject) = function
    | "name", json
    | "source", json
    | "logger", json ->
      name json |> JsonResult.map (fun name -> Message.setName name m, remainder)

    | "message", json
    | "value", json  ->
      value json |> JsonResult.map (fun value -> { m with value = value }, remainder)

    | "fields", json
    | "context", json ->
      context json |> JsonResult.map (fun values ->
      let constructed =
        let name = KnownLiterals.FieldsPrefix + "error"
        match values |> HashMap.tryFind name with
        | Some (:? string as error) ->
          match DotNetStacktrace.parse error with
          | [||] ->
            values
          | st ->
            values |> HashMap.add name (box st)
        | _
        | None ->
          values
      { m with context = constructed |> HashMap.toSeqPair |> Seq.fold (addFieldKVP false) m.context },
      remainder)

    | "level", json
    | "severity", json ->
      level json |> JsonResult.map (fun level -> Message.setLevel level m, remainder)

    | "timestamp", json
    | "@timestamp", json ->
      timestamp json |> JsonResult.map (fun ts -> Message.setNanoEpoch ts m, remainder)

    | otherProp, json ->
      JsonResult.pass (m, remainder |> JsonObject.add otherProp json)

  let message: Decoder<JsonObject, Message> =
    let initial = Message.event Debug "-"
    fun jsonObj ->
      JsonObject.toPropertyList jsonObj
      |> JsonResult.foldBind foldJsonObject (initial, JsonObject.empty)
      |> JsonResult.bind (fun (message, remainder) ->
      decodeMap true message.context remainder |> JsonResult.map (fun nextContext ->
      { message with context = nextContext }))

/// See JsonHelper.fs
module Json =
  open System
  open System.Collections.Concurrent

  open Logary.Internals.Chiron
  open Logary.Internals.Chiron.Operators
  open Inference
  module D = Json.Decode
  module E = Json.Encode

//  let private ec = new ConcurrentDictionary<Type, unit -> obj -> Json>()

  let encode (data: 'a): Json =
    JsonHelper.toJson<'a> Global.jsonEncoderRegistry data

  let formatWith options (data: 'a): string =
    encode data |> Json.formatWith options

  let format (data: obj) =
    formatWith JsonFormattingOptions.Compact data

  let decodeMessage =
    Json.Decode.jsonObjectWith JsonDecode.message