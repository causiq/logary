namespace Logary.Formatting

open Logary
open Logary.Internals

module JsonResult =
  open Logary.Internals.Chiron
  
  let inline mapError (f2b: JsonFailure -> 'a) (aR: JsonResult<'a>): JsonResult<'a> =
    match aR with
    | JPass a -> JPass a
    | JFail x -> JPass (f2b x)

/// A module for decoding arbitrary JSON data into Message values.
module internal JsonDecode =
  open Logary.Internals.Chiron
  open Logary.Internals.Chiron.Operators
  module D = Json.Decode
  open JsonTransformer

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

  let private addToFields prefix acc key v =
    acc |> HashMap.add ((if prefix then KnownLiterals.FieldsPrefix else "") + key) v
    
  let private addFieldKVP prefix acc (KeyValue (key, v)) =
    addToFields prefix acc key v
    
  let private addToContext acc key v =
    acc |> HashMap.add key v


  /// Decodes a JsonObject into a CLR HashMap<string, obj> value that can be
  /// put into the context of the Message
  let rec private decodeMap topLevel initialState: Decoder<JsonObject, HashMap<string, obj>> =
    let foldIntoMap (acc: HashMap<string, obj>) (key, vJ) =
      decodeValue vJ |> JsonResult.map (addToFields topLevel acc key)
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

  let error: JsonDecoder<obj> =
    let parse =
      DotNetStacktrace.parse >> function
        | [||] ->
          printfn "Parse fail"
          JFail (SingleFailure (InvalidJson "'error' property is not a Stacktrace"))
        | trace ->
          printfn "Parse success"
          JPass trace
    let trace: JsonDecoder<StacktraceLine[]> = D.string >> JsonResult.bind parse
    Json.Decode.either (box <!> trace) (box <!> D.string)

  let errors = D.arrayWith error

  let private _context decodeOne addOne: HashMap<string, obj> -> Decoder<Json, HashMap<string, obj>> =
    let foldIntoMap (acc: HashMap<string, obj>) (key, vJ) =
      decodeOne (key, vJ) |> JsonResult.map (addOne acc key)

    fun prevContext ->
      D.jsonObject >=> fun xJO ->
      JsonObject.toPropertyList xJO
      |> JsonResult.foldBind foldIntoMap prevContext

  let context: HashMap<string, obj> -> Decoder<Json, HashMap<string, obj>> =
    _context (fun (key, vJ) -> decodeValue vJ) addToContext

  let fields: HashMap<string, obj> -> Decoder<Json, HashMap<string, obj>> =
    let decodeField = function
      | k, vJ when k = "error" ->
        error vJ
      | _, vJ ->
        decodeValue vJ
    _context decodeField (addToFields true)

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
    Json.Decode.either timestampOfNs timestampOfDto

  let private foldJsonObject (m: Message, remainder: JsonObject) =
    fun (prop, json) ->
      JsonResult.mapError (fun _ -> m, remainder) <|
      match prop, json with
      | "name", json
      | "source", json
      | "logger", json ->
        name json
          |> JsonResult.map (fun name -> Message.setName name m, remainder)

      | "message", json
      | "value", json  ->
        value json
          |> JsonResult.map (fun value -> { m with value = value }, remainder)

      | "_logary.error", json
      | "error", json ->
        error json
          |> JsonResult.map (fun value -> m |> Message.setField "error" value, remainder)
          
      | "fields", json ->
        fields m.context json
          |> JsonResult.map (fun context -> { m with context = context }, remainder)

      | "context", json ->
        context m.context json
          |> JsonResult.map (fun context -> { m with context = context }, remainder)
          
      | "level", json
      | "severity", json ->
        level json
          |> JsonResult.map (fun level -> Message.setLevel level m, remainder)

      | "timestamp", json
      | "@timestamp", json ->
        timestamp json
          |> JsonResult.map (fun ts -> Message.setNanoEpoch ts m, remainder)

      // add extreneous properties to the remainder
      | otherProp, json ->
        JsonResult.pass (m, remainder |> JsonObject.add otherProp json)
        
  let messageObject: ObjectReader<Message> =
    let initial = Message.event Debug "-"
    fun jsonObj ->
      //printfn "Called 'message' decoder"
      //JFail (SingleFailure (InvalidJson "Nope nope"))
      JsonObject.toPropertyList jsonObj
      |> JsonResult.foldBind foldJsonObject (initial, JsonObject.empty)
      |> JsonResult.bind (fun (message, remainder) ->
        decodeMap true message.context remainder |> JsonResult.map (fun nextContext ->
        { message with context = nextContext }))

  let message: JsonDecoder<Message> =
    D.jsonObject >=> messageObject
    

/// See JsonHelper.fs
module Json =
  open Logary.Internals.Chiron
  open Inference
  module D = Json.Decode
  module E = Json.Encode

  let encode (data: 'a): Json =
    JsonHelper.toJson<'a> Global.jsonEncoderRegistry data

  let formatWith options (data: 'a): string =
    encode data |> Json.formatWith options

  let format (data: obj) =
    formatWith JsonFormattingOptions.Compact data

  let decodeMessage: JsonDecoder<Message[]> =
    function
    | Json.Object map ->
      JsonDecode.messageObject map |> JsonResult.map Array.singleton
    | Json.Array _ as json ->
      D.arrayWith JsonDecode.message json
    | other ->
      JFail (SingleFailure (InvalidJson "Value is not a valid Message object"))
