namespace Logary.Formatting

#nowarn "40"

open Logary
open Logary.Internals

module internal JsonResult =
  open Logary.Internals.Chiron
  
  let inline mapError (f2b: JsonFailure -> 'a) (aR: JsonResult<'a>): JsonResult<'a> =
    match aR with
    | JPass a -> JPass a
    | JFail x -> JPass (f2b x)
        
  let foldBind (folder: 'state -> 'item -> JsonResult<'state>) (state: 'state) (xsJ: seq<'item>): JsonResult<'state> =
    Seq.fold (fun aJ xJ -> aJ |> JsonResult.bind (fun a -> folder a xJ))
             (JsonResult.pass state)
             xsJ

module Json =
  open Logary.Internals.Chiron
  open Logary.Internals.Chiron.JsonTransformer
  module D = Json.Decode
  module DI = Inference.Json.Decode
  
  /// A module for decoding arbitrary JSON data into Message values.
  module internal Decode =
    open Logary.Internals.Chiron.Operators

    /// Decodes Message's `name` **values** (not property-name-property-value pair)
    /// from a `Json`.
    let pointName: JsonDecoder<PointName> =
      let ofArray  = PointName.ofArray <!> D.arrayWith D.string
      let ofString = PointName.parse <!> D.string
      D.either ofString ofArray

    /// Decodes Message's value from a `Json`.
    let value: JsonDecoder<string> =
      D.string

    /// Decodes a set of string tags (not key-value tags, but atomic tags)
    let tags: JsonDecoder<Set<string>> =
      D.setWith D.string
      
    let rec unit: JsonDecoder<Units> =
      let inner () =
        function
        | "bits" ->
          Decoder.alwaysPass Bits
        | "bytes" ->
          Decoder.alwaysPass Bytes
        | "seconds" ->
          Decoder.alwaysPass Seconds
        | "metres" ->
          Decoder.alwaysPass Metres
        | "scalar" ->
          Decoder.alwaysPass Scalar
        | "amperes" ->
          Decoder.alwaysPass Amperes
        | "kelvins" ->
          Decoder.alwaysPass Kelvins
        | "moles" ->
          Decoder.alwaysPass Moles
        | "candelas" ->
          Decoder.alwaysPass Candelas
        | "percent" ->
          Decoder.alwaysPass Percent
        | "watts" ->
          Decoder.alwaysPass Watts
        | "hertz" ->
          Decoder.alwaysPass Hertz
        | "joules" ->
          Decoder.alwaysPass Joules
        | "grams" ->
          Decoder.alwaysPass Grams
        | "other" ->
          Other <!> D.required D.string "unit"
        | "scaled" ->
              fun u f -> Scaled (u, f)
          <!> D.required unit "unit"
          <*> D.required D.float "value"
        | "offset" ->
              fun u f -> Offset (u, f)
          <!> D.required unit "unit"
          <*> D.required D.float "value"
        | "mul" ->
              fun aU bU -> Mul (aU, bU)
          <!> D.required unit "unitA"
          <*> D.required unit "unitB"
        | "pow" ->
              fun b p -> Pow (b, p)
          <!> D.required unit "base"
          <*> D.required D.float "power"
        | "div" ->
              fun n d -> Div (n, d)
          <!> D.required unit "nom"
          <*> D.required unit "denom"
        | "root" ->
          Root <!> D.required unit "base"
        | "log10" ->
          Log10 <!> D.required unit "base"
        | other ->
          JsonResult.propertyNotFound other |> Decoder.always
          
      D.jsonObject >=> (
        DI.required "type"
        |> Decoder.map String.toLowerInvariant
        |> Decoder.bind (fun t -> inner () t)
      )

    let gaugeValue: JsonDecoder<Value> =
      let inner =
        function
        | "float" ->
          Value.Float <!> D.required D.float "value"
        | "int64" ->
          Value.Int64 <!> D.required D.int64 "value"
        | "bigint" ->
          Value.BigInt <!> D.required D.bigint "value"
        | "fraction" ->
          Value.Fraction <!> D.required (D.tuple2With D.int64 D.int64) "value"
        | other ->
          JsonResult.propertyNotFound other |> Decoder.always

      D.jsonObject >=> (
        DI.required "type"
        |> Decoder.map String.toLowerInvariant
        |> Decoder.bind inner
      ) 

    let gauge: JsonDecoder<Gauge> =
      let inner (_: string) =
            fun v u -> Gauge (v, u)
        <!> D.required gaugeValue "value"
        <*> D.required unit "unit"
      D.jsonObject >=> (DI.required "type" |> Decoder.bind inner)

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
  
    /// Decodes a DotNetStacktrace or an error string.
    let error: JsonDecoder<obj> =
      let parse =
        DotNetStacktrace.parse >> function
          | [||] ->
            JFail (SingleFailure (InvalidJson "'error' property is not a Stacktrace"))
          | trace ->
            JPass trace
      let trace: JsonDecoder<StacktraceLine[]> = D.string >> JsonResult.bind parse
      Json.Decode.either (box <!> trace) (box <!> D.string)
  
    /// Decodes an array of `error`
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
  
    let fields: HashMap<string, obj> -> JsonDecoder<HashMap<string, obj>> =
      let decodeField = function
        | k, vJ when k = "error" ->
          error vJ
        | _, vJ ->
          decodeValue vJ
      _context decodeField (addToFields true)

    let gauges: HashMap<string, obj> -> JsonDecoder<HashMap<string, obj>> =
      let addOne acc _ (k, v) =
        acc |> HashMap.add k (box v)
        
      let decodeGauge (k, vJ) =
        gauge vJ |> JsonResult.map (fun g ->
        KnownLiterals.GaugeNamePrefix + k, g)

      _context decodeGauge addOne
  
    /// Decodes the Message's `level` from a `Json` value.
    let level: JsonDecoder<LogLevel> =
      LogLevel.ofString <!> D.string
  
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
    let timestampOfMs: JsonDecoder<EpochNanoSeconds> =
      D.int64 |> Decoder.map ((*) 1_000_000L)
  
    let timestampOfMicroS: JsonDecoder<EpochNanoSeconds> =
      D.int64 |> Decoder.map ((*) 1_000L)
  
    let timestampOfNs: JsonDecoder<EpochNanoSeconds> =
      D.int64
  
    let timestampOfDto: JsonDecoder<EpochNanoSeconds> =
      D.dateTimeOffset |> Decoder.map (fun dto -> dto.timestamp)
  
    /// Decodes a timestamp from nanoseconds since epoch or from an RFC3339 date.
    let timestamp: JsonDecoder<EpochNanoSeconds> =
      Json.Decode.either timestampOfNs timestampOfDto
  
    let private foldJsonObject (m: Message, remainder: JsonObject) =
      fun (prop, json) ->
        JsonResult.mapError (fun _ -> m, remainder) <|
        match prop, json with
        | "name", json
        | "source", json
        | "logger", json ->
          pointName json
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
            
        | "gauges", json ->
          gauges m.context json
            |> JsonResult.map (fun context -> { m with context = context }, remainder)
            
        | "tags", json ->
          tags json
            |> JsonResult.map (fun tags -> Message.setContext KnownLiterals.TagsContextName tags m, remainder)
  
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
      fun jsonObj ->
        let initial = Message.event Debug "-"
        //printfn "Called 'message' decoder"
        //JFail (SingleFailure (InvalidJson "Nope nope"))
        JsonObject.toPropertyList jsonObj
        |> JsonResult.foldBind foldJsonObject (initial, JsonObject.empty)
        |> JsonResult.bind (fun (message, remainder) ->
          decodeMap true message.context remainder |> JsonResult.map (fun nextContext ->
          { message with context = nextContext }))

    /// Decodes a Message
    let message: JsonDecoder<Message> =
      D.jsonObject >=> messageObject
      
    /// Decodes an Message[]
    let messageArray: JsonDecoder<Message[]> =
      D.arrayWith message
      
    /// Decodes a Message or an Message[]
    let messageBatch: JsonDecoder<Message[]> =
      D.either (message |> Decoder.map Array.singleton)
               messageArray
    
  let encode (data: 'a): Json =
    JsonHelper.toJson<'a> Global.jsonEncoderRegistry data

  let encodeFormatWith options (data: 'a): string =
    encode data |> Json.formatWith options

  let encodeFormat (data: obj) =
    encodeFormatWith JsonFormattingOptions.Compact data
