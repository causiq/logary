/// A module for decoding arbitrary JSON data into Message values.
module Logary.Json.Decode

open System.Collections.Generic
open NodaTime
open Logary
open Logary.Trace
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Internals.Chiron.JsonTransformer
open Logary.Internals.Chiron.Operators

#nowarn "40"

module D = Json.Decode
module DI = Inference.Json.Decode

let floatMap: JsonDecoder<Map<float, float>> =
  D.mapWithCustomKey
    (Decoder.fromThrowingConverter (fun s -> System.Double.Parse(s, Culture.invariant)))
    D.float

let kind: JsonDecoder<MessageKind> =
  let inner =
    String.toLowerInvariant >> function
    | "control" -> MessageKind.Control
    | "span" -> MessageKind.Span
    | "gauge" -> MessageKind.Gauge
    | "histogram" -> MessageKind.Histogram
    | "setUserProperty" -> MessageKind.SetUserProperty
    | "identifyUser" -> MessageKind.IdentifyUser
    | "event" | _ -> MessageKind.Event
  inner <!> D.string

let resource: JsonDecoder<Model.Resource> =
  Model.Resource.ofMap <!> D.mapWith D.string

/// Decodes Message's `name` **values** (not property-name-property-value pair)
/// from a `Json`.
let pointName: JsonDecoder<PointName> =
  let ofString = PointName.parse <!> D.string
  let ofArray  = PointName.ofArray <!> D.arrayWith D.string
  D.either ofString ofArray

let idDecoder: JsonDecoder<Id> =
  Id.ofBase64String <!> D.string

let idHex: JsonDecoder<Id> =
  Id.ofString <!> D.string

let traceId: JsonDecoder<TraceId> = idDecoder

let traceIdHex: JsonDecoder<TraceId> = idHex

let spanId: JsonDecoder<SpanId> =
  SpanId.ofBase64String <!> D.string

let spanIdHex: JsonDecoder<SpanId> =
  SpanId.ofString <!> D.string

let spanFlags: JsonDecoder<SpanFlags> =
  enum<SpanFlags> <!> D.int

let spanKind: JsonDecoder<SpanKind> =
  SpanKind.ofInt <!> D.int

let spanCode: JsonDecoder<SpanCanonicalCode> =
  enum<SpanCanonicalCode> <!> D.int

let spanStatus: JsonDecoder<SpanStatus> =
  let objectDecoder =
        fun code desc -> code, desc
    <!> D.required spanCode "code"
    <*> D.optional D.string "description"

  let intDecoder =
    spanCode |> Decoder.map (fun c -> c, None)

  D.either intDecoder (D.jsonObjectWith objectDecoder)
let level: JsonDecoder<LogLevel> =
  LogLevel.ofString <!> D.string

let value: JsonDecoder<Value> =
  let decodeObject =
    String.toLowerInvariant >> function
    | "float" ->
      Value.Float <!> D.required D.float "value"
    | "int64" ->
      Value.Int64 <!> D.required D.int64 "value"
    | "bigint" ->
      Value.BigInt <!> D.required D.bigint "value"
    | "fraction" ->
      Value.Fraction <!> D.required (D.tuple2With D.int64 D.int64) "value"
    | "string" ->
      Value.Str <!> D.required D.string "value"
    | "bool" ->
      Value.Bool <!> D.required D.bool "value"
    | other ->
      JsonResult.messageTypeUnknown other |> Decoder.always

  let objectDecoder = D.jsonObject >=> (DI.required "type" |> Decoder.bind decodeObject)
  let floatDecoder = Value.Float <!> D.float
  let bigintDecoder = Value.BigInt <!> D.bigint
  let boolDecoder = Value.Bool <!> D.bool

  D.oneOf [
    objectDecoder
    floatDecoder
    bigintDecoder
    boolDecoder
  ]

let currency: JsonDecoder<Currency> =
  let inner =
    String.toLowerInvariant >> function
      | "usd" -> Currency.USD
      | "eur" -> Currency.EUR
      | other -> Currency.Other other
  inner <!> D.string

let rec units: JsonDecoder<U> =
  let stringDecoder: string -> U =
    String.toLowerInvariant >> function
    | "bits" ->
      U.Bits
    | "bytes" ->
      U.Bytes
    | "seconds" ->
      U.Seconds
    | "metres" ->
      U.Metres
    | "scalar" ->
      U.Scalar
    | "amperes" ->
      U.Amperes
    | "kelvins" ->
      U.Kelvins
    | "moles" ->
      U.Moles
    | "candelas" ->
      U.Candelas
    | "percent" ->
      U.Percent
    | "watts" ->
      U.Watts
    | "hertz" ->
      U.Hertz
    | "joules" ->
      U.Joules
    | "grams" ->
      U.Grams
    | other ->
      U.Other other

  let inner: string -> ObjectReader<U> =
    String.toLowerInvariant >> function
    | "currency" ->
      U.Currency <!> D.required currency "code"
    | "other" ->
      U.Other <!> D.required D.string "unit"
    | "scaled" ->
          fun u f -> U.Scaled (u, f)
      <!> D.required units "unit"
      <*> D.required D.float "value"
    | "offset" ->
          fun u f -> U.Offset (u, f)
      <!> D.required units "unit"
      <*> D.required D.float "value"
    | "mul" ->
          fun aU bU -> U.Mul (aU, bU)
      <!> D.required units "unitA"
      <*> D.required units "unitB"
    | "pow" ->
          fun b p -> U.Pow (b, p)
      <!> D.required units "base"
      <*> D.required D.float "power"
    | "div" ->
          fun n d -> U.Div (n, d)
      <!> D.required units "numerator"
      <*> D.required units "denominator"
    | "root" ->
      U.Root <!> D.required units "unit"
    | "log10" ->
      U.Log10 <!> D.required units "base"
    | other ->
      JsonResult.messageTypeUnknown other |> Decoder.always

  let stringD =
    stringDecoder <!> D.string

  let objD =
    D.jsonObject >=> (DI.required "type" |> Decoder.bind inner)

  D.either stringD objD

let gauge: JsonDecoder<Gauge> =
  let inner (_: string) =
        fun v u -> Gauge (v, u)
    <!> D.required value "value"
    <*> D.required units "unit"
  D.jsonObject >=> (DI.required "type" |> Decoder.bind inner)

let traceContext: JsonDecoder<IReadOnlyDictionary<string, string>> =
  D.mapWith D.string |> Decoder.map (fun m -> m :> IReadOnlyDictionary<string, string>)

let traceState: JsonDecoder<TraceState> =
  let traceStateKey =
    let inner =
          fun k vendor -> TraceStateKey (k, vendor)
      <!> D.required D.string "key"
      <*> D.optional D.string "vendor"
    D.jsonObjectWith inner
  TraceState.ofList <!> D.listWith (D.tuple2With traceStateKey D.string)

let spanContext: JsonDecoder<SpanContext> =
  let decode =
        fun tc ts s t ps f -> SpanContext(t, s, ps, f, ?traceState=ts, ?traceContext=tc)
    <!> D.optional traceContext "traceContext"
    <*> D.optional traceState "traceState"
    <*> D.required spanId "spanId"
    <*> D.required traceId "traceId"
    <*> D.optional spanId "parentSpanId"
    <*> D.required spanFlags "flags"
  D.jsonObjectWith decode

let spanLink: JsonDecoder<SpanLink> =
  let decode (typ: string) =
    match typ with
    | "followsFromTrace" ->
          fun pre attrs -> FollowsFromTrace (pre, attrs)
      <!> D.required traceId "predecessor"
      <*> D.required (D.mapWith value) "attributes"
    | "followsFromSpan" | _ ->
          fun pre attrs -> FollowsFromSpan (pre, attrs)
      <!> D.required spanContext "predecessor"
      <*> D.required (D.mapWith value) "attributes"
  D.jsonObject >=> (D.required D.string "type" |> Decoder.bind decode)




let moduleInfo: JsonDecoder<ModuleInfo> =
  let inner =
        fun name buildId -> ModuleInfo(name, ?buildId=buildId)
    <!> D.required D.string "name"
    <*> D.optional D.string "buildId"
  D.jsonObjectWith inner

let stackFrame: JsonDecoder<StackFrame> =
  let inner =
        fun site file functionName ofn line col sourceV modu ->
          StackFrame(?site=site, ?file=file, ?functionName=functionName, ?originalFunctionName=ofn, ?lineNo=line,
                     ?colNo=col, ?sourceVersion=sourceV, ?loadModule=modu)
    <!> D.optional D.string "site"
    <*> D.optional D.string "file"
    <*> D.optional D.string "functionName"
    <*> D.optional D.string "originalFunctionName"
    <*> D.optional D.int "lineNo"
    <*> D.optional D.int "colNo"
    <*> D.optional D.string "sourceVersion"
    <*> D.optional moduleInfo "loadModule"
  D.jsonObjectWith inner

let stackTrace: JsonDecoder<StackTrace> =
  let inner =
        fun frames dropped -> StackTrace(frames, dropped)
    <!> D.required (D.arrayWith stackFrame) "frames"
    <*> D.required D.uint16 "droppedFramesCount"
  D.jsonObjectWith inner

let errorInfo: JsonDecoder<ErrorInfo> =
  let stringDecoder: Decoder<string, ErrorInfo> =
    DotNetStacktrace.tryParse
      >> JsonResult.ofOption (InvalidJson "Could not deserialise ErrorInfo from JSON")

  let errorInfoDecoderR, errorInfoDecoder = D.ref ()

  let rec objectReader: ObjectReader<ErrorInfo> =
        fun m et st i -> ErrorInfo(?message=m, ?errorType=et, ?stackTrace=st, ?inner=i)
    <!> D.optional D.string "message"
    <*> D.optional D.string "errorType"
    <*> D.optional stackTrace "stackTrace"
    // Look up how to handle refs/recursive definitions
    <*> D.optional errorInfoDecoder "inner"

  and decoder: JsonDecoder<ErrorInfo> =
    D.jsonObjectWith objectReader

  errorInfoDecoderR := decoder

  D.either (D.string >=> stringDecoder) errorInfoDecoder

let errorInfos = D.arrayWith errorInfo

/// Decodes a timestamp from milliseconds, microseconds or nanoseconds.
let numericTimestamp (minI, maxI): JsonDecoder<EpochNanoSeconds> =
  let rec interpret (prev: int64, n: int64) =
    let i = Instant.ofEpoch n
    if i > maxI || n < prev (* wrap-around case*) then prev // we've gone past max expected, settle for the prev (which might be the initial value!)
    elif i < minI then interpret (n, n * 1000L) // it's less than our minimum, bump it three magnitudes and try again
    else n // we're within out interval; we've found a realistic timestamp
  and loop initial =
    interpret (initial, initial)

  D.int64 |> Decoder.map loop

let stringTimestamp: JsonDecoder<EpochNanoSeconds> =
  D.offsetDateTime |> Decoder.map (fun odt -> odt.asTimestamp)

let foldIntoBase (clock: IClock) (acc: #Model.LogaryMessageBase) (key: string, json: Json): JsonResult<#Model.LogaryMessageBase> =
  match key with
  | "id" ->
    D.either idDecoder idHex json |> JsonResult.map (fun mId -> acc.id <- mId; acc)
  | "parentSpanId" ->
    D.either spanId spanIdHex json |> JsonResult.map (fun spanId -> acc.parentSpanId <- Some spanId; acc)
  | "name" ->
    pointName json |> JsonResult.map (fun name -> acc.name <- name; acc)
  | "level" ->
    level json |> JsonResult.map (fun level -> acc.level <- level; acc)
  | "timestamp" ->
    let now = clock.GetCurrentInstant()
    let minI, maxI = now - Duration.FromDays(365 * 2), now + Duration.FromDays(365 * 2)
    let decoder =
      match json with
      | Json.Number _ ->
        numericTimestamp (minI, maxI)
      | _ ->
        stringTimestamp
    decoder json |> JsonResult.map (fun ts -> acc.timestamp <- ts; acc)
  | "context" ->
    D.mapWith value json |> JsonResult.map (fun c -> acc.setContextValues c; acc)
  | "fields" ->
    D.mapWith value json |> JsonResult.map (fun fs -> acc.setFieldValues fs; acc)
  | "gauges" ->
    D.mapWith gauge json |> JsonResult.map (fun gs -> acc.setGaugeValues gs; acc)
  | _ ->
    JsonResult.pass acc


let internal logaryMessageWith clock ctorDecoder ctorFactory =
  let folder (m: _) = foldIntoBase clock m

  fun jObj ->
    ctorDecoder jObj
      |> JsonResult.map ctorFactory
      |> JsonResult.bind (fun m -> JsonResult.foldBind folder m (JsonObject.toPropertyList jObj))


let eventMessageReader clock: ObjectReader<Model.Event> =
  let ctorDecoder =
        fun e m -> e, m
    <!> D.required D.string "event"
    <*> D.optional gauge "monetaryValue"

  logaryMessageWith clock ctorDecoder (fun (event, monetaryValue) -> Model.Event(event, monetaryValue))

let eventMessage (clock: IClock): JsonDecoder<Model.Event> =
  D.jsonObjectWith (eventMessageReader clock)

let eventMessageInterface clock: JsonDecoder<Logary.EventMessage> =
  eventMessage clock |> Decoder.map (fun m -> m :> Logary.EventMessage)


let gaugeMessageReader clock: ObjectReader<Model.GaugeMessage> =
  let ctorDecoder =
        fun g labels -> g, labels
    <!> D.required gauge "gauge"
    <*> D.required (D.mapWith D.string) "labels"

  logaryMessageWith clock ctorDecoder (fun (g, labels) -> Model.GaugeMessage(g, labels))

let gaugeMessage (clock: IClock): JsonDecoder<Model.GaugeMessage> =
  D.jsonObjectWith (gaugeMessageReader clock)


let histogramMessageReader clock: ObjectReader<Model.HistogramMessage> =
  let ctorDecoder =
        fun labels buckets sum -> labels, buckets, sum
    <!> D.required (D.mapWith D.string) "labels"
    <*> D.required floatMap "buckets"
    <*> D.required D.float "sum"

  logaryMessageWith clock ctorDecoder (fun (labels, buckets, sum) -> Model.HistogramMessage(labels, buckets, sum))

let histogramMessage (clock: IClock): JsonDecoder<Model.HistogramMessage> =
  D.jsonObjectWith (histogramMessageReader clock)


let foldSpan (clock: IClock) (m: Model.SpanMessage) (key: string, json: Json): JsonResult<Model.SpanMessage> =
  match key with
  | "label" ->
    D.string json |> JsonResult.map (fun label -> m.label <- label; m)
  | "traceId" ->
    traceId json |> JsonResult.map (fun traceId -> m.context <- m.context.withTraceId traceId; m)
  | "spanId" ->
    spanId json |> JsonResult.map (fun spanId -> m.context <- m.context.withSpanId spanId; m)
  | "parentSpanId" ->
    spanId json |> JsonResult.map (fun parentSpanId -> m.context <- m.context.withParentId (Some parentSpanId); m)
  | "traceState" ->
    JsonResult.pass m // TODO: W3C parser on: D.string |> JsonResult.bind (parse W3C.traceState >> JsonResult.ofParseResult)
  | "traceContext" ->
    traceContext json |> JsonResult.map (fun tc -> m.context <- m.context.withContext tc; m)
  | "kind" ->
    spanKind json |> JsonResult.map (fun kind -> m.spanKind <- kind; m)
  | "started" ->
    let now = clock.GetCurrentInstant()
    let minI, maxI = now - Duration.FromDays(365 * 2), now + Duration.FromDays(365 * 2)
    numericTimestamp (minI, maxI) json |> JsonResult.map (fun started -> m.started <- started; m)
  | "finished" ->
    let now = clock.GetCurrentInstant()
    let minI, maxI = now - Duration.FromDays(365 * 2), now + Duration.FromDays(365 * 2)
    numericTimestamp (minI, maxI) json |> JsonResult.map (fun finished -> m.finished <- Some finished; m)
  | "flags" ->
    spanFlags json |> JsonResult.map (fun flags -> m.flags <- flags; m)
  | "links" ->
    D.arrayWith spanLink json |> JsonResult.map (fun links -> m.setLinks links; m)
  | "events" ->
    D.arrayWith (eventMessageInterface clock) json |> JsonResult.map (fun es -> m.setEvents es; m)
  | "attrs" ->
    D.mapWith value json |> JsonResult.map (fun attrs -> m.setAttributes attrs; m)
  | "status" ->
    spanStatus json |> JsonResult.map (fun s -> m.status <- s; m)
  | _ ->
    JsonResult.pass m


let spanMessageReader clock: ObjectReader<Model.SpanMessage> =
  let folder m = foldSpan clock m
  fun jObj ->
    let m = Model.SpanMessage()
    jObj
      |> JsonObject.toPropertyList
      |> JsonResult.foldBind folder m

let spanMessage (clock: IClock): JsonDecoder<Model.SpanMessage> =
  D.jsonObjectWith (spanMessageReader clock)


let identifyUserMessageReader clock: ObjectReader<Model.IdentifyUserMessage> =
  let ctorDecoder =
        fun prevUserId newUserId -> prevUserId, newUserId
    <!> D.required D.string "prevUserId"
    <*> D.required D.string "newUserId"

  logaryMessageWith clock ctorDecoder (fun (prevUserId, newUserId) -> Model.IdentifyUserMessage(prevUserId, newUserId))


let identifyUserMessage (clock: IClock): JsonDecoder<Model.IdentifyUserMessage> =
  D.jsonObjectWith (identifyUserMessageReader clock)


let setUserPropertyMessageReader clock: ObjectReader<Model.SetUserPropertyMessage> =
  let ctorDecoder =
        fun key value -> key, value
    <!> D.required D.string "key"
    <*> D.required value "value"

  logaryMessageWith clock ctorDecoder (fun (k, v) -> Model.SetUserPropertyMessage(k, v))

let setUserPropertyMessage clock: JsonDecoder<Model.SetUserPropertyMessage> =
  D.jsonObjectWith (setUserPropertyMessageReader clock)


// At this level we use the Global IClock (Global.clockD); otherwise just use the JsonDecoders above

let internal toBase decoder = decoder |> Decoder.map (fun (m: 'input) -> m :> Model.LogaryMessageBase)

let logaryMessageReader: ObjectReader<Model.LogaryMessageBase> =
  let parseByType (typ: MessageKind): ObjectReader<Model.LogaryMessageBase> =
    let reader =
      match typ with
      | MessageKind.Event           -> eventMessageReader >> toBase
      | MessageKind.Gauge           -> gaugeMessageReader >> toBase
      | MessageKind.Histogram       -> histogramMessageReader >> toBase
      | MessageKind.Span            -> spanMessageReader >> toBase
      | MessageKind.IdentifyUser    -> identifyUserMessageReader >> toBase
      | MessageKind.SetUserProperty -> setUserPropertyMessageReader >> toBase
      | MessageKind.Control         ->
        fun _ _ -> JsonResult.messageTypeUnknown "ControlMessage are not accepted for deserialisation"

    Global.clockD |> DVar.mapFun reader

  D.required kind "type" |> Decoder.bind parseByType

let logaryMessage: JsonDecoder<Model.LogaryMessageBase> =
  D.jsonObject >=> logaryMessageReader

let logaryMessageArray: JsonDecoder<Model.LogaryMessageBase[]> =
  D.arrayWith logaryMessage

let messageBatch: JsonDecoder<Model.LogaryMessageBase[]> =
  D.either (logaryMessage |> Decoder.map Array.singleton)
           logaryMessageArray
