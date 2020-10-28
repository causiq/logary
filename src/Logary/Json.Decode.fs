/// A module for decoding arbitrary JSON data into Message values.
module Logary.Json.Decode

open System.Collections.Generic
open System.Text.RegularExpressions
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
  D.eitherNamed ("pointName string", ofString)
                ("pointName array of string", ofArray)

let idB64: JsonDecoder<Id> =
  D.string >=> fun s ->
  match Id.tryOfBase64String s with
  | None ->
    JsonResult.invalidJson (sprintf "'%s' as a base64 string didn't convert to 16 bytes" s)
  | Some res ->
    JsonResult.pass res

let idHex: JsonDecoder<Id> =
  D.string >=> fun s ->
  match Id.tryOfString s with
  | None ->
    JsonResult.invalidJson (sprintf "'%s' wasn't a 16 char long hex string" s)
  | Some res ->
    JsonResult.pass res


let traceIdB64: JsonDecoder<TraceId> = idB64
let traceIdHex: JsonDecoder<TraceId> = idHex

let traceId: JsonDecoder<TraceId> =
  D.eitherNamed ("TraceId b64", traceIdB64)
                ("TraceId hex", traceIdHex)


let spanIdB64: JsonDecoder<SpanId> =
  let inner = SpanId.tryOfBase64String >> JsonResult.ofOption (InvalidJson "SpanId: not Base64")
  D.string >> JsonResult.bind inner

let spanIdHex: JsonDecoder<SpanId> =
  let inner = SpanId.tryOfString >> JsonResult.ofOption (InvalidJson "SpanId: not hexadecimal number")
  D.string >> JsonResult.bind inner

let spanId: JsonDecoder<SpanId> =
  D.eitherNamed ("SpanId b64", spanIdB64)
                ("SpanId hex", spanIdHex)


let spanFlags: JsonDecoder<SpanFlags> =
  enum<SpanFlags> <!> D.int

let spanKind: JsonDecoder<SpanKind> =
  SpanKind.ofInt <!> D.int

let spanCode: JsonDecoder<SpanStatusCode> =
  enum<SpanStatusCode> <!> D.int

let spanSource: JsonDecoder<SpanStatusSource> =
  enum<SpanStatusSource> <!> D.int

let spanStatus: JsonDecoder<SpanStatus> =
  let objectDecoder =
        fun code source desc -> SpanStatus.create(code, ?source=source, ?description=desc)
    <!> D.required spanCode "code"
    <*> D.optional spanSource "source"
    <*> D.optional D.string "description"

  let intDecoder =
    spanCode |> Decoder.map SpanStatus.create

  D.eitherNamed ("spanStatus int", intDecoder)
                ("spanStatus {code,source,description}", D.jsonObjectWith objectDecoder)

let level: JsonDecoder<LogLevel> =
  D.either (LogLevel.ofString <!> D.string)
           (LogLevel.ofInt <!> D.int)


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
  let stringDecoder = Value.Str <!> D.string

  D.oneOf [
    stringDecoder
    floatDecoder
    boolDecoder
    objectDecoder
    bigintDecoder
  ]

let currency: JsonDecoder<Currency> =
  let upper (s: string) = s.ToUpperInvariant()
  let inner =
    upper >> function
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

  D.eitherNamed ("unit string", stringD)
                ("unit object", objD)

let gauge: JsonDecoder<Gauge> =
  let inner (_: string) =
        fun v u -> Gauge (v, u)
    <!> D.required value "value"
    <*> D.required units "unit"
  D.jsonObject >=> (DI.required "type" |> Decoder.bind inner)

let money: JsonDecoder<Money> =
  let case1 =
    let inner =
          fun (a: float) c -> Gauge (Value.Float a, U.Currency c)
      <!> D.required D.float "amount"
      <*> D.required currency "currency"
    D.jsonObject >=> inner

  D.eitherNamed ("money {amount,currency} simple object", case1)
                ("money {type,value,unit} Gauge object", gauge)

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
        fun tc ts s t ps sf tf ->
          let f = sf |> Option.orElse tf |> Option.defaultValue SpanFlags.Sampled // after all, it's here, isn't it?
          SpanContext(t, s, ps, f, ?traceState=ts, ?traceContext=tc)
    <!> D.optional traceContext "traceContext"
    <*> D.optional traceState "traceState"
    <*> D.required spanId "spanId"
    <*> D.required traceId "traceId"
    <*> D.optional spanId "parentSpanId"
    <*> D.optional spanFlags "flags"
    <*> D.optional spanFlags "traceFlags"
  D.jsonObjectWith decode

let spanLink: JsonDecoder<SpanLink> =
  let decode (typ: string) =
    match typ with
    | "followsFromTrace" ->
          fun pre attrs -> FollowsFromTrace (pre, attrs)
      <!> D.required traceId "predecessor"
      <*> D.required (D.mapWith value) "attrs"
    | "followsFromSpan" | _ ->
          fun pre attrs -> FollowsFromSpan (pre, attrs)
      <!> D.required spanContext "predecessor"
      <*> D.required (D.mapWith value) "attrs"
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

  let rec objectReader (): ObjectReader<ErrorInfo> =
        fun m et st i -> ErrorInfo(?message=m, ?errorType=et, ?stackTrace=st, ?inner=i)
    <!> D.optional D.string "message"
    <*> D.optional D.string "errorType"
    <*> D.optional stackTrace "stackTrace"
    // Look up how to handle refs/recursive definitions
    <*> D.optional (D.delay decoder) "inner"

  and decoder (): JsonDecoder<ErrorInfo> =
    D.jsonObjectWith (objectReader ())

  D.eitherNamed ("errorInfo of string", D.string >=> stringDecoder)
                ("errorInfo of {message,errorType,stackTrace,inner} nested JSON", decoder ())

let errorInfos = D.arrayWith errorInfo

/// Decodes a timestamp from milliseconds, microseconds or nanoseconds, as long as it turns
/// out to be between minTS and maxTS; otherwise retuns the value of `now`.
let numericTimestamp (now: EpochNanoSeconds) (minTS, maxTS): JsonDecoder<EpochNanoSeconds> =
  let minTS, maxTS = min minTS maxTS, max minTS maxTS // ensure ranges

  let rec interpret (n: int64, iteration: int64) =
    if n > maxTS then
      now // [minI,maxI] wasn't hit
    elif n < minTS then
      let nextN = n * 1000L
      if nextN <= n then
        // wrap case; let's bail with the `now` value
        now
      else
        // it's less than our minimum, bump it three magnitudes and try again
        interpret (nextN, iteration+1L)
    else
      n // we're within our interval; we've found a realistic timestamp

  and loop x = interpret (x, 1L)

  D.int64 |> Decoder.map loop

let stringTimestamp: JsonDecoder<EpochNanoSeconds> =
  let isoString = D.offsetDateTime |> Decoder.map (fun odt -> odt.asTimestamp)
  function
  | String s when Regex.IsMatch(s, "\d{19,21}") ->
    System.Int64.Parse(s, Culture.invariant)
      |> JsonResult.pass
  | json ->
    isoString json
      |> JsonResult.tagOnFail (PropertyTag "timestamp / isoString")

let timestamp (getTimestamp: unit -> EpochNanoSeconds): JsonDecoder<EpochNanoSeconds> =
  fun json ->
    let now = getTimestamp ()
    let minI, maxI =
      now - Duration.FromDays(365 * 2).ToInt64Nanoseconds(),
      now + Duration.FromDays(365 * 2).ToInt64Nanoseconds()
    let decoder =
      match json with
      | Number n when n <> "0" ->
        numericTimestamp now (minI, maxI)
      | Number _ ->
        Decoder.always (JsonResult.pass now)
      | _ ->
        stringTimestamp
    decoder json

let foldIntoBase (getTimestamp: unit -> EpochNanoSeconds) (acc: #Model.LogaryMessageBase) (key: string, json: Json): JsonResult<#Model.LogaryMessageBase> =
  match key with
  | "id" ->
    D.eitherNamed ("id of base64", idB64)
                  ("id of hex", idHex)
                  json
      |> JsonResult.map (fun mId -> acc.id <- mId; acc)
      |> JsonResult.tagOnFail (PropertyTag "id")

  | "parentSpanId" ->
    D.eitherNamed ("parentSpanId of base64", spanId)
                  ("parentSpanId of hex", spanIdHex)
                  json
      |> JsonResult.map (fun spanId -> acc.parentSpanId <- Some spanId; acc)
      |> JsonResult.tagOnFail (PropertyTag "parentSpanId")

  | "name" ->
    pointName json
      |> JsonResult.map (fun name -> acc.name <- name; acc)
      |> JsonResult.tagOnFail (PropertyTag "name")

  | "level" ->
    level json
      |> JsonResult.map (fun level -> acc.level <- level; acc)
      |> JsonResult.tagOnFail (PropertyTag "level")

  | "timestamp" ->
    timestamp getTimestamp json
      |> JsonResult.map (fun ts -> acc.timestamp <- ts; acc)
      |> JsonResult.tagOnFail (PropertyTag "timestamp")

  | "context" ->
    D.mapWith value json
      |> JsonResult.map (fun c -> acc.setContextValues c; acc)
      |> JsonResult.tagOnFail (PropertyTag "context")

  | "fields" ->
    D.mapWith value json
      |> JsonResult.map (fun fs -> acc.setFieldValues fs; acc)
      |> JsonResult.tagOnFail (PropertyTag "fields")

  | "gauges" ->
    D.mapWith gauge json
      |> JsonResult.map (fun gs -> acc.setGaugeValues gs; acc)
      |> JsonResult.tagOnFail (PropertyTag "gauges")

  | _ ->
    JsonResult.pass acc


let internal logaryMessageWith clock ctorDecoder ctorFactory =
  let folder (m: _) = foldIntoBase clock m

  fun jObj ->
    ctorDecoder jObj
      |> JsonResult.map ctorFactory
      |> JsonResult.bind (fun m -> JsonResult.foldBind folder m (JsonObject.toPropertyList jObj))

let eventMessageReader clock: ObjectReader<Logary.Model.Event> =
  let ctorDecoder =
        fun e m error -> e, m, error
    <!> D.required D.string "event"
    <*> D.eitherNamed ("simple monetaryValue", D.optional money "monetaryValue") ("default None", D.always None)
    <*> D.eitherNamed ("errorInfo", D.optional errorInfo "error") ("default None", D.always None)

  logaryMessageWith clock ctorDecoder (fun (event, monetaryValue, error) ->
      Model.Event(event, monetaryValue, ?error=error))

let eventMessage (getTimestamp: unit -> EpochNanoSeconds): JsonDecoder<Logary.Model.Event> =
  D.jsonObjectWith (eventMessageReader getTimestamp)

let eventMessageInterface clock: JsonDecoder<EventMessage> =
  eventMessage clock |> Decoder.map (fun m -> m :> EventMessage)


let gaugeMessageReader clock: ObjectReader<Model.GaugeMessage> =
  let ctorDecoder =
        fun g labels -> g, labels
    <!> D.required gauge "gauge"
    <*> D.required (D.mapWith D.string) "labels"

  logaryMessageWith clock ctorDecoder (fun (g, labels) -> Model.GaugeMessage(g, labels))

let gaugeMessage (getTimestamp: unit -> EpochNanoSeconds): JsonDecoder<Model.GaugeMessage> =
  D.jsonObjectWith (gaugeMessageReader getTimestamp)


let histogramMessageReader clock: ObjectReader<Model.HistogramMessage> =
  let ctorDecoder =
        fun labels buckets sum -> labels, buckets, sum
    <!> D.required (D.mapWith D.string) "labels"
    <*> D.required floatMap "buckets"
    <*> D.required D.float "sum"

  logaryMessageWith clock ctorDecoder (fun (labels, buckets, sum) -> Model.HistogramMessage(labels, buckets, sum))

let histogramMessage (getTimestamp: unit -> EpochNanoSeconds): JsonDecoder<Model.HistogramMessage> =
  D.jsonObjectWith (histogramMessageReader getTimestamp)


let foldSpan (getTimestamp: unit -> EpochNanoSeconds) (m: Model.SpanMessage) (key: string, json: Json): JsonResult<Model.SpanMessage> =
  match key with
  | "label" ->
    D.string json
      |> JsonResult.map (fun label -> m.label <- label; m)
      |> JsonResult.tagOnFail (PropertyTag "label")

  | "traceId" -> // part of spanContext
    traceId json
      |> JsonResult.map (fun traceId -> m.context <- m.context.withTraceId traceId; m)
      |> JsonResult.tagOnFail (PropertyTag "traceId")

  | "spanId" -> // part of spanContext
    spanId json
      |> JsonResult.map (fun spanId -> m.context <- m.context.withSpanId spanId; m)
      |> JsonResult.tagOnFail (PropertyTag "spanId")

  | "parentSpanId" -> // part of spanContext
    spanId json
      |> JsonResult.map (fun parentSpanId -> m.context <- m.context.withParentId (Some parentSpanId); m)
      |> JsonResult.tagOnFail (PropertyTag "spanId")

  | "spanContext" ->
    spanContext json
      |> JsonResult.map (fun sc -> m.context <- sc; m)
      |> JsonResult.tagOnFail (PropertyTag "spanContext")

  | "traceState" ->
    JsonResult.pass m // TODO: W3C parser on: D.string |> JsonResult.bind (parse W3C.traceState >> JsonResult.ofParseResult)

  | "traceContext" ->
    traceContext json
      |> JsonResult.map (fun tc -> m.context <- m.context.withContext tc; m)
      |> JsonResult.tagOnFail (PropertyTag "traceContext")

  | "kind" ->
    spanKind json
      |> JsonResult.map (fun kind -> m.spanKind <- kind; m)
      |> JsonResult.tagOnFail (PropertyTag "kind")

  | "started" ->
    timestamp getTimestamp json
      |> JsonResult.map (fun started -> m.started <- started; m)
      |> JsonResult.tagOnFail (PropertyTag "started")

  | "finished" ->
    timestamp getTimestamp json
      |> JsonResult.map (fun finished -> m.finished <- Some finished; m)
      |> JsonResult.tagOnFail (PropertyTag "finished")

  | "flags" ->
    spanFlags json
      |> JsonResult.map (fun flags -> m.flags <- flags; m)
      |> JsonResult.tagOnFail (PropertyTag "flags")

  | "links" ->
    D.arrayWith spanLink json
      |> JsonResult.map (fun links -> m.setLinks links; m)
      |> JsonResult.tagOnFail (PropertyTag "links")

  | "events" ->
    D.arrayWith (eventMessageInterface getTimestamp) json
      |> JsonResult.map (fun es -> m.setEvents es; m)
      |> JsonResult.tagOnFail (PropertyTag "events")

  | "attrs" ->
    D.mapWith value json
      |> JsonResult.map (fun attrs -> m.setAttributes attrs; m)
      |> JsonResult.tagOnFail (PropertyTag "attrs")

  | "status" ->
    spanStatus json
      |> JsonResult.map (fun s -> m.status <- Some s; m)
      |> JsonResult.tagOnFail (PropertyTag "status")

  | _ ->
    JsonResult.pass m


let spanMessageReader clock: ObjectReader<Model.SpanMessage> =
  let folder m = foldSpan clock m

  let ctorDecoder: ObjectReader<Model.SpanMessage> =
    fun jObj ->
      let m = Model.SpanMessage()
      jObj
        |> JsonObject.toPropertyList
        |> JsonResult.foldBind folder m

  logaryMessageWith clock ctorDecoder (fun m -> m)

let spanMessage (getTimestamp: unit -> EpochNanoSeconds): JsonDecoder<Model.SpanMessage> =
  D.jsonObjectWith (spanMessageReader getTimestamp)


let identifyUserMessageReader clock: ObjectReader<Model.IdentifyUserMessage> =
  let ctorDecoder =
        fun prevUserId newUserId -> prevUserId, newUserId
    <!> D.required D.string "prevUserId"
    <*> D.required D.string "newUserId"

  logaryMessageWith clock ctorDecoder (fun (prevUserId, newUserId) ->
    Model.IdentifyUserMessage(prevUserId, newUserId))


let identifyUserMessage (getTimestamp: unit -> EpochNanoSeconds): JsonDecoder<Model.IdentifyUserMessage> =
  D.jsonObjectWith (identifyUserMessageReader getTimestamp)


let setUserPropertyMessageReader clock: ObjectReader<Model.SetUserPropertyMessage> =
  let ctorDecoder =
        fun userId key value -> userId, key, value
    <!> D.required D.string "userId"
    <*> D.required D.string "key"
    <*> D.required value "value"

  logaryMessageWith clock ctorDecoder (fun (u, k, v) -> Model.SetUserPropertyMessage(u, k, v))

let setUserPropertyMessage clock: JsonDecoder<Model.SetUserPropertyMessage> =
  D.jsonObjectWith (setUserPropertyMessageReader clock)


let forgetUserMessageReader clock: ObjectReader<Model.ForgetUserMessage> =
  let ctorDecoder = D.required D.string "userId"
  logaryMessageWith clock ctorDecoder (fun u -> Model.ForgetUserMessage(u))

let forgetUserMessage clock: JsonDecoder<Model.ForgetUserMessage> =
  D.jsonObjectWith (forgetUserMessageReader clock)


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
      | MessageKind.ForgetUser      -> forgetUserMessageReader >> toBase
      | MessageKind.Control         ->
        fun _ _ -> JsonResult.messageTypeUnknown "ControlMessage are not accepted for deserialisation"

    Global.getTimestampD |> DVar.mapFun reader

  D.required kind "type" |> Decoder.bind parseByType

let logaryMessage: JsonDecoder<Model.LogaryMessageBase> =
  D.jsonObjectWith logaryMessageReader

let logaryMessageArray: JsonDecoder<Model.LogaryMessageBase[]> =
  D.arrayWith logaryMessage

let messageBatch: JsonDecoder<Model.LogaryMessageBase[]> =
  D.eitherNamed ("single message", logaryMessage |> Decoder.map Array.singleton)
                ("batch of message", logaryMessageArray)
