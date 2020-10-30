module Logary.Json.Encode

open System
open System.Collections.Generic
open Logary
open Logary.Trace
open Logary.Internals.Chiron
open Logary.Internals.Chiron.JsonTransformer

module E = Json.Encode
module EI = Inference.Json.Encode

let kind: JsonEncoder<MessageKind> =
  function
  | MessageKind.Control -> E.string "control"
  | MessageKind.Event -> E.string "event"
  | MessageKind.Span -> E.string "span"
  | MessageKind.Gauge -> E.string "gauge"
  | MessageKind.Histogram -> E.string "histogram"
  | MessageKind.IdentifyUser -> E.string "identifyUser"
  | MessageKind.SetUserProperty -> E.string "setUserProperty"
  | MessageKind.ForgetUser -> E.string "forgetUser"

let resource: JsonEncoder<Model.Resource> =
  fun r ->
    E.stringMap (r.asMap())

let pointName: JsonEncoder<PointName> =
  fun pn ->
    E.string (pn.ToString())

let idB64: JsonEncoder<Id> =
  fun id ->
    E.string (id.toBase64String())

let idHex: JsonEncoder<Id> =
  fun tId ->
    E.string (tId.to32HexString())

let traceId: JsonEncoder<TraceId> = idB64
let traceIdHex: JsonEncoder<TraceId> = idHex

let spanId: JsonEncoder<SpanId> =
  fun sId ->
    E.string (sId.toBase64String())

let spanIdHex: JsonEncoder<SpanId> =
  fun sId ->
    E.string (sId.toHexString())

let spanKind: JsonEncoder<SpanKind> =
  (fun x -> x.asInt) >> E.int

let level: JsonEncoder<LogLevel> =
  fun level ->
    E.string (level.ToString())

let value: JsonEncoder<Value> =
  function
  | Value.Bool b ->
    E.bool b
  | Value.Float f ->
    E.float f
  | Value.Int64 i ->
    E.int64 i
  | Value.Fraction (n, d) ->
    let inner (n, d) =
      EI.required "type" "fraction"
      >> E.required E.int64 "numerator" n
      >> E.required E.int64 "denominator" d
    E.jsonObjectWith inner (n, d)
  | Value.BigInt i ->
    E.bigint i
  | Value.Str s ->
    E.string s

let currency: JsonEncoder<Currency> =
  function
  | Currency.USD -> E.string "USD"
  | Currency.EUR -> E.string "EUR"
  | Currency.Other other -> E.string (other.ToUpperInvariant())

let rec units: JsonEncoder<U> =
  function
  | U.Bits ->
    E.string "bits"
  | U.Bytes ->
    E.string "bytes"
  | U.Seconds ->
    E.string "seconds"
  | U.Metres ->
    E.string "metres"
  | U.Scalar ->
    E.string "scalar"
  | U.Amperes ->
    E.string "amperes"
  | U.Kelvins ->
    E.string "kelvins"
  | U.Moles ->
    E.string "moles"
  | U.Candelas ->
    E.string "candelas"
  | U.Percent ->
    E.string "percent"
  | U.Watts ->
    E.string "watts"
  | U.Hertz ->
    E.string "hertz"
  | U.Joules ->
    E.string "joules"
  | U.Grams ->
    E.string "grams"
  | U.Currency c ->
    currency c
  | U.Other u ->
    E.string u
  | U.Scaled (u, v) ->
    let inner (u, v) =
      E.required E.string "type" "scaled"
      >> E.required units "unit" u
      >> E.required E.float "value" v
    E.jsonObjectWith inner (u, v)
  | U.Offset (u, v) ->
    let inner (u, v) =
      E.required E.string "type" "offset"
      >> E.required units "unit" u
      >> E.required E.float "value" v
    E.jsonObjectWith inner (u, v)
  | U.Mul (unitA, unitB) ->
    let inner (a, b) =
      E.required E.string "type" "mul"
      >> E.required units "unitA" a
      >> E.required units "unitB" b
    E.jsonObjectWith inner (unitA, unitB)
  | U.Pow (baseU, power) ->
    let inner (baseU, power) =
      E.required E.string "type" "pow"
      >> E.required units "base" baseU
      >> E.required E.float "power" power
    E.jsonObjectWith inner (baseU, power)
  | U.Div (num, denom) ->
    let inner (n, d) =
      E.required E.string "type" "div"
      >> E.required units "numerator" n
      >> E.required units "denominator" d
    E.jsonObjectWith inner (num, denom)
  | U.Root u ->
    let inner u =
      E.required E.string "type" "root"
      >> E.required units "unit" u
    E.jsonObjectWith inner u
  | U.Log10 baseU ->
    let inner baseU =
      E.required E.string "type" "log10"
      >> E.required units "base" baseU
    E.jsonObjectWith inner baseU

let gauge: JsonEncoder<Gauge> =
  let inner (Gauge (v, u)) =
    EI.required "type" "gauge"
    >> E.required value "value" v
    >> E.required units "unit" u
  E.jsonObjectWith inner

let moduleInfo: JsonEncoder<ModuleInfo> =
  fun m ->
    E.propertyList [
      if m.name.IsSome then "name", String m.name.Value
      if m.buildId.IsSome then "buildId", String m.buildId.Value
    ]

let stackFrameBuilder: ObjectBuilder<StackFrame> =
  fun frame ->
    E.optional E.string "site" frame.site
    >> E.optional E.string "file" frame.file
    >> E.optional E.string "functionName" frame.functionName
    >> E.optional E.string "originalFunctionName" frame.originalFunctionName
    >> E.optional E.int "lineNo" frame.lineNo
    >> E.optional E.int "colNo" frame.colNo
    >> E.optional E.string "sourceVersion" frame.sourceVersion
    >> E.optional moduleInfo "loadModule" frame.loadModule

let stackFrame: JsonEncoder<StackFrame> =
  E.jsonObjectWith stackFrameBuilder


let stackTraceBuilder: ObjectBuilder<StackTrace> =
  fun st ->
    E.required (E.ilistWith stackFrame) "frames" st.frames
    >> E.required E.uint16 "droppedFramesCount" st.droppedFramesCount

let stackTrace: JsonEncoder<StackTrace> =
  E.jsonObjectWith stackTraceBuilder


let rec errorInfoBuilder: ObjectBuilder<ErrorInfo> =
  fun ei ->
    E.optional E.string "message" ei.message
    >> E.optional E.string "errorType" ei.errorType
    >> E.required stackTrace "stackTrace" ei.stackTrace
    >> E.optional errorInfo "inner" ei.inner

and errorInfo: JsonEncoder<ErrorInfo> =
  E.jsonObjectWith errorInfoBuilder

let errorInfos infos = E.arrayWith errorInfo infos

let logaryMessage: ObjectBuilder<LogaryMessage> =
  fun m ->
    E.required kind "type" m.kind
    >> E.required idB64 "id" m.id
    >> E.optional spanId "parentSpanId" m.parentSpanId
    >> E.required pointName "name" m.name
    >> E.required level "level" m.level
    >> E.optional E.int64 "received" m.received
    >> E.required E.int64 "timestamp" m.timestamp
    >> E.required (E.readDictWith value) "context" m.context
    >> E.required (E.readDictWith value) "fields" m.fields
    >> E.required (E.readDictWith gauge) "gauges" m.gauges

let traceStateKey: JsonEncoder<TraceStateKey> =
  let inner (TraceStateKey (k, v)) =
    E.required E.string "key" k
    >> E.optional E.string "vendor" v
  E.jsonObjectWith inner

let traceStateBuilder: ObjectBuilder<TraceState> =
  let traceStateKV = E.listWith (E.tuple2 traceStateKey E.string)
  fun ts -> E.required traceStateKV "traceState" ts.value

let traceContextBuilder: ObjectBuilder<IReadOnlyDictionary<string, string>> =
  let dictEncoder = E.readDictWith E.string
  E.required dictEncoder "traceContext"

let spanFlags: JsonEncoder<SpanFlags> =
  int >> E.int

let spanContextBuilder: ObjectBuilder<SpanContext> =
  fun c ->
    E.requiredMixin traceContextBuilder c.traceContext
    >> E.requiredMixin traceStateBuilder c.traceState
    >> E.required spanId "spanId" c.spanId
    >> E.required traceId "traceId" c.traceId
    >> E.optional spanId "parentSpanId" c.parentSpanId
    >> E.required spanFlags "flags" c.flags

let spanContext: JsonEncoder<SpanContext> =
  E.jsonObjectWith spanContextBuilder

let spanLink: JsonEncoder<SpanLink> =
  let inner predecessorE from: ObjectBuilder<_> =
    fun (predecessor, attrs) ->
      E.required E.string "type" (sprintf "followsFrom%s" from)
      >> E.required predecessorE "predecessor" predecessor
      >> E.required (E.readDictWith value) "attrs" attrs

  let fromTrace = inner traceId "Trace"
  let fromSpan = inner spanContext "Span"

  function
  | FollowsFromTrace (preTraceId, attrs) ->
    E.jsonObjectWith fromTrace (preTraceId, attrs)
  | FollowsFromSpan (preSpan, attrs) ->
    E.jsonObjectWith fromSpan (preSpan, attrs)

let eventMessageBuilder: ObjectBuilder<EventMessage> =
  fun e ->
    E.requiredMixin logaryMessage e
    >> E.required E.string "event" e.event
    >> E.optional gauge "monetaryValue" e.monetaryValue

let eventMessage: JsonEncoder<EventMessage> =
  E.jsonObjectWith eventMessageBuilder

let spanStatus: JsonEncoder<SpanStatus> =
  let inner (s: SpanStatus) =
    E.required E.string "type" "spanStatus"
    >> E.required (int >> E.int) "code" s.code
    >> E.required (int >> E.int) "source" s.source
    >> E.optional E.string "description" s.description

  E.jsonObjectWith inner

let spanMessageBuilder: ObjectBuilder<SpanMessage> =
  fun s ->
    E.requiredMixin logaryMessage s
    >> E.required spanContext "spanContext" s.context
    >> E.required E.string "label" s.label
    >> E.required spanKind "kind" s.kind
    >> E.required E.int64 "started" s.started
    >> E.required E.int64 "finished" s.finished
    >> E.required (E.arrayWith spanLink) "links" (Array.ofSeq s.links)
    >> E.required (E.arrayWith eventMessage) "events" (Array.ofSeq s.events)
    >> E.optional spanStatus "status" s.status
    // "attrs" -> "fields", so we don't serialise these
    // >> E.required (E.readDictWith value) "attrs" s.attrs

let spanMessage: JsonEncoder<SpanMessage> =
  E.jsonObjectWith spanMessageBuilder

let gaugeMessageBuilder: ObjectBuilder<GaugeMessage> =
  fun g ->
    E.requiredMixin logaryMessage g
    >> E.required gauge "gauge" g.gauge
    >> E.required (E.readDictWith E.string) "labels" g.labels

let gaugeMessage: JsonEncoder<GaugeMessage> =
  E.jsonObjectWith gaugeMessageBuilder

let histogramMessageBuilder: ObjectBuilder<HistogramMessage> =
  let buckets = E.readDictWithCustomKey (fun (f: float) -> (f :> IFormattable).ToString("G", Culture.invariant)) E.float
  let labels = E.readDictWith E.string
  fun m ->
    E.requiredMixin logaryMessage m
    >> E.required buckets "buckets" m.buckets
    >> E.required labels "labels" m.labels
    >> E.required E.float "sum" m.sum

let histogramMessage: JsonEncoder<HistogramMessage> =
  E.jsonObjectWith histogramMessageBuilder

let identifyUserMessageBuilder: ObjectBuilder<IdentifyUserMessage> =
  fun ium ->
    E.requiredMixin logaryMessage ium
    >> E.required E.string "prevUserId" ium.prevUserId
    >> E.required E.string "newUserId" ium.newUserId

let identifyUserMessage: JsonEncoder<IdentifyUserMessage> =
  E.jsonObjectWith identifyUserMessageBuilder


let setUserPropertyMessageBuilder: ObjectBuilder<SetUserPropertyMessage> =
  fun supm ->
    E.requiredMixin logaryMessage supm
    >> E.required E.string "userId" supm.userId
    >> E.required E.string "key" supm.key
    >> E.required value "value" supm.value

let setUserPropertyMessage: JsonEncoder<SetUserPropertyMessage> =
  E.jsonObjectWith setUserPropertyMessageBuilder


let forgetUserMessageBuilder: ObjectBuilder<ForgetUserMessage> =
  fun supm ->
    E.requiredMixin logaryMessage supm
    >> E.required E.string "userId" supm.userId

let forgetUserMessage: JsonEncoder<ForgetUserMessage> =
  E.jsonObjectWith forgetUserMessageBuilder


let logaryMessageBase: JsonEncoder<LogaryMessage> =
  fun (m: LogaryMessage) ->
    match m.kind with
    | MessageKind.Control         -> E.propertyList ["type", String "control"]
    | MessageKind.Event           -> m.getAsOrThrow<EventMessage>() |> eventMessage
    | MessageKind.Span            -> m.getAsOrThrow<SpanMessage>() |> spanMessage
    | MessageKind.Gauge           -> m.getAsOrThrow<GaugeMessage>() |> gaugeMessage
    | MessageKind.Histogram       -> m.getAsOrThrow<HistogramMessage>() |> histogramMessage
    | MessageKind.IdentifyUser    -> m.getAsOrThrow<IdentifyUserMessage>() |> identifyUserMessage
    | MessageKind.SetUserProperty -> m.getAsOrThrow<SetUserPropertyMessage>() |> setUserPropertyMessage
    | MessageKind.ForgetUser      -> m.getAsOrThrow<ForgetUserMessage>() |> forgetUserMessage
