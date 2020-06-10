[<AutoOpen>]
module Logary.Extensions

open System.Collections.Generic
open System.Runtime.Serialization
open NodaTime
open System
open System.Globalization
open Logary.Trace
open Logary.Internals

type Value with
  member x.uriEncode () = Uri.EscapeUriString (x.ToString())
  static member ofPrimitive (s: string) = Value.Str s
  static member ofPrimitive (i: uint16) = Value.Int64 (int64 i)
  static member ofPrimitive (i: int16) = Value.Int64 (int64 i)
  static member ofPrimitive (i: uint32) = Value.Int64 (int64 i)
  static member ofPrimitive (i: int32) = Value.Int64 (int64 i)
  static member ofPrimitive (i: int64) = Value.Int64 i
  static member ofPrimitive (i: uint64) = Value.BigInt (bigint i)
  static member ofPrimitive (b: bool) = Value.Bool b
  static member ofPrimitive (bi: bigint) = Value.BigInt bi
  static member ofPrimitive (f: float) = Value.Float f
  static member ofPrimitive (sid: SpanId) = Value.Str (sid.toBase64String())
  static member ofPrimitive (tid: TraceId) = Value.Str (tid.toBase64String())

type Id with
  static member create (?high, ?low) =
    { high = high |> Option.defaultWith Rnd.nextInt64NonZero
      low = low |> Option.defaultWith Rnd.nextInt64NonZero }

  static member ofGuid (g: Guid) =
    let bs = g.ToByteArray()
    { high = BitConverter.ToInt64(bs, 0)
      low = BitConverter.ToInt64(bs, 8) }

  static member ofString (s: string) =
    if s.Length > 32 then TraceId.Zero else
    let highLen = max 0 (s.Length - 16)
    let high = // "".Substring(0, 0) => ""
      match Int64.TryParse(s.Substring(0, highLen), NumberStyles.HexNumber, null) with
      | false, _ -> 0L
      | true, high -> high
    let low =
      match Int64.TryParse(s.Substring highLen, NumberStyles.HexNumber, null) with
      | false, _ -> 0L
      | true, low -> low
    TraceId.create (high, low)

type SpanId with
  static member create (?value: int64) =
    { id = value |> Option.defaultWith Rnd.nextInt64NonZero }
  static member ofTraceId (tId: TraceId) =
    SpanId.create tId.low
  static member ofString (s: string) =
    match Int64.TryParse(s, NumberStyles.HexNumber, null) with
    | false, _ -> SpanId.Zero
    | true, v -> SpanId.create v

type SpanMessage with
  member x.elapsed =
    match x.finished with
    | 0L | _ when x.finished < x.started -> Duration.Zero
    | finished -> Duration.FromNanoseconds(finished - x.started)
  /// Gets whether this Span has a `Debug` or `Sampled` `SpanFlags` flag. Downstream collectors may choose to down-sample,
  /// so setting the `Sampled` flag is no guarantee that the Span will be sampled.
  member x.isSampled = x.flags &&& SpanFlags.Sampled = SpanFlags.Sampled
  /// Is false if this Status represents an error, otherwise true.
  member x.isOK =
    let s, _ = x.status
    s = SpanCanonicalCode.OK
  /// Returns the SpanId of the parent of this SpanData.
  member x.parentSpanId = x.context.parentSpanId
  /// Returns the Resource associated with this SpanData. When None is returned the assumption is that Resource will be taken from the Logger that is used to record this SpanData.
  member x.resource =
    match box x with
    | :? Logger as logger -> Some logger.name
    | _ -> None
  member x.tryGet key =
    match x.attrs.TryGetValue key with
    | false, _ -> None
    | true, value -> Some value
  member x.tryGetString key =
    match x.tryGet key with
    | Some (Value.Str value) -> Some value
    | _ -> None
  member x.tryGetBool key =
    match x.tryGet key with
    | Some (Value.Bool value) -> Some value
    | _ -> None
  member x.tryGetValue key =
    x.tryGet key
  member x.tryGetFloat key =
    x.tryGetValue key
      |> Option.bind (function
        | Value.Float f -> Some f
        | _ -> None)
  member x.tryGetInt key =
    x.tryGetValue key
      |> Option.bind (function
          | Value.Int64 i -> Some i
          | _ -> None)
  /// Returns the `component` attribute value associated with the `SpanData`; useful to use in conjunction with
  /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-semantic-conventions.md
  member x.``component`` =
    x.tryGetString "component"

type SpanOps with
  member x.sample () =
    x.setFlags (fun flags -> flags ||| SpanFlags.Sampled)
  member x.setDebug (debug: bool) =
    x.setFlags (fun flags ->
      if debug then
        flags ||| SpanFlags.Debug ||| SpanFlags.Sampled
      else
        ~~~SpanFlags.Debug &&& flags)
  member x.debug () =
    x.setDebug true
  member x.clearFlags () =
    x.setFlags (fun _ -> SpanFlags.None)
  member x.setAttribute (key: string, value: int) =
    x.setAttribute(key, Value.Int64 (int64 value))
  member x.setAttribute (key: string, value: int64) =
    x.setAttribute(key, Value.Int64 value)
  member x.setAttribute (key: string, value: float) =
    x.setAttribute(key, Value.Float value)
  member x.setAttribute (key: string, value: bigint) =
    x.setAttribute(key, Value.BigInt value)

type LogaryMessage with
  member x.asInstant = Instant.ofEpoch x.timestamp

  /// NOTE: only acts on the `Logary.*Message` types; NOT the `Logary.Model.*Message*` types.
  member x.tryGetAs<'t when 't :> LogaryMessage>(): 't option =
    let isMatch =
      match x.kind with
      | MessageKind.Control when typeof<'t> = typeof<ControlMessage> -> true
      | MessageKind.Event when typeof<'t> = typeof<EventMessage> -> true
      | MessageKind.Span when typeof<'t> = typeof<SpanMessage> -> true
      | MessageKind.Gauge when typeof<'t> = typeof<GaugeMessage> -> true
      | MessageKind.Histogram when typeof<'t> = typeof<HistogramMessage> -> true
      | _ -> false
    if isMatch then Some (x :?> 't) else None

  member x.getAsOrThrow<'t when 't :> LogaryMessage>(): 't =
    match x.tryGetAs<'t>() with
    | None ->
      failwithf "Failed to get %s – had actual type %s" typeof<'t>.FullName (x.GetType().FullName)
    | Some res ->
      res

  member x.tryGetField(key: string): Value option =
    match x.fields.TryGetValue key with
    | true, value -> Some value
    | false, _ -> None

  member x.tryGetFieldString(key: string): string option =
    x.tryGetField key
      |> Option.bind (function Value.Str s -> Some s | _ -> None)

  member x.tryGetContext(key: string): Value option =
    match x.context.TryGetValue key with
    | false, _ -> None
    | true, value -> Some value

  /// Tries to get from both fields and context, first from the fields, then from the context
  member x.tryGet(key: string) =
    x.tryGetField key
      |> Option.orElseWith (fun () -> x.tryGetContext key)

  /// Tries to get from both fields and context, first from the fields, then from the context
  member x.tryGetString(key: string) =
    x.tryGet key
      |> Option.bind (function Value.Str s -> Some s | _ -> None)


type System.Exception with
  member x.toErrorInfo(?oig: ObjectIDGenerator, ?seen: IDictionary<int64, ErrorInfo>): ErrorInfo =
    let seen = seen |> Option.defaultWith (fun () -> Dictionary<_,_>() :> _)
    let oig = oig |> Option.defaultWith ObjectIDGenerator
    match oig.HasId x with
    | oid, true ->
      let st = DotNetStacktrace.tryParse x.StackTrace |> Option.map (fun ei -> ei.stackTrace)
      let inner = if isNull x.InnerException then None else Some (x.InnerException.toErrorInfo(oig, seen))
      let ei = ErrorInfo(x.Message, x.GetType().FullName, ?stackTrace=st, ?inner=inner)
      seen.[oid] <- ei
      ei
    | oid, false ->
      seen.[oid]
