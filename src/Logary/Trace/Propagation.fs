namespace Logary.Trace.Propagation

open System
open Logary.Trace
open Logary.Internals.Regex

/// https://w3c.github.io/trace-context/#trace-context-http-headers-format
module W3CTraceContext =

  let extract (getter: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =
    [], None

  let inject (setter: Setter<'a>) (ctx: SpanContext) (target: 'a): 'a =
    target

  let propagator =
    { new Propagator with
        member x.extract(getter, carrier) =
          extract getter carrier
        member x.inject(setter, ctx, carrier) =
          inject setter ctx carrier
       }

module internal JaegerBaggage =
  let Prefix = "uberctx-"

  let extract (source: (string * string list) list): Map<string, string> =
    [ for header, values in source do
        match values with
        | valueEncoded :: _ ->
          let key = header.Substring(Prefix.Length)
          yield key, Uri.UnescapeDataString valueEncoded
        | _ -> ()
    ]
    |> Map.ofSeq


  let inject setter (context: SpanContext) target =
    context.traceState
      |> Seq.map (fun (KeyValue (k, v)) -> sprintf "%s%s" Prefix k, (SpanAttrValue.S v).uriEncode() :: [])
      |> Seq.fold (fun x data -> setter data x) target


/// https://www.jaegertracing.io/docs/1.14/client-libraries/#trace-span-identity
/// `uber-trace-id: {trace-id}:{span-id}:{parent-span-id}:{flags}`
///
/// where
///
/// `{trace-id}`:
///   -  64-bit or 128-bit random number in base16 format
///   -  Can be variable length, shorter values are 0-padded on the left
///   -  Clients in some languages support 128-bit, migration pending
///   -  Value of 0 is invalid
/// `{span-id}`:
///   -  64-bit random number in base16 format
/// `{parent-span-id}`:
///   -  64-bit value in base16 format representing parent span id
///   -  Deprecated, most Jaeger clients ignore on the receiving side, but still include it on the sending side
///      0 value is valid and means “root span” (when not ignored)
/// `{flags}`:
///   - One byte bitmap, as two hex digits
///   - Bit 1 (right-most, least significant) is “sampled” flag
///        * 1 means the trace is sampled and all downstream services are advised to respect that
///        * 0 means the trace is not sampled and all downstream services are advised to respect that
///            * We’re considering a new feature that allows downstream services to upsample if they find their tracing level is too low
///   - Bit 2 is “debug” flag
///        * Debug flag should only be set when the sampled flag is set
///        * Instructs the backend to try really hard not to drop this trace
///   - Other bits are unused
module Jaeger =
  /// https://github.com/jaegertracing/jaeger-client-node#debug-traces-forced-sampling
  ///
  /// When Jaeger sees this header in the request that otherwise has no tracing context, it ensures that the new trace started for this request will be sampled in the "debug" mode (meaning it should survive all downsampling that might happen in the collection pipeline), and the root span will have a tag as if this statement was executed:
  /// `span.setTag('jaeger-debug-id', 'some-correlation-id')`
  let DebugIdHeader = "jaeger-debug-id"
  let TraceHeader = "uber-trace-id"
  let BaggageHeaderPrefix = JaegerBaggage.Prefix
  let ExtractRegex = "^([A-F0-9]{32}|[A-F0-9]{16}):([A-F0-9]{1,16}):([A-F0-9]{0,16}):(\d)$"


  let extract (get: Getter<'t>) (source: 't): SpanAttr list * SpanContext option =

    let context =
      get source TraceHeader
        |> List.tryPick (fun (name, values) -> if name = TraceHeader then Some values else None)
        |> Option.bind (List.tryPick (function
          | Regex ExtractRegex [ traceId; spanId; parentSpanId; flags ] ->
            let traceId, spanId = TraceId.ofString traceId, SpanId.ofString spanId
            let psId =
              if parentSpanId = "" || parentSpanId = "0"
              then None else Some (SpanId.ofString parentSpanId)
            let flags = enum<SpanFlags>(int flags)
            Some (SpanContext(traceId, spanId, flags, ?parentSpanId=psId))
          | _ ->
            None)
        )

    let attrs: SpanAttr list =
      match get source DebugIdHeader with
      | (_, debugIdValue :: _) :: _ ->
        [ DebugIdHeader, SpanAttrValue.S debugIdValue ]
      | _ ->
        []

    let traceState: Map<string, string> =
      get source BaggageHeaderPrefix |> JaegerBaggage.extract

    attrs,
    if Map.isEmpty traceState then context
    else context |> Option.map (fun ctx -> ctx.withState traceState)

  let inject (setter: Setter<'t>) (context: SpanContext) (target: 't): 't =
    let psId = match context.parentSpanId with None -> "0" | Some psId -> psId.ToString()
    let trace = sprintf "%O:%O:%O:%i" context.traceId context.spanId psId (byte context.flags)
    target
      |> setter (TraceHeader, trace :: [])
      |> JaegerBaggage.inject setter context

  let propagator =
    { new Propagator with
        member x.extract(getter, carrier) =
          extract getter carrier
        member x.inject(setter, ctx, carrier) =
          inject setter ctx carrier
    }


/// B3 trace format support
///
/// # Multiple headers [Docs](https://github.com/openzipkin/b3-propagation#multiple-headers)
///
///     X-B3-TraceId: 80f198ee56343ba864fe8b2a57d3eff7
///     X-B3-ParentSpanId: 05e3ac9a4f6e3b90
///     X-B3-SpanId: e457b5a2e4d86bd1
///     X-B3-Sampled: 1
///
/// OR (debug; `"X-B3-Sampled"` is omitted):
///
///     X-B3-TraceId: 80f198ee56343ba864fe8b2a57d3eff7
///     X-B3-ParentSpanId: 05e3ac9a4f6e3b90
///     X-B3-SpanId: e457b5a2e4d86bd1
///     X-B3-Flags: 1
///
/// Single header [Docs](https://github.com/openzipkin/b3-propagation#single-header),
/// [Rationale](https://github.com/openzipkin/b3-propagation/blob/master/RATIONALE.md):
///
///     b3: {traceId}-{spanId}-{samplingState}-{parentSpanId}
///     b3: {traceId}-{spanId}-{samplingState}
///     b3: 0
///
/// where
///
///     samplingState = '0' | '1' | 'd'
///
/// ### Examples:
///
///     b3: 80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1-05e3ac9a4f6e3b90
///
/// OR (sampling decision = don't)
///
///     b3: 0
///
/// OR (no parentSpanId)
///
///     b3: 80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1
///
/// OR (no parentSpanId, nor samplingState — root Span was NOT sampled)
///
///     b3:
///
/// ## Sharing SpanId
///
/// **We don't**, there's no `shared` flag in Logary:
/// [docs](https://github.com/openzipkin/brave/blob/master/brave/README.md#sharing-span-ids-between-client-and-server)
///
///  https://github.com/openzipkin/b3-propagation
module B3 =

  let BaggageHeaderPrefix = JaegerBaggage.Prefix
  let TraceHeader = "b3"
  let TraceIdHeader = "x-b3-traceid"
  let ParentSpanIdHeader = "x-b3-parentspanid"
  let SpanIdHeader = "x-b3-spanid"
  let FlagsHeader = "x-b3-flags"
  let SampledHeader = "x-b3-sampled"


  let ExtractRegex = @"^([A-F0-9]{32}|[A-F0-9]{16})-([A-F0-9]{1,16})(?:-(\d|d)(?:-([A-F0-9]{0,16}))?)?$"

  let internal parseFlags = function
    | "1" -> SpanFlags.Sampled
    | "d" -> SpanFlags.Debug
    | "0" | _ -> SpanFlags.None

  let internal getSingleExact (get: Getter<_>) source header =
    get source header |> List.tryPick (function
      | name, value :: _ when name.ToLowerInvariant() = header -> Some value
      | _ -> None)

  let internal getManyExact (get: Getter<_>) source header =
    get source header |> List.tryPick (function
      | name, values when name.ToLowerInvariant() = header -> Some values
      | _ -> None)

  let extractMulti (get: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =
    let getFlags () =
      getSingleExact get source FlagsHeader
        |> Option.map (fun _ -> SpanFlags.Debug)
        |> Option.orElseWith (fun () ->
          getSingleExact get source SampledHeader
            |> Option.map (fun _ -> SpanFlags.Sampled)
        )
        |> Option.defaultValue SpanFlags.None

    [],
    getSingleExact get source TraceIdHeader |> Option.bind (fun traceId ->
    getSingleExact get source SpanIdHeader |> Option.map (fun spanId ->
    let flags = getFlags ()
    let parentSpanIdO = getSingleExact get source ParentSpanIdHeader |> Option.map SpanId.ofString
    let traceState = get source BaggageHeaderPrefix |> JaegerBaggage.extract
    SpanContext(TraceId.ofString traceId, SpanId.ofString spanId, flags, ?parentSpanId=parentSpanIdO, ?traceState=Some traceState)))

  let injectMulti (setter: Setter<'a>) (context: SpanContext) (target: 'a): 'a =
    let flagsHeader, flagsValue =
      if context.isDebug then FlagsHeader, "1"
      elif context.isSampled then SampledHeader, "1"
      else SampledHeader, "0"

    target
      |> setter (flagsHeader, flagsValue :: [])
      |> setter (TraceIdHeader, context.traceId.ToString() :: [])
      |> setter (SpanIdHeader, context.spanId.ToString() :: [])
      |> match context.parentSpanId with
         | None -> id
         | Some psId -> setter (ParentSpanIdHeader, psId.ToString() :: [])
      |> JaegerBaggage.inject setter context



  let extractSingle (get: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =
    let context =
      getManyExact get source TraceHeader
        |> Option.bind (List.tryPick (function
          | Regex ExtractRegex [ traceId; spanId; flags; parentSpanId ] ->
            let psId = SpanId.ofString parentSpanId
            Some (traceId, spanId, parseFlags flags, Some psId)

          | Regex ExtractRegex [ traceId; spanId; flags ] ->
            Some (traceId, spanId, parseFlags flags, None)

          | Regex ExtractRegex [ traceId; spanId ] ->
            Some (traceId, spanId, SpanFlags.None, None)
          | _ ->
            None)
        )
        |> Option.map (fun (traceId, spanId, flags, psIdO) ->
          SpanContext(TraceId.ofString traceId, SpanId.ofString spanId, flags, ?parentSpanId=psIdO)
        )

    let traceState: Map<string, string> =
      get source BaggageHeaderPrefix |> JaegerBaggage.extract

    [],
    if Map.isEmpty traceState then context
    else context |> Option.map (fun ctx -> ctx.withState traceState)

  let injectSingle (setter: Setter<'a>) (context: SpanContext) (target: 'a): 'a =
    let flags =
      if context.isDebug then "d"
      elif context.isSampled then "1"
      else "0"

    let parentSpanId =
      match context.parentSpanId with
      | None -> ""
      | Some psId -> sprintf "-%O" psId

    let trace = sprintf "%O-%O-%s%s" context.traceId context.spanId flags parentSpanId

    target
      |> setter (TraceHeader, trace :: [])
      |> JaegerBaggage.inject setter context


  let extract (get: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =
    let attrs, scO = extractSingle get source
    match scO with
    | None ->
      extractMulti get source
    | Some sc ->
      attrs, Some sc

  let inject (setter: Setter<'a>) (ctx: SpanContext) (target: 'a): 'a =
    injectSingle setter ctx target


  let propagator =
    { new Propagator with
        member x.extract(getter, carrier) =
          extract getter carrier
        member x.inject(setter, ctx, carrier) =
          inject setter ctx carrier
    }


module Combined =

  /// Chooses the first propagator that matches. It is the first propagator in the list that does the injection.
  let choose: Propagator list -> Propagator =
    fun ps ->
      if ps.Length = 0 then invalidArg "ps" "You must provide at least one propagator"
      { new Propagator with
          member x.extract<'t>(getter, instance: 't) =
            let rec tryExtract = function
              | [] ->
                [], None
              | p: Propagator :: tail ->
                match p.extract(getter, instance) with
                | sAs, Some ctxO ->
                  sAs, Some ctxO
                | _, _ ->
                  tryExtract tail
            tryExtract ps

          member x.inject(setter, ctx, target) =
            let p = List.head ps
            p.inject(setter, ctx, target)
      }
