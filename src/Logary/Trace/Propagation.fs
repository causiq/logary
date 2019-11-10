namespace Logary.Trace.Propagation

open System
open Logary.Trace

/// https://w3c.github.io/trace-context/#trace-context-http-headers-format
module W3CTraceContext =
  ()

module Jaeger =

  open System.Text.RegularExpressions
  let internal (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.IgnorePatternWhitespace ||| RegexOptions.IgnoreCase)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


  /// https://github.com/jaegertracing/jaeger-client-node#debug-traces-forced-sampling
  ///
  /// When Jaeger sees this header in the request that otherwise has no tracing context, it ensures that the new trace started for this request will be sampled in the "debug" mode (meaning it should survive all downsampling that might happen in the collection pipeline), and the root span will have a tag as if this statement was executed:
  /// `span.setTag('jaeger-debug-id', 'some-correlation-id')`
  let DebugIdHeader = "jaeger-debug-id"

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
  let TraceHeader = "uber-trace-id"
  let BaggageHeaderPrefix = "uberctx-"
  let ExtractRegex = "^([A-F0-9]{32}|[A-F0-9]{16}):([A-F0-9]{1,16}):([A-F0-9]{0,16}):(\d)$"


  let extract (get: Getter<'t>) (source: 't): SpanAttr list * SpanContext option =
    let context =
      match get source TraceHeader with
      | (_, headerValue :: _) :: _ ->
        match headerValue with
        | Regex ExtractRegex [ traceId; spanId; parentSpanId; flags ] ->
          let traceId, spanId = TraceId.ofString traceId, SpanId.ofString spanId
          let psId =
            if parentSpanId = "" || parentSpanId = "0"
            then None else Some (SpanId.ofString parentSpanId)
          let flags = enum<SpanFlags>(int flags)
          Some (SpanContext(traceId, spanId, flags, ?parentSpanId=psId))
        | _ ->
          None
      | _ ->
        None

    let attrs: SpanAttr list =
      match get source DebugIdHeader with
      | (_, debugIdValue :: _) :: _ ->
        [ DebugIdHeader, SpanAttrValue.S debugIdValue ]
      | _ ->
        []

    let traceState: Map<string, string> =
      [ for header, values in get source BaggageHeaderPrefix do
          match values with
          | valueEncoded :: _ ->
            let key = header.Substring(BaggageHeaderPrefix.Length)
            yield key, Uri.UnescapeDataString valueEncoded
          | _ -> ()
      ]
      |> Map.ofSeq

    attrs,
    if Map.isEmpty traceState then context
    else context |> Option.map (fun ctx -> ctx.withState traceState)

  let inject (setter: Setter<'t>) (context: SpanContext) (target: 't): 't =
    let psId = match context.parentSpanId with None -> "0" | Some psId -> psId.ToString()
    let trace = sprintf "%O:%O:%O:%i" context.traceId context.spanId psId (byte context.flags)
    let setBaggage target =
      context.traceState
        |> Seq.map (fun (KeyValue (k, v)) -> sprintf "%s%s" BaggageHeaderPrefix k, (SpanAttrValue.S v).uriEncode() :: [])
        |> Seq.fold (fun x data -> setter data x) target

    target
      |> setter (TraceHeader, trace :: [])
      |> setBaggage

  let propagator =
    { new Propagator with
        member x.extract(getter, instance) =
          extract getter instance
        member x.inject(setter, ctx, target) =
          inject setter ctx target
    }

module B3 =
  ()

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
