namespace Logary.Trace.Propagation

open System
open Logary.Trace

/// https://w3c.github.io/trace-context/#trace-context-http-headers-format
module W3CTraceContext =
  ()

module Jaeger =

  open System.Text.RegularExpressions
  let private (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern, RegexOptions.IgnorePatternWhitespace)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

  /// https://github.com/jaegertracing/jaeger-client-node#debug-traces-forced-sampling
  let DebugIdHeader = "jaeger-debug-id"
  /// uber-trace-id: {trace-id}:{span-id}:{parent-span-id}:{flags}
  let TraceHeader = "uber-trace-id"
  let BaggageHeaderPrefix = "uberctx-"

  let extract (get: Getter<'t>) (source: 't): SpanAttr list * SpanAttr list * SpanContext option =
    let context =
      match get source TraceHeader with
      | (_, headerValue :: _) :: _ ->
        match headerValue with
        | Regex "^([A-F0-9]{32}|[A-F0-9]{16}):([A-F0-9]{1,16}):([A-F0-9]{1,16}):(\d)$" [ traceId; spanId; parentSpanId; flags ] ->
          let traceId, spanId = TraceId.ofString traceId, SpanId.ofString spanId
          let psId = if parentSpanId = "" then None else Some (SpanId.ofString parentSpanId)
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

    let baggage: SpanAttr list =
      [ for header, values in get source BaggageHeaderPrefix do
          match values with
          | valueEncoded :: _ ->
            let key = header.Substring(BaggageHeaderPrefix.Length)
            yield key, SpanAttrValue.S (Uri.UnescapeDataString valueEncoded)
          | _ -> ()
      ]

    attrs, baggage, context

  let inject (setter: Setter<'t>) (contextAttrs: SpanAttr list, context: SpanContext) (target: 't): 't =
    let trace = sprintf "%O:%O:%O:%i" context.traceId context.spanId context.parentSpanId (byte context.flags)
    let setBaggage target =
      contextAttrs
        |> List.map (fun (k, v) -> sprintf "%s%s" BaggageHeaderPrefix k, v.uriEncode() :: [])
        |> List.fold (fun x data -> setter data x) target

    target
      |> setter (TraceHeader, trace :: [])
      |> setBaggage

  let propagator =
    { new Propagator with
        member x.extract(getter, instance) =
          extract getter instance
        member x.inject(setter, ctxAttrs, ctx, target) =
          inject setter (ctxAttrs, ctx) target
    }
