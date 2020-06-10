namespace Logary.Trace

open System.Collections.Generic
open Logary
open Logary.Trace
open Logary.Internals

type SpanBuilder(label: string) =
  let mutable _label = label
  let mutable _started = Global.getTimestamp()
  let mutable _enableAmbient = false
  let mutable _transform = id
  let mutable _traceId = None
  let mutable _spanId = None
  let mutable _parentSpanId = None
  let mutable _kind = SpanKind.Internal
  let mutable _flags = SpanFlags.None
  let _links = ResizeArray<_>(2)
  let _attrs = Dictionary<string, Value>(5)
  let mutable _status = SpanCanonicalCode.OK, None
  let mutable _logThrough = false
  let _events = ResizeArray<EventMessage>()

  let createContext (): SpanContext * SpanContext option =
    let spanId = _spanId |> Option.defaultWith SpanId.create
    match ActiveSpan.getContext () with
    | Some ambient when _parentSpanId.IsNone && _enableAmbient ->
      //printfn "createContext() => has ambient"
      SpanContext(ambient.traceId, spanId, Some ambient.spanId, _flags ||| ambient.flags),
      Some ambient
    | _ ->
      //printfn "createContext() => has no ambient"
      let traceId = _traceId |> Option.defaultWith TraceId.create
      SpanContext(traceId, spanId, _parentSpanId, _flags),
      None

  /// WARNING: Enabling "ambient" support is only valid if you ONLY use `System.Threading.Tasks.Task` and their
  /// derivatives.
  ///
  /// However, F#'s async doesn't use `AsyncLocal` and neither does Hopac. It is the context that calls `start()`
  /// whose context will be inherited.
  member x.enableAmbient () =
    _enableAmbient <- true
    x

  /// WARNING: Enabling "ambient" support is only valid if you ONLY use `System.Threading.Tasks.Task` and their
  /// derivatives.
  ///
  /// However, F#'s async doesn't use `AsyncLocal` and neither does Hopac. It is the context that calls `start()`
  /// whose context will be inherited.
  member x.setAmbientEnabled enabled =
    _enableAmbient <- enabled
    x

  member x.setLabel label =
    _label <- label
    x

  member x.addLink link =
    _links.Add link
    x
  member x.setAttribute (key: string, value: Value) =
    _attrs.Add (key, value)
    x
  member x.setAttribute (key: string, f: float) = x.setAttribute(key, Value.Float f)
  member x.setAttribute (key: string, i: int64) = x.setAttribute(key, Value.Int64 i)
  member x.setAttribute (key: string, i: int) = x.setAttribute(key, Value.Int64 (int64 i))
  member x.setAttribute (key: string, i: int16) = x.setAttribute(key, Value.Int64 (int64 i))
  member x.setAttribute (key: string, i: bigint) = x.setAttribute(key, Value.BigInt i)
  member x.setAttribute (key: string, boolean: bool) = x.setAttribute(key, Value.Bool boolean)
  member x.setAttribute (key: string, string: string) = x.setAttribute(key, Value.Str string)
  member x.setAttributes (attrs: #seq<SpanAttr>) =
    for KeyValue (k, v) in attrs do _attrs.Add(k, v)
    x

  member x.setStatus (code: SpanCanonicalCode) =
    _status <- (code, None)
    x
  member x.setStatus (code: SpanCanonicalCode, description: string) =
    _status <- (code, Some description)
    x
  member x.setFlags flags =
    _flags <- flags
    x
  member x.sample () =
    _flags <- _flags ||| SpanFlags.Sampled
    x
  member x.debug () =
    x.setDebug true
  member x.setDebug (debug: bool) =
    _flags <-
      if debug then _flags ||| SpanFlags.Sampled ||| SpanFlags.Debug
      else _flags &&& ~~~SpanFlags.Debug
    x

  member x.clearFlags () =
    _flags <- SpanFlags.None
    x

  member x.logThrough () =
    _logThrough <- true
    x

  member x.setKind kind =
    _kind <- kind
    x

  member x.withParent (tId: TraceId, pSId: SpanId, ?flag: SpanFlags) =
    let flag = Option.defaultValue SpanFlags.None flag
    _traceId <- Some tId
    _parentSpanId <- Some pSId
    _flags <- _flags ||| flag
    x

  member x.withParent (context: SpanContext) =
    x.withParent (context.traceId, context.spanId, _flags ||| context.flags)

  member x.withParent (parent: Span) =
    x.withParent (parent.context.traceId, parent.context.spanId, _flags ||| parent.flags)

  member x.followsFromTrace (trace: TraceId) =
    x.addLink (FollowsFromTrace (trace, Map.empty))

  member x.followsFromSpan (context: SpanContext) =
    x.addLink (FollowsFromSpan (context, Map.empty))

  member x.withTransform tx =
    _transform <- tx
    x

  member x.setStarted (ts: EpochNanoSeconds) =
    _started <- ts
    x
  member x.setTimestamp (ts: EpochNanoSeconds) =
    x.setStarted ts
  member x.addEvents (es: #seq<EventMessage>) =
    _events.AddRange es
    x
  member x.start() =
    let context, ambient = createContext ()

    if _enableAmbient then ActiveSpan.setContext (Some context)
    let reset () = if _enableAmbient then ActiveSpan.setContext ambient

    Model.SpanMessage(label, _transform, _started, context, _kind, _links, _attrs, _events, _status, reset)
    :> Span

  member x.startWith(logger: Logger) =
    let context, ambient = createContext ()

    if _enableAmbient then ActiveSpan.setContext (Some context)
    let reset () = if _enableAmbient then ActiveSpan.setContext ambient

    new SpanLoggerImpl(logger, label, _transform, _started, context, _kind, _links,
                       _attrs, _events, _status, reset, _logThrough)
    :> SpanLogger
