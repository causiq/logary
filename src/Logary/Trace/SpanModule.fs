namespace Logary.Trace

open Logary
open Logary.Message
open Logary.Internals
open NodaTime
open System.Threading
open System
open System.Collections.Generic
open System.Globalization
open System.Runtime.CompilerServices

[<AutoOpen>]
module Extensions =
  type TraceId with
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

  type SpanData with
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

  /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span
  type internal T(inner: Logger,
                  label: string,
                  transform: Message -> Message,
                  started: EpochNanoSeconds,
                  context: SpanContext,
                  kind: SpanKind,
                  // once passed, is owned by T
                  links: ResizeArray<_>,
                  // once passed, is owned by T
                  attrs: ResizeArray<_>,
                  status: SpanCanonicalCode * string option,
                  onFinish: unit -> unit,
                  logThrough: bool) =
    inherit LoggerWrapper(inner)

    let mutable kind = kind
    let mutable logThrough = logThrough
    let mutable _label = label
    let mutable _finished = None
    let mutable _flags = context.flags
    let mutable _status = status
    let _semaphore = obj ()
    let _events = ResizeArray<_>(50)

    let _finish x transformMessage =
      lock _semaphore <| fun () ->
      if Option.isSome _finished then () else
      let ts = Global.getTimestamp()

      do _finished <- Some ts
      do onFinish()

      let spanData = x :> SpanData

      let composedMsgFac level =
        event level _label
        |> transform
        |> transformMessage
        |> setContext KnownLiterals.SpanDataContextName spanData

      inner.logWith Info composedMsgFac

    let _setStatus (code, desc) =
      lock _semaphore <| fun () ->
      _status <- code, desc

    let _setLabel labelFactory =
      lock _semaphore <| fun () ->
      _label <- labelFactory _label

    let _addAttr (k: string, a: SpanAttrValue) =
      lock _semaphore <| fun () ->
      attrs.Add (k, a)

    let _addLink (link: SpanLink) =
      lock _semaphore <| fun () ->
      links.Add link

    let _setFlags (setFlags: SpanFlags -> SpanFlags) =
      lock _semaphore <| fun () ->
      _flags <- setFlags _flags

    let _elapsed () =
      Duration.FromNanoseconds(
        match _finished with
        | None -> Global.getTimestamp() - started
        | Some finished -> finished - started)

    override x.logWithAck (waitForBuffers, logLevel) messageFactory =
      let message = messageFactory logLevel

      let res =
        // log through to the other targets if needed
        if logThrough then
          inner.logWithAck (waitForBuffers, logLevel) (fun _ -> message)
        else
          LogResult.success

      lock _semaphore <| fun () ->
      // add to _events
      _events.Add message

      // ensure we set the sampled flag if level >= Warn
      if logLevel >= Warn then
        _flags <- _flags ||| SpanFlags.Sampled

      // set the error flag when errors are logged
      if logLevel >= Error then
        attrs.Add ("error", SpanAttrValue.B true)

      res

    interface SpanData with
      member __.context = context
      member __.kind = kind
      member __.label = _label
      member __.started = started
      member __.finished = _finished |> Option.defaultValue 0L
      member __.elapsed = _elapsed ()
      member __.flags = _flags
      member __.links = links :> IReadOnlyList<_>
      member __.events = _events :> IReadOnlyList<_>
      member __.status = _status

    interface SpanOps with
      member x.addLink link = _addLink link
      member x.setAttribute (key: string, value: Value) = _addAttr (key, SpanAttrValue.V value)
      member x.setAttribute (key: string, boolean: bool) = _addAttr (key, SpanAttrValue.B boolean)
      member x.setAttribute (key: string, string: string) = _addAttr (key, SpanAttrValue.S string)
      member x.setStatus (code: SpanCanonicalCode) = _setStatus (code, None)
      member x.setStatus (code: SpanCanonicalCode, description: string) = _setStatus (code, Some description)
      member x.setFlags flags = _setFlags flags
      member x.finish transform = _finish x transform
      member x.finish () = _finish x id

    interface Span with
      member __.label = _label
      member __.setLabel labelFactory = _setLabel labelFactory
      member __.finished = _finished

    interface SpanLogger with
      member x.logThrough () = logThrough <- true
      member x.Dispose() = _finish x id

type SpanBuilder(logger: Logger, label: string) =
  let mutable _started = Global.getTimestamp()
  let mutable _enableAmbient = false
  let mutable _transform = id
  let mutable _traceId = None
  let mutable _spanId = None
  let mutable _parentSpanId = None
  let mutable _kind = SpanKind.Internal
  let mutable _flags = SpanFlags.None
  let _links = ResizeArray<_>(2)
  let _attrs = ResizeArray<_>(50)
  let mutable _status = SpanCanonicalCode.OK, None
  let mutable _logThrough = false

  let createContext (): SpanContext * SpanContext =
    let ambient = SpanBuilder.ActiveSpan.Value
    let spanId = _spanId |> Option.defaultWith SpanId.create
    if Option.isNone _parentSpanId && _enableAmbient && not (obj.ReferenceEquals(ambient, null)) then
      SpanContext(ambient.traceId, spanId, _flags ||| ambient.flags, ambient.spanId),
      ambient
    else
      let traceId = _traceId |> Option.defaultWith TraceId.create
      SpanContext(traceId, spanId, _flags, ?parentSpanId=_parentSpanId),
      ambient

  static member internal ActiveSpan: AsyncLocal<SpanContext> = new AsyncLocal<SpanContext>()

  /// WARNING: Enabling "ambient" support is only valid if you ONLY use `System.Threading.Tasks.Task` and their
  /// derivatives.
  ///
  /// However, F#'s async doesn't use `AsyncLocal` and neither does Hopac. It is the context that calls `start()`
  /// whose context will be inherited.
  member x.enableAmbient () =
    _enableAmbient <- true
    x

  member x.addLink link =
    _links.Add link
    x
  member x.setAttribute (key: string, value: Value) =
    _attrs.Add (key, SpanAttrValue.V value)
    x
  member x.setAttribute (key: string, boolean: bool) =
    _attrs.Add (key, SpanAttrValue.B boolean)
    x
  member x.setAttribute (key: string, string: string) =
    _attrs.Add (key, SpanAttrValue.S string)
    x
  member x.setAttributes (attrs: #seq<SpanAttr>) =
    _attrs.AddRange attrs
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
    x.addLink (FollowsFromTrace (trace, []))

  member x.followsFromSpan (context: SpanContext) =
    x.addLink (FollowsFromSpan (context, []))

  member x.withTransform tx =
    _transform <- tx
    x

  member x.setStarted (ts: EpochNanoSeconds) =
    _started <- ts
    x
  member x.setTimestamp (ts: EpochNanoSeconds) =
    x.setStarted ts

  member x.start() =
    let context, ambient = createContext ()

    if _enableAmbient then SpanBuilder.ActiveSpan.Value <- context
    let reset () = if _enableAmbient then SpanBuilder.ActiveSpan.Value <- ambient

    let attrs, links = ResizeArray<_>(_attrs), ResizeArray<_>(_links)
    new T(logger, label, _transform, _started, context, _kind, links, attrs, _status, reset, _logThrough)
    :> SpanLogger

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =
  /// Use the instance methods on the returned object
  let create logger label = new SpanBuilder(logger, label)
  let start (x: SpanBuilder) = x.start()

module ActiveSpan =
  let getContext () =
    let active = SpanBuilder.ActiveSpan.Value
    if obj.ReferenceEquals(active, null) then None
    else Some active

  let getSpan () =
    getContext () |> Option.map (fun ctx -> ctx.spanId)

[<AutoOpen; Extension>]
module LoggerEx =
  type Logger with
    member x.buildSpan (label: string, ?parent: SpanContext, ?transform: Message -> Message) =
      let builder = new SpanBuilder(x, label)
      parent |> Option.iter (builder.withParent >> ignore)
      builder.withTransform(Option.defaultValue id transform)

    member x.buildSpan (label: string, parent: SpanData, ?transform: Message -> Message) =
      x.buildSpan (label, parent.context, ?transform=transform)

    member x.startSpan (label: string, ?parent: SpanContext, ?transform: Message -> Message) =
      x.buildSpan(label, ?parent=parent, ?transform=transform).start()

    member x.startSpan (label: string, parent: SpanData, ?transform: Message -> Message) =
      x.startSpan (label, parent.context, ?transform=transform)