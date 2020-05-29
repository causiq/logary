namespace Logary.Trace

open Logary
open Logary.Message
open Logary.Internals
open Logary.Trace.Propagation
open NodaTime
open Hopac
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
    member x.tryGet key =
      match x.attrs.TryGetValue key with
      | false, _ -> None
      | true, value -> Some value
    member x.tryGetString key =
      match x.tryGet key with
      | Some (SpanAttrValue.S value) -> Some value
      | _ -> None
    member x.tryGetBool key =
      match x.tryGet key with
      | Some (SpanAttrValue.B value) -> Some value
      | _ -> None
    member x.tryGetValue key =
      match x.tryGet key with
      | Some (SpanAttrValue.V v) -> Some v
      | _ -> None
    member x.tryGetFloat key =
      match x.tryGetValue key with
      | Some v -> Some (v.toFloat())
      | _ -> None
    member x.tryGetInt key =
      match x.tryGetValue key with
      | Some (Int64 i) -> Some i
      | Some v -> Some (int64 (v.toFloat()))
      | _ -> None
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

/// This is a DTO for Span.
type SpanImpl(label: string,
              transform: Message -> Message,
              started: EpochNanoSeconds,
              context: SpanContext,
              kind: SpanKind,
              // once passed, is owned by SpanImpl
              links: ResizeArray<_>,
              // once passed, is owned by SpanImpl
              attrs: ResizeArray<_>,
              events: ResizeArray<_>,
              status: SpanCanonicalCode * string option,
              onFinish: unit -> unit) =

  let mutable kind = kind
  let mutable _label = label
  let mutable _finished = None
  let mutable _flags = context.flags
  let mutable _status = status
  let mutable _highestLevel = Info
  let _semaphore = obj ()
  let alreadyFinished = Message.eventFormat("Span={label} already finished", label) |> Message.setLevel Error

  let _finish x ts transformMessage =
    lock _semaphore <| fun () ->
    if Option.isSome _finished then false, alreadyFinished else
    let ts = ts |> Option.defaultWith Global.getTimestamp

    do _finished <- Some ts
    do onFinish()

    let spanData = x :> SpanData

    true,
    event _highestLevel _label
    |> transform
    |> transformMessage
    |> setContext KnownLiterals.SpanDataContextName spanData

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

  let _attrsAsDic = lazy (attrs |> Map.ofSeq :> IReadOnlyDictionary<_, _>)
  let _calcAttrs () =
    if Option.isNone _finished then
      attrs |> Map.ofSeq :> IReadOnlyDictionary<_, _>
    else
      _attrsAsDic.Value

  let _addEvent message =
    lock _semaphore <| fun () ->
    // add to _events
    events.Add message

    // ensure we set the sampled flag if level >= Warn
    if message.level >= Warn then
      _flags <- _flags ||| SpanFlags.Sampled

    // set the error flag when errors are logged
    if message.level >= Error then
      attrs.Add ("error", SpanAttrValue.B true)

    if message.level > _highestLevel then
      _highestLevel <- message.level


  interface SpanData with
    member __.context = context
    member __.kind = kind
    member __.label = _label
    member __.started = started
    member __.finished = _finished |> Option.defaultValue 0L
    member __.elapsed = _elapsed ()
    member __.isRecording = _finished |> Option.isNone
    member __.flags = _flags
    member __.links = links :> IReadOnlyList<_>
    member __.events = events :> IReadOnlyList<_>
    member __.attrs = _calcAttrs ()
    member __.status = _status


  interface SpanOps with
    member x.addLink link = _addLink link
    member x.setAttribute (key: string, value: Value) = _addAttr (key, SpanAttrValue.V value)
    member x.setAttribute (key: string, boolean: bool) = _addAttr (key, SpanAttrValue.B boolean)
    member x.setAttribute (key: string, string: string) = _addAttr (key, SpanAttrValue.S string)
    member x.setStatus (code: SpanCanonicalCode) = _setStatus (code, None)
    member x.setStatus (code: SpanCanonicalCode, description: string) = _setStatus (code, Some description)
    member x.setFlags flags = _setFlags flags
    member x.addEvent m = _addEvent m
    member x.finish transform = snd (_finish x None transform)
    member x.finish ts = snd (_finish x (Some ts) id)
    member x.finish () = snd (_finish x None id)


  interface Span with
    member __.label = _label
    member __.setLabel labelFactory = _setLabel labelFactory
    member __.finished = _finished

/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span
type internal T(logger: Logger,
                label: string,
                transform: Message -> Message,
                started: EpochNanoSeconds,
                context: SpanContext,
                kind: SpanKind,
                // once passed, is owned by T
                links: ResizeArray<_>,
                // once passed, is owned by T
                attrs: ResizeArray<_>,
                // once passed, is owned by T
                events: ResizeArray<_>,
                status: SpanCanonicalCode * string option,
                onFinish: unit -> unit,
                logThrough: bool) =
  inherit LoggerWrapper(logger)

  let mutable _logThrough = logThrough

  let span = SpanImpl(label, transform, started, context, kind, links, attrs, events, status, onFinish) :> Span

  override x.logWithAck (waitForBuffers, logLevel) messageFactory =
    let message = messageFactory logLevel

    span.addEvent message

    if _logThrough then
      logger.logWithAck (waitForBuffers, message.level) (fun _ -> message)
    else
      LogResult.success

  interface SpanData with
    member __.context = span.context
    member __.kind = span.kind
    member __.label = span.label
    member __.started = span.started
    member __.finished = (span :> SpanData).finished
    member __.elapsed = span.elapsed
    member __.isRecording = span.isRecording
    member __.flags = span.flags
    member __.links = span.links
    member __.events = span.events
    member __.attrs = span.attrs
    member __.status = span.status


  interface SpanOps with
    member x.addLink link = span.addLink link
    member x.setAttribute (key: string, value: Value) = span.setAttribute(key, value)
    member x.setAttribute (key: string, boolean: bool) = span.setAttribute(key, boolean)
    member x.setAttribute (key: string, string: string) = span.setAttribute(key, string)
    member x.setStatus (code: SpanCanonicalCode) = span.setStatus code
    member x.setStatus (code: SpanCanonicalCode, description: string) = span.setStatus(code, description)
    member x.setFlags flags = span.setFlags flags
    member x.addEvent m = span.addEvent m
    member x.finish (transform: _ -> _) = span.finish transform
    member x.finish (ts: EpochNanoSeconds) = span.finish ts
    member x.finish () = span.finish()


  interface Span with
    member __.label = span.label
    member __.setLabel labelFactory = span.setLabel labelFactory
    member __.finished = span.finished


  interface SpanOpsAdvanced with
    member __.finishWithAck (transform: _ -> _) =
      let message = span.finish(transform)
      logger.logWithAck (false, message.level) (fun _ -> message)

    member x.finishWithAck (ts: EpochNanoSeconds) =
      let message = span.finish(ts)
      logger.logWithAck (false, message.level) (fun _ -> message)

  interface SpanLogger with
    member x.logThrough () = _logThrough <- true
    member x.Dispose() =
      let message = span.finish()
      queueIgnore (logger.logWithAck (false, message.level) (fun _ -> message))

module ActiveSpan =
  let private asyncLocal: AsyncLocal<SpanContext> = new AsyncLocal<SpanContext>()

  let getContext () =
    let active = asyncLocal.Value
    if obj.ReferenceEquals(active, null) then None
    else Some active

  let setContext (ctx: SpanContext option) =
    asyncLocal.Value <- match ctx with Some ctx -> ctx | _ -> Unchecked.defaultof<_>

  let getSpan () =
    getContext () |> Option.map (fun ctx -> ctx.spanId)

type SpanBuilder(label: string) =
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
  let _events = ResizeArray<Message>()

  let createContext (): SpanContext * SpanContext option =
    let spanId = _spanId |> Option.defaultWith SpanId.create
    match ActiveSpan.getContext () with
    | Some ambient when _parentSpanId.IsNone && _enableAmbient ->
      //printfn "createContext() => has ambient"
      SpanContext(ambient.traceId, spanId, _flags ||| ambient.flags, ambient.spanId),
      Some ambient
    | _ ->
      //printfn "createContext() => has no ambient"
      let traceId = _traceId |> Option.defaultWith TraceId.create
      SpanContext(traceId, spanId, _flags, ?parentSpanId=_parentSpanId),
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
  member x.addEvents (es: #seq<Message>) =
    _events.AddRange es
    x
  member x.start() =
    let context, ambient = createContext ()

    if _enableAmbient then ActiveSpan.setContext (Some context)
    let reset () = if _enableAmbient then ActiveSpan.setContext ambient

    SpanImpl(label, _transform, _started, context, _kind, _links, _attrs, _events, _status, reset)
    :> Span

  member x.startWith(logger: Logger) =
    let context, ambient = createContext ()

    if _enableAmbient then ActiveSpan.setContext (Some context)
    let reset () = if _enableAmbient then ActiveSpan.setContext ambient

    new T(logger, label, _transform, _started, context, _kind, _links, _attrs, _events, _status, reset, _logThrough)
    :> SpanLogger


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =
  /// Use the instance methods on the returned object
  let create label = SpanBuilder(label)
  let start (x: SpanBuilder) = x.start()
  let startWith (x: SpanBuilder) logger = x.startWith logger

[<AutoOpen>]
module LoggerEx =
  type Logger with
    member x.buildSpan (label: string, ?parent: SpanContext, ?enhance: SpanBuilder -> SpanBuilder, ?transform: Message -> Message, ?enableAmbient: bool) =
      let builder =
        SpanBuilder(label)
          |> Option.defaultValue id enhance

      parent |> Option.iter (builder.withParent >> ignore)

      builder
        .withTransform(Option.defaultValue id transform)
        .setAmbientEnabled(defaultArg enableAmbient false)

    member x.buildSpan (label, parent: SpanData, ?enhance, ?transform, ?enableAmbient) =
      x.buildSpan (label, parent.context, ?enhance=enhance, ?transform=transform, ?enableAmbient=enableAmbient)

    member x.startSpan (label, ?parent: SpanData, ?enhance, ?transform: Message -> Message, ?enableAmbient: bool) =
      let parentO = parent |> Option.map (fun p -> p.context)
      let builder = x.buildSpan(label, ?parent=parentO, ?enhance=enhance, ?transform=transform, ?enableAmbient=enableAmbient)
      builder.startWith x

[<AutoOpen>]
module SpanLoggerEx =
  type SpanLogger with
    member x.startChild (label: string, ?enhance: SpanBuilder -> SpanBuilder, ?transform: Message -> Message, ?enableAmbient: bool) =
      let enhance = enhance |> Option.defaultValue (fun b -> b.setKind SpanKind.Client)
      let logger = x :> Logger
      let builder = logger.buildSpan(label, x.context, enhance, ?transform=transform, ?enableAmbient=enableAmbient)
      builder.start()

    member x.inject (propagator: Propagator, setter: Setter<'t>, target: 't): 't =
      propagator.inject (setter, x.context, target)

    member x.injectWith (propagator: Propagator, setter: Setter<'t>): 't -> 't =
      fun t ->
        propagator.inject(setter, x.context, t)

[<AutoOpen>]
module SpanOpsAdvancedEx =
  open Hopac.Infixes
  type SpanOpsAdvanced with
    member x.finishAck (ts: EpochNanoSeconds) =
      x.finishWithAck(ts)
      >>=* function | Ok ack -> ack | _ -> Promise.unit
    member x.finishAck() =
      x.finishWithAck id
      >>=* function | Ok ack -> ack | _ -> Promise.unit
