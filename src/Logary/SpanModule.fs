namespace Logary

open Logary
open Logary.Message
open Logary.Internals
open System.Threading
open System
open System.Collections.Generic
open System.Globalization
open System.Runtime.CompilerServices

/// Extensions for TraceId, SpanId, Span, etc...
[<AutoOpen>]
module SpanModuleEx =
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
      x.setFlags SpanFlags.Sampled
    member x.debug () =
      x.setFlags SpanFlags.Debug
    member x.clearFlags () =
      x.setFlags SpanFlags.None

/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =
  open NodaTime
  let private activeSpan: AsyncLocal<SpanContext> = new AsyncLocal<SpanContext>()

  [<Struct; RequireQualifiedAccess>]
  type SpanAttrValue =
    | B of boolean: bool
    | V of value: Value
    | S of string: string

  type internal T(inner,
                  lbl: string,
                  transform: Message -> Message,
                  started: EpochNanoSeconds,
                  context: SpanContext,
                  previous: SpanContext,
                  ?kind: SpanKind,
                  ?logThrough: bool) =

    inherit LoggerWrapper(inner)
    let _semaphore = obj ()
    let mutable kind = defaultArg kind SpanKind.Internal
    let mutable logThrough = defaultArg logThrough false
    let mutable _label = lbl
    let mutable _finished = None
    let mutable _flags = context.flags
    let _links = ResizeArray<_>(2)
    let _events = ResizeArray<_>(50)
    let _attrs = ResizeArray<_>(50)
    let mutable _status = SpanCanonicalCode.OK, None

    let _finish x transformMessage =
      lock _semaphore <| fun () ->
      if Option.isSome _finished then () else
      let ts = Global.getTimestamp()
      do _finished <- Some ts

      let spanData = x :> SpanData

      let composedMsgFac level =
        event level _label
        |> transform
        |> transformMessage
        |> setContext KnownLiterals.SpanDataContextName spanData

      do activeSpan.Value <- previous
      inner.logWith Info composedMsgFac

    let _setStatus (code, desc) =
      lock _semaphore <| fun () ->
      _status <- code, desc

    let _setLabel labelFactory =
      lock _semaphore <| fun () ->
      _label <- labelFactory _label

    let _addAttr (k: string, a: SpanAttrValue) =
      lock _semaphore <| fun () ->
      _attrs.Add (k, a)

    let _addLink (link: SpanLink) =
      lock _semaphore <| fun () ->
      _links.Add link

    let _setFlags (flags: SpanFlags) =
      lock _semaphore <| fun () ->
      _flags <- flags

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

      res


    interface SpanData with
      member __.context = context
      member __.kind = kind
      member __.label = _label
      member __.started = started
      member __.finished = _finished |> Option.defaultValue 0L
      member __.elapsed = _elapsed ()

      member __.flags = _flags
      member __.links = _links :> IReadOnlyList<_>
      member __.events = _events :> IReadOnlyList<_>
      member __.status = _status

    interface SpanOps with
      member x.addLink link = _addLink link
      member x.setAttribute (key: string, value: Value) = _addAttr (key, SpanAttrValue.V value)
      member x.setAttribute (key: string, boolean: bool) = _addAttr (key, SpanAttrValue.B boolean)
      member x.setAttribute (key: string, string: string) = _addAttr (key, SpanAttrValue.S string)
      member x.setStatus (code: SpanCanonicalCode) = _setStatus (code, None)
      member x.setStatus (code: SpanCanonicalCode, description: string) = _setStatus (code, Some description)
      member x.finish transform = _finish x transform
      member x.finish () = _finish x id
      member x.setFlags flags = _setFlags flags

    interface Span with
      member __.label = _label
      member __.setLabel labelFactory = _setLabel labelFactory
      member __.finished = _finished

    interface SpanLogger with
      member x.logThrough () = logThrough <- true
      member x.Dispose() = _finish x id

  type SpanBuilder(logger: Logger, label: string, ?traceId: TraceId, ?parentSpanId: SpanId, ?spanId: SpanId, ?enableAmbient: bool) =
    let enableAmbient = defaultArg enableAmbient true
    let mutable transform = id
    let mutable traceId = traceId |> Option.defaultWith TraceId.create
    let mutable spanId = spanId |> Option.defaultWith SpanId.create
    let mutable parentSpanId = parentSpanId
    let mutable flags = SpanFlags.None

    /// If the explicit `parentSpanId` is None, and we allow ambient spans and there exists an ambient span,
    /// then fallback to use it, from AsyncLocal
    let generateSpanInfo (): SpanContext =
      let parent = activeSpan.Value
      if Option.isNone parentSpanId && enableAmbient && not (obj.ReferenceEquals(parent, null)) then
        SpanContext(parent.traceId, spanId, flags ||| parent.flags, parent.spanId)
      else
        SpanContext(traceId, spanId, flags, ?parentSpanId=parentSpanId)

    member x.withParent (tId: TraceId, pSId: SpanId, ?flag: SpanFlags) =
      traceId <- tId
      parentSpanId <- Some pSId
      flags <- flags ||| Option.defaultValue SpanFlags.None flag
      x
    member x.withParent (context: SpanContext) =
      x.withParent (context.traceId, context.spanId, flags ||| context.flags)
    member x.withParent (parent: Span) =
      x.withParent (parent.context.traceId, parent.context.spanId, flags ||| parent.flags)

    member x.withTransform tx =
      transform <- tx
      x

    member x.start() =
      let startAt = Global.getTimestamp ()
      let spanInfo = generateSpanInfo ()

      let previous = activeSpan.Value
      activeSpan.Value <- spanInfo

      new T(logger, label, transform, startAt, spanInfo, previous) :> SpanLogger

  let create logger label = new SpanBuilder(logger, label)
  let withParentContext (info: SpanContext) (b: SpanBuilder) = b.withParent info
  let withParentSpan (span: Span) (b: SpanBuilder) = b.withParent span
  let withParentValues (tId: TraceId, pSId: SpanId) (b: SpanBuilder) = b.withParent(tId, pSId)
  let withTransform (transform: Message -> Message) (b: SpanBuilder) = b.withTransform transform
  let start (b: SpanBuilder) = b.start ()

  let getActiveSpanId () =
    let active = activeSpan.Value
    if obj.ReferenceEquals(active, null) then None
    else Some active.spanId

[<AutoOpen; Extension>]
module LoggerSpanEx =
  type Logger with
    member x.buildSpan (label: string, ?transform: Message -> Message) =
      Span.create x label
        |> Span.withTransform (Option.defaultValue id transform)

    member x.startSpan (label: string, ?transform: Message -> Message) =
      x.buildSpan (label, ?transform=transform)
        |> Span.start

    member x.startSpan (label: string, parent: SpanContext) =
      x.buildSpan label
        |> Span.withParentContext parent
        |> Span.start

    member x.startSpan (label: string, parent: Span) =
      x.startSpan(label, parent.context)
