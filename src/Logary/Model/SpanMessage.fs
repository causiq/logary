namespace Logary.Model

open System.Diagnostics
open Logary
open NodaTime
open Logary.Internals
open Logary.Trace
open System.Threading
open System.Collections.Generic

/// Once you pass mutable reference types into this message c'tor, those references are owned by the SpanMessage and
/// should not be mutated from the outside.
[<Sealed>]
[<DebuggerDisplay("{debuggerDisplay,nq}")>]
type SpanMessage(label: string,
                 transform: SpanMessage -> SpanMessage,
                 started: EpochNanoSeconds,
                 spanContext: SpanContext,
                 spanKind: SpanKind,
                 links: IList<_>,
                 attrs: IReadOnlyDictionary<string, Value>,
                 events: IList<EventMessage>,
                 status: SpanCanonicalCode * string option,
                 onFinish: unit -> unit,
                 ?messageId,
                 ?name,
                 ?level,
                 ?ctx,
                 ?gauges,
                 ?received) =

  inherit LogaryMessageBase(MessageKind.Span, started, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=Some attrs, ?gauges=gauges, ?received=received)

  let links = ResizeArray<_>(links)
  let events = ResizeArray<_>(events)

  let mutable _context = spanContext
  let mutable _spanKind = spanKind
  let mutable _label = label
  let mutable _finished = None
  let mutable _flags = _context.flags
  let mutable _status = status
  let mutable _highestLevel = Info
  let _semaphore = obj ()

  let _finish x tsFin: SpanMessage =
    if Option.isSome _finished then
      x
    else
      lock _semaphore <| fun () ->
      let ts =
        match tsFin with
        | None ->
          Global.getTimestamp ()
        | Some ts ->
          ts

      do _finished <- Some ts
      do onFinish()

      let res = transform x
      res

  let _setStatus (code, desc) =
    lock _semaphore <| fun () ->
    _status <- code, desc

  let _setLabel labelFactory =
    lock _semaphore <| fun () ->
    _label <- labelFactory _label

  let _addLink (link: SpanLink) =
    lock _semaphore <| fun () ->
    links.Add link

  let _setFlags (setFlags: SpanFlags -> SpanFlags) =
    lock _semaphore <| fun () ->
    _flags <- setFlags _flags

  new() =
    SpanMessage("", id, 0L, SpanContext.Zero, SpanKind.Internal, ResizeArray<_>(), Map.empty, ResizeArray<_>(), (SpanCanonicalCode.OK, None), id)

  new(context: SpanContext) =
    SpanMessage("", id, 0L, context, SpanKind.Internal, ResizeArray<_>(), Map.empty, ResizeArray<_>(), (SpanCanonicalCode.OK, None), id)

  new(m: Logary.SpanMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>() in let attrs = Dictionary<_,_>()
    let prevCtx = (m :> LogaryMessage).context
    prevCtx |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    m.attrs  |> Seq.iter (fun (KeyValue (k, v)) -> attrs.Add(k, v))
    let links = List<_> m.links
    let events = List<_> m.events
    SpanMessage(m.label, id, m.started, m.context, m.kind, links, attrs, events, m.status, id, m.id, m.name, m.level, ctx, gauges)

  member private x._elapsed () =
    let delta =
      match _finished with
      | None ->
        Global.getTimestamp() - base.timestamp
      | Some finished ->
        finished - base.timestamp
    Duration.FromNanoseconds delta

  member private x.addAttr (k: string, a: Value) =
    Monitor.Enter _semaphore
    try base.fields.Add (k, a)
    finally Monitor.Exit _semaphore

  member private x.addEvent (message: EventMessage) =
    Monitor.Enter(_semaphore)
    try
      // add to _events
      events.Add message

      // ensure we set the sampled flag if level >= Warn
      if message.level >= Warn then
        _flags <- _flags ||| SpanFlags.Sampled

      // set the error flag when errors are logged
      if message.level >= Error then
        base.fields.["error"] <- Value.Bool true

      if message.level > _highestLevel then
        _highestLevel <- message.level
    finally
      Monitor.Exit(_semaphore)

  member x.finish(runAfterFinish: SpanMessage -> unit) =
    _finish x None |> runAfterFinish
    x

  member x.context
    with get() = _context
     and set v = _context <- v

  member x.label
    with get () = _label
     and set v = _label <- v

  member x.flags
    with get () = _flags
     and set v = _flags <- v

  member x.spanKind
    with get () = _spanKind
     and set v = _spanKind <- v

  member x.started
    with get () = base.timestamp
     and set v = base.timestamp <- v

  member x.finished
    with get () = _finished
     and set v = _finished <- v

  member x.status
    with get () = _status
     and set v = _status <- v

  member internal x.debuggerDisplay
    with get() =
      let formatField (KeyValue (s, v)) = sprintf "%s=%O" s v
      sprintf "%s %O finished=%O flags=%O kind=%O id=%O with %i events and fields={ %s } (M.SpanMessage)"
        x.label (x.elapsed.toGauge()) x.finished x.flags x.spanKind x.context.spanId events.Count
        (x.fields |> Seq.map formatField |> String.concat ", ")

  override x.level
    with get() = _highestLevel
     and set v = _highestLevel <- v

  override x.parentSpanId
    with get() = _context.parentSpanId
     and set v = v |> Option.iter (fun newParent -> _context <- _context.withParentId (Some newParent))

  member x.setEvents (es: IReadOnlyList<_>) =
    Monitor.Enter _semaphore
    try
      events.Clear()
      events.AddRange es
    finally
      Monitor.Exit _semaphore

  member x.setLinks (ls: IReadOnlyList<_>) =
    Monitor.Enter _semaphore
    try
      links.Clear()
      links.AddRange ls
    finally
      Monitor.Exit _semaphore

  member x.setAttributes (attrs: _) =
    Monitor.Enter _semaphore
    try
      for (KeyValue (k, v)) in attrs do
        base.fields.[k] <- v
    finally
      Monitor.Exit _semaphore

  member x.writeCopy (cb: SpanMessage -> unit): SpanMessage =
    let m = SpanMessage x
    cb m
    m

  override x.cloneAndUpdate builder =
    x.writeCopy builder :> LogaryMessageBase

  interface Logary.SpanMessage with
    member __.label = _label
    member __.context = _context
    member __.kind = spanKind
    member __.started = base.timestamp
    member __.finished = _finished |> Option.defaultValue 0L
    member __.flags = _flags
    member __.links = links :> IReadOnlyList<_>
    member __.events = events :> IReadOnlyList<_>
    member __.attrs = base.fields :> _
    member __.status = _status


  interface SpanOps with
    member x.addLink link = _addLink link
    member x.setAttribute(key: string, value: Value) = x.addAttr (key, value)
    member x.setStatus (code: SpanCanonicalCode) = _setStatus (code, None)
    member x.setStatus (code: SpanCanonicalCode, description: string) = _setStatus (code, Some description)
    member x.setFlags flags = _setFlags flags
    member x.addEvent m = x.addEvent m
    member x.finish ts = _finish x (Some ts) :> _
    member x.finish () = _finish x None :> _


  interface Span with
    member x.elapsed = x._elapsed()
    member __.isRecording = _finished |> Option.isNone
    member __.setLabel labelFactory = _setLabel labelFactory
    member __.finished = _finished

