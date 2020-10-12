namespace Logary.Model

open System.Diagnostics
open System.Text
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
                 status: SpanStatus option,
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

  let _setStatus (status: SpanStatus option) =
    lock _semaphore <| fun () ->
    _status <- status

  let _setLabel labelFactory =
    lock _semaphore <| fun () ->
    _label <- labelFactory _label

  let _addLink (link: SpanLink) =
    lock _semaphore <| fun () ->
    links.Add link

  let _setFlags (setFlags: SpanFlags -> SpanFlags) =
    lock _semaphore <| fun () ->
    _context <- _context.withFlags (setFlags _context.flags)

  new() =
    SpanMessage("", id, 0L, SpanContext.Zero, SpanKind.Internal, ResizeArray<_>(), Map.empty, ResizeArray<_>(), None, id)

  new(context: SpanContext) =
    SpanMessage("", id, 0L, context, SpanKind.Internal, ResizeArray<_>(), Map.empty, ResizeArray<_>(), None, id)

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
        _context <- _context.withFlags (_context.flags ||| SpanFlags.Sampled)

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
    with get () = _context.flags
     and set v = _context <- _context.withFlags v

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

  // TODO: get hash code for SpanMessage

  // equality
  override x.Equals o =
    let dictEq (d1: IReadOnlyDictionary<_,_>) (d2: IReadOnlyDictionary<_,_>) =
      if d1.Count <> d2.Count then false else
      let mutable stillTrue = true
      use d1e = d1.GetEnumerator()
      while stillTrue && d1e.MoveNext() do
        let (KeyValue (k, v)) = d1e.Current
        match d2.TryGetValue k with
        | false, _ ->
          stillTrue <- false
        | true, v2 ->
          stillTrue <- v = v2
      stillTrue

    let listEq (xs1: IReadOnlyList<_>) (xs2: IReadOnlyList<_>) =
      if xs1.Count <> xs2.Count then false else
      let mutable stillTrue = true
      let mutable i = 0
      while stillTrue && i < xs1.Count do
        stillTrue <- xs1.[i] = xs2.[i]
        i <- i + 1
      stillTrue

    match o with
    | :? Logary.SpanMessage as oM ->
      let xM = x :> Logary.SpanMessage
      let xL = x :> LogaryMessage
      let oL = oM :> LogaryMessage
      oM.label = xM.label
      && oM.context = xM.context
      && oM.kind = xM.kind
      && oM.started = xM.started
      && oM.finished = xM.finished
      && oM.flags = xM.flags
      && oM.status = xM.status
      && listEq oM.links xM.links
      && dictEq oM.fields xM.fields
      && listEq oM.events xM.events
      && oL.id = xL.id
      && oL.name = xL.name
      && oL.level = xL.level
      && oL.received = xL.received
      && oL.timestamp = xL.timestamp
      && dictEq oL.context xL.context
      && dictEq oL.gauges xL.gauges
      && oL.parentSpanId = xL.parentSpanId

    | _ ->
      false


  // pretty printing (for tests primarily)

  override x.ToString() =
    let xM = x :> Logary.SpanMessage
    let xL = x :> LogaryMessage
    let sb = StringBuilder()
    let line (line: string) = sb.AppendLine line |> ignore
    let lineIndent indent prefix (str: string) = line (System.String(' ', indent) + prefix + ": " + str)
    let lineI prefix (str: string) = lineIndent 2 prefix str
    let lineII prefix (str: string) = lineIndent 4 prefix str
    line "Model.SpanMessage"
    line "-----------------"
    line "  context:"
    lineII "traceId" (xM.context.traceId.ToString())
    lineII "spanId" (xM.context.spanId.ToString())
    lineII "parentSpanId" (xM.context.parentSpanId |> Option.map (sprintf "%O") |> Option.defaultValue "-")
    lineII "flags" (xM.context.flags.ToString())
    lineII "traceContext" (xM.context.traceContext.ToString())
    lineII "traceState" (xM.context.traceState.ToString())
    lineI "kind" (xM.kind.ToString())
    lineI "started" (xM.started.ToString())
    lineI "finished" (xM.finished.ToString())
    lineI "flags" (xM.flags.ToString())
    lineI "status" (xM.status |> Option.map (sprintf "%O") |> Option.defaultValue "-")
    lineI "# fields (aka. attrs)" (xM.fields.Count.ToString())
    lineI "# events" (xM.events.Count.ToString())
    lineI "# links" (xM.links.Count.ToString())
    line "  ................"
    line "  :> LogaryMessage"
    lineI "id" (xL.id.ToString())
    lineI "name" (xL.name.ToString())
    lineI "level" (xL.level.ToString())
    lineI "received" (xL.received |> Option.map (sprintf "%O") |> Option.defaultValue "-")
    lineI "timestamp" (xL.timestamp.ToString())
    lineI "# context" (xL.context.Count.ToString())
    lineI "# gauges" (xL.gauges.Count.ToString())
    lineI "parentSpanId" (xL.parentSpanId |> Option.map (sprintf "%O") |> Option.defaultValue "-")
    sb.ToString()

  // interfaces
  interface Logary.SpanMessage with
    member __.label = _label
    member __.context = _context
    member __.kind = _spanKind
    member __.started = base.timestamp
    member __.finished = _finished |> Option.defaultValue 0L
    member __.flags = _context.flags
    member __.links = links :> IReadOnlyList<_>
    member __.events = events :> IReadOnlyList<_>
    member __.attrs = base.fields :> _
    member __.status = _status


  interface SpanOps with
    member x.addLink link = _addLink link
    member x.setAttribute(key: string, value: Value) = x.addAttr (key, value)
    member x.unsetStatus () = _setStatus None
    member x.setStatus (code: SpanStatusCode) = _setStatus (Some (SpanStatus.create code))
    member x.setStatus (code: SpanStatusCode, description: string) = _setStatus (Some (SpanStatus.create(code, description)))
    member x.setStatus (code: SpanStatusCode, description: string, source: SpanStatusSource) = _setStatus (Some (SpanStatus.create(code, description, source)))
    member x.setFlags flags = _setFlags flags
    member x.addEvent m = x.addEvent m
    member x.finish ts = _finish x (Some ts) :> _
    member x.finish () = _finish x None :> _


  interface Span with
    member x.elapsed = x._elapsed()
    member __.isRecording = _finished |> Option.isNone
    member __.setLabel labelFactory = _setLabel labelFactory
    member __.finished = _finished

