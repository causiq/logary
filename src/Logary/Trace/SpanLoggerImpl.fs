namespace Logary.Trace

open Logary
open Logary.Internals
open Hopac
open System.Collections.Generic

/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span
type internal SpanLoggerImpl(logger: Logger,
                             label: string,
                             transform: Model.SpanMessage -> Model.SpanMessage,
                             started: EpochNanoSeconds,
                             context: SpanContext,
                             kind: SpanKind,
                             links: ResizeArray<_>,
                             attrs: Dictionary<_,_>,
                             events: ResizeArray<_>,
                             status: SpanCanonicalCode * string option,
                             onFinish: unit -> unit,
                             logThrough: bool) =

  inherit LoggerWrapper(logger)

  let mutable _logThrough = logThrough

  let spanModel = Model.SpanMessage(label, transform, started, context, kind, links, attrs, events, status, onFinish, received=started)
  let span = spanModel :> Span

  override x.logWithAck (waitForBuffers, message) =
    message.tryGetAs<EventMessage>() |> Option.iter span.addEvent

    if _logThrough then base.logWithAck(waitForBuffers, message)
    else LogResult.success


  interface Logary.LogaryMessage with
    member x.kind = (span :> LogaryMessage).kind
    member x.id = span.id
    member x.spanId = span.spanId
    member x.name = span.name
    member x.level = span.level
    member x.timestamp = span.timestamp
    member x.received = span.received
    member x.context = (span :> LogaryMessage).context
    member x.fields = span.fields
    member x.gauges = span.gauges

  interface Logary.SpanMessage with
    member __.label = span.label
    member __.context = span.context
    member __.kind = span.kind
    member __.started = span.started
    member __.finished = (span :> SpanMessage).finished
    member __.flags = span.flags
    member __.links = span.links
    member __.events = span.events
    member __.attrs = span.attrs
    member __.status = span.status


  interface Logary.Trace.SpanOps with
    member x.addLink link = span.addLink link
    member x.setAttribute (key: string, value: Value) = span.setAttribute(key, value)
    member x.setStatus (code: SpanCanonicalCode) = span.setStatus code
    member x.setStatus (code: SpanCanonicalCode, description: string) = span.setStatus(code, description)
    member x.setFlags flags = span.setFlags flags
    member x.addEvent m = span.addEvent m
    member x.finish (ts: EpochNanoSeconds) = span.finish ts
    member x.finish () = span.finish()


  interface Logary.Trace.Span with
    member __.elapsed = span.elapsed
    member __.isRecording = span.isRecording
    member __.setLabel labelFactory = span.setLabel labelFactory
    member __.finished = span.finished


  interface Logary.Trace.SpanOpsAdvanced with
    member x.finish transform =
      let message = spanModel.finish transform
      queueIgnore (x.logWithAck(false, message))
      message :> _

  interface Logary.Trace.SpanLogger with
    member x.logThrough () = _logThrough <- true

    member __.finishWithAck (transform: Model.SpanMessage -> unit) =
      let message = spanModel.finish transform
      logger.logWithAck (false, message)

    member x.finishWithAck (ts: EpochNanoSeconds) =
      let message = span.finish ts
      logger.logWithAck (false, message)

    member x.finishWithAck () =
      let message = span.finish()
      logger.logWithAck(false, message)

    member x.Dispose() =
      let message = if Option.isNone span.finished then span.finish() else x :> Logary.SpanMessage
      queueIgnore (logger.logWithAck (false, message))
