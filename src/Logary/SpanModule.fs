namespace Logary

open Logary
open Logary.Message
open System.Threading
open System
open NodaTime

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =
  type private T =
    { traceId: Guid
      spanId: Guid
      parentSpanInfo: SpanInfo option
      messageFac: LogLevel -> Message
      clock: IClock
    }

  let private activeSpan:AsyncLocal<SpanInfo> = new AsyncLocal<SpanInfo> ()

  type SpanBuilder (logger: Logger) =
    let mutable t =
      { traceId = Guid.NewGuid()
        spanId = Guid.NewGuid()
        parentSpanInfo = None
        messageFac = eventX ""
        clock = NodaTime.SystemClock.Instance
      }

    let mutable hasFired = false

    let generateSpanInfo (): SpanInfo =
      // parentSpanInfo can be set explicitly (usually used when combine with outside service, e.g. through http, rpc...)
      match t.parentSpanInfo with
      | None ->
        // fallback to use from AsyncLocal ( ambient context )
        let parent = activeSpan.Value
        if obj.ReferenceEquals(parent, null) then
          { traceId = t.traceId
            parentSpanId = None
            spanId = t.spanId }
        else
          // has ambient span
          { traceId = parent.traceId
            parentSpanId = Some parent.spanId
            spanId = t.spanId }
      | Some parent ->
        { traceId = parent.traceId
          parentSpanId = Some parent.spanId
          spanId = t.spanId }

    member x.withParentSpanInfo info =
      do t <- {t with parentSpanInfo = Some info}
      x

    member x.withMessage fac =
      do t <- {t with messageFac = fac}
      x

    member x.withClock clock =
      do t <- {t with clock = clock}
      x

    member x.start () =
      let spanInfo = generateSpanInfo ()
      let beginAt = t.clock.GetCurrentInstant()

      let previous = activeSpan.Value
      activeSpan.Value <- spanInfo

      { new Span with
          member x.info = spanInfo

          member x.finish (transform: Message -> Message) =
            let composedMsgFac level =
              let endAt = t.clock.GetCurrentInstant()
              let dur = endAt - beginAt
              let spanLog = {
                traceId = SpanInfo.formatId spanInfo.traceId
                spanId =  SpanInfo.formatId spanInfo.spanId
                parentSpanId = spanInfo.parentSpanId |> Option.map SpanInfo.formatId |> Option.defaultValue null
                beginAt = beginAt.ToUnixTimeTicks()
                endAt = endAt.ToUnixTimeTicks()
                duration = dur.BclCompatibleTicks
              }

              (t.messageFac >> transform) level
              |> setContext KnownLiterals.SpanInfoContextName spanLog

            if not hasFired then
              do hasFired <- true
              do activeSpan.Value <- previous
              logger.logWithBP Info composedMsgFac |> Hopac.Hopac.start

          member x.Dispose () =
            x.finish id
      }


  let create logger =
      new SpanBuilder(logger)

  let setClock (clock: IClock) (b: SpanBuilder) =
    b.withClock clock

  let setParentSpanInfo (info: SpanInfo) (b: SpanBuilder) =
    b.withParentSpanInfo info

  let setMessage (fac: LogLevel -> Message) (b: SpanBuilder) =
    b.withMessage fac

  let start (b: SpanBuilder) =
    b.start ()

  let getActiveSpanId () =
    let active = activeSpan.Value
    if obj.ReferenceEquals(active, null) then None
    else Some active.spanId

[<AutoOpen>]
module LoggerSpanEx =
  type Logger with
    member x.buildSpan () =
      Span.create x