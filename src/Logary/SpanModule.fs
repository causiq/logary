namespace Logary

open Logary
open Logary.Message
open Logary.Internals
open System.Threading
open System
open System.Globalization
open System.Runtime.CompilerServices

[<AutoOpen>]
module SpanModuleEx =
  open System.Text.RegularExpressions

  let private m = new Regex("")
  type TraceId with
    static member create (?high, ?low) =
      { high = high |> Option.defaultWith Rnd.nextInt64NonZero
        low = low |> Option.defaultWith Rnd.nextInt64NonZero }

    static member ofGuid (g: Guid) =
      let bs = g.ToByteArray()
      { high = BitConverter.ToInt64(bs, 0)
        low = BitConverter.ToInt64(bs, 8) }

    static member ofString (s: string) =
      if s.Length > 32 then
        Result.Error(sprintf "Could not parse string more than 32 chars into TraceId: %s" s)
      else
        let highLen = max 0 (s.Length - 16)
        let high = // "".Substring(0, 0) => ""
          match Int64.TryParse(s.Substring(0, highLen), NumberStyles.HexNumber, null) with
          | false, _ -> 0L
          | true, high -> high
        let low =
          match Int64.TryParse(s.Substring highLen, NumberStyles.HexNumber, null) with
          | false, _ -> 0L
          | true, low -> low
        Ok(TraceId.create (high, low))

  type SpanId with
    static member create (?value: int64) =
      { id = value |> Option.defaultWith Rnd.nextInt64NonZero }
    static member ofTraceId (tId: TraceId) =
      SpanId.create tId.low
    static member ofString (s: string) =
      match Int64.TryParse(s, NumberStyles.HexNumber, null) with
      | false, _ -> SpanId.create 0L
      | true, v -> SpanId.create v


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =
  type private T =
    { traceId: TraceId
      spanId: SpanId
      parentSpanId: SpanId option
      transform: Message -> Message
      flags: int32
    }

  let private activeSpan: AsyncLocal<SpanInfo> = new AsyncLocal<SpanInfo>()

  type SpanBuilder(logger: Logger, label: string, ?traceId: TraceId, ?parentSpanId: SpanId, ?spanId: SpanId) =
    let messageFactory = eventX label
    let mutable t =
      { traceId = traceId |> Option.defaultWith TraceId.create
        spanId = spanId |> Option.defaultWith SpanId.create
        parentSpanId = parentSpanId
        transform = id
        flags = 0
      }

    let mutable isFinished = false

    let generateSpanInfo(): SpanInfo =
      // parentSpanInfo can be set explicitly (usually used when combine with outside service, e.g. through http, rpc...)
      match t.parentSpanId with
      | None ->
        // fallback to use from AsyncLocal ( ambient context )
        let parent = activeSpan.Value
        if obj.ReferenceEquals(parent, null) then
          { traceId = t.traceId
            parentSpanId = None
            spanId = t.spanId
            flags = 0 }
        else
          // has ambient span
          { traceId = parent.traceId
            parentSpanId = Some parent.spanId
            spanId = t.spanId
            flags = 0 }
      | Some parent ->
        { traceId = t.traceId
          parentSpanId = Some parent
          spanId = t.spanId
          flags = 0
        }

    member x.withParentSpanInfo (info: SpanInfo) =
      do t <- { t with parentSpanId = Some info.spanId
                       traceId = info.traceId
                       flags = info.flags }
      x
    member x.withParent (traceId: TraceId, parentSpanId: SpanId) =
      do t <- { t with parentSpanId = Some parentSpanId
                       traceId = traceId }
      x


    member x.withTransform transform =
      do t <- { t with transform = transform }
      x

    member x.start() =
      let spanInfo = generateSpanInfo ()
      let startAt = Global.getTimestamp ()

      let previous = activeSpan.Value
      activeSpan.Value <- spanInfo

      { new Span with
          member x.info = spanInfo

          member x.finish (transform: Message -> Message) =
            if isFinished then () else
            do isFinished <- true
            let finishAt = Global.getTimestamp()
            let composedMsgFac level =
              let spanLog = {
                traceId = spanInfo.traceId
                spanId = spanInfo.spanId
                parentSpanId = spanInfo.parentSpanId
                beginAt = startAt
                endAt = finishAt
                duration = finishAt - startAt
              }

              (messageFactory >> t.transform >> transform) level
              |> setContext KnownLiterals.SpanInfoContextName spanLog

            do activeSpan.Value <- previous
            logger.logWith Info composedMsgFac

          member x.Dispose() =
            x.finish id
      }

  let create logger label =
    new SpanBuilder(logger, label)

  let setParentSpanInfo (info: SpanInfo) (b: SpanBuilder) =
    b.withParentSpanInfo info

  let withTransform (transform: Message -> Message) (b: SpanBuilder) =
    b.withTransform transform

  let start (b: SpanBuilder) =
    b.start ()

  let getActiveSpanId () =
    let active = activeSpan.Value
    if obj.ReferenceEquals(active, null) then None
    else Some active.spanId

[<AutoOpen; Extension>]
module LoggerSpanEx =
  type Logger with
    [<CompiledName "BuildSpan">]
    member x.buildSpan (label: string, ?transform: Message -> Message) =
      Span.create x label
        |> Span.withTransform (Option.defaultValue id transform)

    member x.startSpan (label: string, ?transform: Message -> Message) =
      x.buildSpan (label, ?transform=transform)
        |> Span.start

    member x.startSpan (label: string, parent: SpanInfo) =
      x.buildSpan label
        |> Span.setParentSpanInfo parent
        |> Span.start

    member x.startSpan (label: string, parent: Span) =
      x.startSpan(label, parent.info)
