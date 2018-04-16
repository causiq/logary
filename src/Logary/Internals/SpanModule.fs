namespace Logary

open System.Threading
open System
open NodaTime
open FSharp.NativeInterop


/// describe the time scope info about a span, will be sent as a message's context data
[<Struct>]
type SpanInfo =
  {
    id : string
    beginAt: int64 // number of ticks since the Unix epoch. Negative values represent instants before the Unix epoch. (from NodaTime)
    endAt: int64 // number of ticks since the Unix epoch. Negative values represent instants before the Unix epoch. (from NodaTime)
    duration: int64 // total number of ticks in the duration as a 64-bit integer. (from NodaTime)
  }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpanBuilder =

  type private T =
    {
      parentSpanId: string
      messageFac: LogLevel -> Message
      clock: IClock
      localRootPrefix: string
    }

  type private SpanBookmark =
    {
      spanId: string
      childCount: int64 ref
    }

  let private getRandomLo () =
    let mutable random = System.Guid.NewGuid()
    && random
    |> NativePtr.toNativeInt
    |> NativePtr.ofNativeInt<int64>
    |> NativePtr.read<int64>


  type SpanBuilder (logger: Logger) =
    static let activeSpan:AsyncLocal<SpanBookmark> = new AsyncLocal<SpanBookmark> ()
    static let localRootCount = ref 0L
    static let mutable defaultRootPrefix = sprintf "%x" (getRandomLo ())


    let mutable t =
      { parentSpanId = null
        messageFac = Message.eventX ""
        clock = NodaTime.SystemClock.Instance
        localRootPrefix = defaultRootPrefix
      }

    let mutable hasFired = false

    // we assume that one span will not have more then int64.MaxValue child spans in its lifetime
    let generateId () =
      // parent span id can be set explicitly (usually used when combine with outside service, e.g. through http, rpc...)
      if String.IsNullOrEmpty t.parentSpanId then
        // fallback to use from AsyncLocal ( ambient context )
        let parent = activeSpan.Value
        if obj.ReferenceEquals(parent, null) then
          // root span from this service, prepend local info
          sprintf "#%s.%x" t.localRootPrefix (Interlocked.Increment(localRootCount))
        else
          // child span from this service
          sprintf "%s.%x" parent.spanId (Interlocked.Increment(parent.childCount))
      else
        // set parent spanId explicitly, check if it is a local span
        let parentSpanId = t.parentSpanId
        if parentSpanId.Substring(parentSpanId.LastIndexOf("#")+1).StartsWith(t.localRootPrefix) then
          // child span belong to this service, use local root count
          sprintf "%s.%x" parentSpanId (Interlocked.Increment(localRootCount))
        else
          // root span from this service, prepend local info
          sprintf "%s#%s.%x" parentSpanId t.localRootPrefix (Interlocked.Increment(localRootCount))

    member x.withParentId pid =
      do t <- {t with parentSpanId = pid}
      x

    member x.withMessage fac =
      do t <- {t with messageFac = fac}
      x

    member x.withClock clock =
      do t <- {t with clock = clock}
      x

    member x.withLocalRootPrefix prefix =
      if not <| String.IsNullOrEmpty(prefix) then
        do t <- {t with localRootPrefix = prefix}
      x
    member x.start () =
      let spanId = generateId ()
      let beginAt = t.clock.GetCurrentInstant()

      let previous = activeSpan.Value
      let bookMark = {
        spanId = spanId
        childCount = ref 0L
      }
      activeSpan.Value <- bookMark

      { new Span with
          member x.id = spanId

          member x.finish (transform: Message -> Message) =
            let composedMsgFac level =
              let endAt = t.clock.GetCurrentInstant()
              let dur = endAt - beginAt
              let spanInfo = {
                id = spanId
                beginAt = beginAt.ToUnixTimeTicks()
                endAt = endAt.ToUnixTimeTicks()
                duration = dur.BclCompatibleTicks
              }

              (t.messageFac >> transform) level
              |> Message.setContext KnownLiterals.SpanInfoContextName spanInfo
              |> Message.setSpanId spanId

            if not hasFired then
              do hasFired <- true
              do activeSpan.Value <- previous
              logger.logWith Info composedMsgFac

          member x.Dispose () =
            x.finish id
      }

    static member setGlobalLocalRootPrefix (rootPrefix: string) =
      defaultRootPrefix <- rootPrefix

    static member getActiveSpanId () =
      let active = activeSpan.Value
      if obj.ReferenceEquals(active, null) then null
      else active.spanId

  let create logger =
      new SpanBuilder(logger)

  let setClock (clock: IClock) (b: SpanBuilder) =
    b.withClock clock
  let setLocalRootPrefix (prefix: string) (b: SpanBuilder) =
    b.withLocalRootPrefix prefix

  let setParentSpanId (pid: string) (b: SpanBuilder) =
    b.withParentId pid

  let setMessage (fac: LogLevel -> Message) (b: SpanBuilder) =
    b.withMessage fac

  let start (b: SpanBuilder) =
    b.start ()

[<AutoOpen>]
module LoggerSpanEx =
  type Logger with
    member x.buildSpan () =
      SpanBuilder.create x