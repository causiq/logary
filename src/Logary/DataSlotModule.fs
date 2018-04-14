namespace Logary

open System.Threading
open System
open NodaTime
open System.Collections.Concurrent
open Logary.Internals

[<AutoOpen>]
module DataSlotExtension =
  type IDataSlot with
    member x.push (dataFac: unit -> _) =
      x.push (lazy (dataFac () |> box))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DataSlot =

  type internal AsyncLocalDataSlot () =
    let _currentData = new AsyncLocal<Lazy<obj> list>()
    do _currentData.Value <- list.Empty

    let getCurrentDataFromAsyncLocal () =
      let current = _currentData.Value
      if obj.ReferenceEquals(current, null) then
        _currentData.Value <- []
        []
      else current

    let mutable innerDataSlot: IDataSlot option = None

    interface IDataSlot with
      member x.collect () =
        let innerData =
          match innerDataSlot with
          | Some inner -> inner.collect ()
          | None -> []

        getCurrentDataFromAsyncLocal ()
        |> List.map (fun lz -> lz.Value)
        |> List.append innerData

      member x.push data =
        match innerDataSlot with
        | Some inner ->
          inner.push data
        | None ->
          let previous = getCurrentDataFromAsyncLocal ()
          _currentData.Value <- data :: previous

          {
            new IDisposable with
              member x.Dispose () =
                _currentData.Value <- previous
          }

      member x.wrap inner = innerDataSlot <- Some inner

  let useDefault () : IDataSlot  = new AsyncLocalDataSlot () :> IDataSlot

module internal Span =

  open Hopac
  open Hopac.Infixes

  type SpanIdGenerator (runtime: RuntimeInfo, counterDic: ConcurrentDictionary<string, int ref>) =

    new (runtime: RuntimeInfo) = SpanIdGenerator (runtime, new ConcurrentDictionary<string, int ref>())

    member x.removeFromCounter spanId =
      counterDic.TryRemove(spanId) |> ignore

    // we assume that one span will not have more then int64.MaxValue child spans in its lifetime
    member x.generate (parentSpanId: string) =
      let parentSpanId = if isNull parentSpanId then String.Empty else parentSpanId.Trim()
      let localInfo = sprintf "#%s-%s" runtime.host runtime.service
      let localInfoPrefix = if parentSpanId.Contains(localInfo) then parentSpanId else localInfo

      let counter = counterDic.GetOrAdd(parentSpanId, ref 0)
      let counter = Interlocked.Increment(counter)
      sprintf "%s.%x" localInfoPrefix counter


  /// describe the time scope info about a span
  type SpanInfo =
    {
      id : string
      beginAt: int64 // UnixTimeTicks
      endAt: int64 // UnixTimeTicks
      duration: int64
    }

  /// span focus on time scope (scope means it needs explicitly specify its end) which usually used for tracing.
  /// message focus on data context which usually used for logging/metric (since metric can be express by gauge which inclued by message).
  /// one message can belong to a span, it means this log message happened in this span.
  /// span itself contains one message, when logging, it just fill message with some specific info (startTime, endTime, duration, id ...)，
  /// then every target can transform this message into their own backend's implementation (like zipkin,jaeger,opentracing...).
  /// since span can be finished, so they will have a logger and send it's message at that time.
  type T =
    {
      messageFac: LogLevel -> Message
      id: string
      beginAt: Instant
      logger: Logger
      hasFired: bool
      childCount: int ref
      clock: IClock
      idGen: SpanIdGenerator
    }

    interface Span with
      member x.id = x.id

      member x.finish (transform: Message -> Message) =
        if not x.hasFired then
          do x.idGen.removeFromCounter x.id
          let composedMsgFac level =
            let endAt = x.clock.GetCurrentInstant()
            let dur = endAt - x.beginAt
            let spanInfo = {
              id = x.id
              beginAt = x.beginAt.ToUnixTimeTicks()
              endAt = endAt.ToUnixTimeTicks()
              duration = dur.BclCompatibleTicks
            }

            (x.messageFac >> transform) level
            |> Message.setContext KnownLiterals.SpanInfoContextName spanInfo
            |> Message.setSpanId x.id

          x.logger.logWithAck Info composedMsgFac >>=* id
        else Promise (())

    interface IDisposable with
      member x.Dispose () =
         (x :> Span).finish id |> Hopac.start


  let createSpanT (clock: IClock) (idGen: SpanIdGenerator) (parentSpanId: string) (logger: Logger) (msgFac: LogLevel -> Message) : T =
    let now = clock.GetCurrentInstant ()
    let spanId = idGen.generate parentSpanId

    { messageFac = msgFac
      id = spanId
      beginAt = now
      logger = logger
      hasFired = false
      childCount = ref 0
      clock = clock
      idGen = idGen}
