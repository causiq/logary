namespace Logary

open Hopac
open Hopac.Infixes
open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open System.Diagnostics
open Logary
open NodaTime

/// The Logger module provides functions for expressing how a Message should be
/// logged.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); Extension>]
module Logger =

  /// How many milliseconds Logary should wait for placing messages in the RingBuffer
  let defaultTimeout = 5000u

  /////////////////////
  // Logging methods //
  /////////////////////

  /// Log a message, but don't await all targets to flush. Equivalent to logWithBP.
  let inline log (logger: Logger) logLevel messageFactory: Alt<unit> =
    logger.logWithAck logLevel messageFactory ^-> ignore

  let private simpleTimeout millis loggerName =
    timeOutMillis millis
    |> Alt.afterFun (fun msg ->
      Console.Error.WriteLine("Logary message timed out. This means that you have an underperforming Logary target. {0}Logger: {1}",
                              Environment.NewLine, loggerName))

  /// Log a message, but don't await all targets to flush. Also, if it takes more
  /// than 5 seconds to add the log message to the buffer; simply drop the message.
  /// Returns true if the message was successfully placed in the buffers, or
  /// false otherwise.
  let logWithTimeout (logger: Logger) (millis: uint32) logLevel messageFactory: Alt<bool> =
    Alt.choose [
      log logger logLevel messageFactory ^->. true
      simpleTimeout (int millis) logger.name ^->. false
    ]

  /// Log a message, but don't synchronously wait for the message to be placed
  /// inside Logary's buffers. Instead the message will be added to Logary's
  /// buffers asynchronously with a timeout of 5 second, and will then be dropped.
  ///
  /// This is the way we avoid the unbounded buffer problem.
  ///
  /// If you have dropped messages, they will be logged to STDERR. You should load-
  /// test your app to ensure that your targets can send at a rate high enough
  /// without dropping messages.
  ///
  /// It's recommended to have alerting on STDERR.
  let logSimple (logger: Logger) msg: unit =
    start (logWithTimeout logger defaultTimeout msg.level (fun _ -> msg) ^->. ())

  /// See `logSimple`; but with a message factory parameter.
  let logWith (logger: Logger) level messageFactory: unit =
    start (logWithTimeout logger defaultTimeout level messageFactory ^->. ())

  /// Log a message, which returns a promise. The first Alt denotes having the
  /// Message placed in all Targets' buffers. The inner Promise denotes having
  /// the message properly flushed to all targets' underlying "storage". Targets
  /// whose rules do not match the message will not be awaited.
  let logWithAck (logger: Logger) logLevel messageFactory: Alt<Promise<unit>> =
    logger.logWithAck logLevel messageFactory

  let apply (middleware: Message -> Message) (logger: Logger): Logger =
    { new Logger with // Logger.apply delegator
      member x.logWithAck logLevel messageFactory =
        logger.logWithAck logLevel (messageFactory >> middleware)
      member x.name =
        logger.name
      member x.level =
        logger.level
    }

/// Syntactic sugar on top of Logger for use of curried factory function
/// functions.
[<AutoOpen>]
module LoggerEx =

  let private lwa (x: Logger) lvl f =
    let ack = IVar ()
    start (x.logWithAck lvl f
           |> Alt.afterJob id
           |> Alt.afterJob (IVar.fill ack))
    ack :> Promise<_>

  type Logger with
    member x.log logLevel (messageFactory: LogLevel -> Message): Alt<unit> =
      Logger.log x logLevel messageFactory

    member x.logSimple message: unit =
      Logger.logSimple x message

    member x.logWith level messageFactory: unit =
      Logger.logWith x level messageFactory

    member x.verbose (messageFactory: LogLevel -> Message): unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Verbose messageFactory ^->. ())

    /// Log with backpressure
    member x.verboseWithBP (messageFactory: LogLevel -> Message): Alt<unit> =
      x.log Verbose messageFactory

    member x.verboseWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      lwa x Verbose messageFactory

    member x.debug (messageFactory: LogLevel -> Message): unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Debug messageFactory ^->. ())

    /// Log with backpressure
    member x.debugWithBP (messageFactory: LogLevel -> Message): Alt<unit> =
      x.log Debug messageFactory

    member x.debugWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      lwa x Debug messageFactory

    member x.info messageFactory: unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Info messageFactory ^->. ())

    /// Log with backpressure
    member x.infoWithBP messageFactory: Alt<unit> =
      x.log Info messageFactory

    member x.infoWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      lwa x Info messageFactory

    member x.warn messageFactory: unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Warn messageFactory ^->. ())

    /// Log with backpressure
    member x.warnWithBP messageFactory: Alt<unit> =
      x.log Warn messageFactory

    member x.warnWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      lwa x Warn messageFactory

    member x.error messageFactory: unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Error messageFactory ^->. ())

    /// Log with backpressure
    member x.errorWithBP messageFactory: Alt<unit> =
      x.log Error messageFactory

    member x.errorWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      lwa x Error messageFactory

    member x.fatal messageFactory: unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Fatal messageFactory ^->. ())

    /// Log with backpressure
    member x.fatalWithBP messageFactory: Alt<unit> =
      x.log Fatal messageFactory

    member x.fatalWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      lwa x Fatal messageFactory

    member x.timeFun (f: 'input -> 'res,
                      ?measurement: string,
                      ?transform: Message -> Message,
                      ?waitForAck: bool,
                      ?logBefore: bool,
                      [<CallerMemberName>] ?memberName: string,
                      [<CallerFilePath>] ?path: string,
                      [<CallerLineNumber>] ?line: int)
                      : 'input -> 'res =
      let measurement = measurement |> Option.bind nullIsNone |> Option.orElse memberName |> Option.defaultValue "time"
      let transform = defaultArg transform id
      let waitForAck = defaultArg waitForAck false
      let logBefore = defaultArg logBefore false
      fun input ->
        if logBefore then
          x.verbose (Message.eventX "Before {measurement}" >> Message.setField "measurement" measurement)
        let ts = StopwatchTicks.getTimestamp()
        let res = f input
        let dur = Gauge.ofStopwatchTicks (ts - StopwatchTicks.getTimestamp())
        let cb dur =
          fun level ->
            dur
            |> Message.gaugeWithUnit x.name measurement
            |> Message.setLevel level
            |> Message.addCallerInfo (memberName, path, line)
            |> transform
        let logged =
          memo (
            if waitForAck then (x.logWithAck Debug (cb dur) ^=> id)
            else x.log Debug (cb dur)
          )
        // TODO: handle this with returning false from tryEnqueue
        while not (Promise.Now.isFulfilled logged) do
          System.Threading.Thread.Sleep(1)
        res

    member x.timeJob (xJ: Job<'a>,
                      ?measurement: string,
                      ?transform: Message -> Message,
                      ?waitForAck: bool,
                      ?logBefore: bool,
                      [<CallerMemberName>] ?memberName: string,
                      [<CallerFilePath>] ?path: string,
                      [<CallerLineNumber>] ?line: int)
                      : Job<'a> =
      let measurement = measurement |> Option.bind nullIsNone |> Option.orElse memberName |> Option.defaultValue "time"
      let transform = defaultArg transform id
      let waitForAck = defaultArg waitForAck false
      let logBefore = defaultArg logBefore false
      let cb dur =
        fun level ->
          dur
          |> Message.gaugeWithUnit x.name measurement
          |> Message.setLevel level
          |> Message.addCallerInfo (memberName, path, line)
          |> transform
      let onComplete dur =
        if waitForAck then x.logWithAck Debug (cb dur) ^=> id
        else x.log Debug (cb dur)
      let timedJob =
        Job.timeJob onComplete xJ
      if logBefore then
        x.log Verbose (Message.eventX "Before {measurement}" >> Message.setField "measurement" measurement)
        >>=. timedJob
      else
        timedJob

    member x.timeAlt (xA: Alt<'a>,
                      ?measurement: string,
                      ?transform: Message -> Message,
                      ?waitForAck: bool,
                      ?logBefore: bool,
                      [<CallerMemberName>] ?memberName: string,
                      [<CallerFilePath>] ?path: string,
                      [<CallerLineNumber>] ?line: int)
                      : Alt<'a> =
      let measurement = measurement |> Option.bind nullIsNone |> Option.orElse memberName |> Option.defaultValue "time"
      let transform = defaultArg transform id
      let waitForAck = defaultArg waitForAck false
      let logBefore = defaultArg logBefore false
      let cb wasNacked dur =
        fun level ->
          Message.gaugeWithUnit x.name measurement dur
          |> Message.tag (if wasNacked then "nack" else "ack")
          |> Message.setLevel level
          |> Message.addCallerInfo (memberName, path, line)
          |> transform
      let onComplete dur =
        if waitForAck then x.logWithAck Debug (cb true dur) ^=> id
        else x.log Debug (cb true dur)
      let onNack dur =
        if waitForAck then x.logWithAck Debug (cb false dur) ^=> id
        else x.log Debug (cb false dur)
      let timedAlt =
        Alt.timeJob onComplete onNack xA
      if logBefore then
        Alt.prepareJob (fun () ->
          x.log Verbose (Message.eventX "Before {measurement}" >> Message.setField "measurement" measurement)
          >>-. timedAlt)
      else timedAlt

    member x.timeScopeT (scopeName: string) (transform: Message -> Message): TimeScope =
      let name = x.name |> PointName.setEnding scopeName
      let bisections: (StopwatchTicks * string) list ref = ref []

      let sw = Stopwatch.StartNew()

      let addSpan (m, i) (span: StopwatchTicks, label: string) =
        let spanName = PointName [| PointName.format name ; "span"; string i |]
        let spanLabelName = PointName.setEnding "label" spanName

        let m' =
          m
          |> Message.addGauge (PointName.format spanName) (Gauge.ofStopwatchTicks span)
          |> Message.setContext (PointName.format spanLabelName) label

        m', i + 1L

      let addSpans m =
        if !bisections = [] then m else
        !bisections |> List.fold addSpan (m, 0L) |> fst

      let stop (sw: Stopwatch) (decider: Duration -> LogLevel) =
        sw.Stop()
        let level = Duration.FromTicks sw.Elapsed.Ticks |> decider
        sw.toGauge()
        |> Message.gaugeWithUnit name "duration"
        |> Message.setLevel level
        |> addSpans

      let bisect (sw: Stopwatch): string -> unit =
        fun label ->
          lock bisections <| fun () ->
          match !bisections with
          | [] ->
            bisections := (sw.ElapsedTicks, label) :: []
          | (latest, _) :: _ as bs ->
            bisections := (sw.ElapsedTicks - latest, label) :: bs

      { new TimeScope with
          member y.Dispose () =
            let message = stop sw (fun _ -> Debug)
            x.logSimple message

          member y.elapsed =
            Duration.FromTimeSpan sw.Elapsed

          member y.bisect label =
            bisect sw label

          member y.stop decider =
            let m = stop sw decider
            x.logWithAck m.level (fun _ -> transform m)

          member y.logWithAck logLevel messageFactory =
            x.logWithAck logLevel (messageFactory >> transform)

          member y.name = name
          member y.level = x.level
      }

    /// Print the ToString representation of the Job before and after it is executed.
    member x.beforeAfter atLevel (xJ: Job<'x>): Job<'x> =
      job {
        x.logWith atLevel (fun level -> Message.eventX (sprintf "Before %O" xJ) level)
        let! res = xJ
        x.logWith atLevel (fun level -> Message.eventX (sprintf "After %O" xJ) level)
        return res
      }