namespace Logary

open Hopac
open Hopac.Infixes
open System.Runtime.CompilerServices
open System.Diagnostics
open Logary
open NodaTime

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Logger =
  let defaultBackPressurePutBufferTimeOut = Duration.FromMinutes 1L

  /// log message without blocking, and ignore its result.
  /// if the buffer is full, drop this message
  let logWith (logger: Logger) level messageFactory: unit =
    queueIgnore (logger.logWithAck (false, level) messageFactory)

  /// log message without blocking, and ignore its result.
  /// if the buffer is full, drop this message
  let logSimple (logger: Logger) msg: unit =
    logWith logger msg.level (fun _ -> msg)

  /// log message, but don't await all targets to flush. And backpressure the buffers.
  ///
  /// default `waitForBuffersTimeout` is 1 minutes to back pressure the buffers (not the backend process after target buffer)
  let logWithBP (logger: Logger) logLevel messageFactory : Alt<unit> =
    logger.logWithAck (true, logLevel) (messageFactory >> Message.waitForBuffersTimeout defaultBackPressurePutBufferTimeOut) ^-> ignore

  /// log message, leave the error result handle in registry error handler.
  /// when read the return pomise, it will waiting for buffer's backend process flush logs
  ///
  /// default `waitForBuffersTimeout` is 1 minutes to back pressure the buffers (not the backend process after target buffer)
  let logAck (logger: Logger) level messageFactory: Promise<unit> =
    logger.logWithAck (true, level) (messageFactory >> Message.waitForBuffersTimeout defaultBackPressurePutBufferTimeOut)
    >>=* function
    | Ok promise -> promise
    | _ -> Promise.unit

  let private ensureName name =
    fun (m: Message) ->
      if m.name.isEmpty then { m with name = name } else m

  let apply (middleware: Message -> Message) (logger: Logger): Logger =
    let ensureName = ensureName logger.name
    { new LoggerWrapper(logger) with
        override x.logWithAck (waitForBuffers, logLevel) messageFactory =
          logger.logWithAck (waitForBuffers, logLevel) (
            messageFactory >> ensureName >> middleware)
    } :> Logger

[<AutoOpen>]
module LoggerEx =
  type Logger with

    /// Log a message, but don't await all targets to flush.
    /// Returns whether the message was successfully placed in the buffers.
    ///
    /// if the buffer is full, drop this message, return false
    member x.log logLevel (messageFactory: LogLevel -> Message): Alt<bool> =
      x.logWithAck (false, logLevel) messageFactory ^-> function
      | Ok _ -> true
      | _ -> false

    member x.logSimple message: unit =
      Logger.logSimple x message

    member x.logWith level messageFactory: unit =
      Logger.logWith x level messageFactory

    member x.logWithBP level messageFactory: Alt<unit> =
      Logger.logWithBP x level messageFactory

    member x.logAck level messageFactory: Promise<unit> =
      Logger.logAck x level messageFactory

    member x.apply transform: Logger =
      Logger.apply transform x

    member x.verbose (messageFactory: LogLevel -> Message): unit =
      Logger.logWith x Verbose messageFactory

    member x.verboseWithBP (messageFactory: LogLevel -> Message): Alt<unit> =
      Logger.logWithBP x Verbose messageFactory

    member x.verboseWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Verbose messageFactory

    member x.debug (messageFactory: LogLevel -> Message): unit =
      Logger.logWith x Debug messageFactory

    member x.debugWithBP (messageFactory: LogLevel -> Message): Alt<unit> =
      Logger.logWithBP x Debug messageFactory

    member x.debugWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Debug messageFactory

    member x.info messageFactory: unit =
      Logger.logWith x Info messageFactory

    member x.infoWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Info messageFactory

    member x.infoWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Info messageFactory

    member x.warn messageFactory: unit =
      Logger.logWith x Warn messageFactory

    member x.warnWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Warn messageFactory

    member x.warnWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Warn messageFactory

    member x.error messageFactory: unit =
      Logger.logWith x Error messageFactory

    member x.errorWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Error messageFactory

    member x.errorWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Error messageFactory

    member x.fatal messageFactory: unit =
      Logger.logWith x Fatal messageFactory

    member x.fatalWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Fatal messageFactory

    member x.fatalWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Fatal messageFactory

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
          if waitForAck then
            x.debugWithAck (cb dur)
          else
            x.debug (cb dur)
            Promise.unit

        while not (Promise.Now.isFulfilled logged) do
          System.Threading.Thread.Sleep(5)

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
        if waitForAck then x.logAck Debug (cb dur) :> Job<_>
        else x.log Debug (cb dur) |> Job.Ignore
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
        if waitForAck then x.logAck Debug (cb true dur) :> Job<_>
        else x.log Debug (cb true dur) |> Job.Ignore
      let onNack dur =
        if waitForAck then x.logAck Debug (cb false dur) :> Job<_>
        else x.log Debug (cb false dur) |> Job.Ignore
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
            x.logWithAck (false, m.level) (fun _ -> transform m)

          member y.logWithAck (waitForBuffers, logLevel) messageFactory =
            x.logWithAck (waitForBuffers, logLevel) (messageFactory >> transform)

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
