namespace Logary

open System
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.Builders
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Model
open Logary.Trace

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Logger =

  /// Log the message without blocking, and ignore how it went. If all targets' buffers are full, the message is
  /// dropped immediately. This is the safest function to log with, from a liveness perspective.
  let log (logger: Logger) message: unit =
    queueIgnore (logger.logWithAck(false, message))

  /// Logs the message with back pressure; this means that if the Alt is committed to, all target buffers have accepted
  /// the message. Otherwise, if the Alt is not committed to, at least one target buffer rejected the message.
  let logBP (logger: Logger) message: Alt<bool> =
    logger.logWithAck(true, message) ^-> Result.isOk

  /// Log the message, leaving any error result to be handled by the registry error handler.
  /// The returned Promise will be waiting for all matched targets to ACK the message, or if there was a failure,
  /// returns false as the result of the promise.
  let logAck (logger: Logger) message: Promise<bool> =
    logger.logWithAck(true, message) >>=* function
      | Ok ack -> ack ^->. true
      | Result.Error _ -> Promise false :> _

  /// Logs the message and hands back a LogResult. Does not wait for target buffers.
  let logWithAck (logger: Logger) message =
    logger.logWithAck(false, message)

  /// Sets the Logger's name.
  let setPointName (name: PointName) (logger: Logger): Logger =
    { new LoggerWrapper(logger) with override x.name = name }
    :> Logger

  /// Set the Logger's name; the name is passed to `PointName.parse` before being set as the Logger name.
  let setName (name: string) (logger: Logger): Logger =
    setPointName (PointName.parse name) logger

  /// Sets the logger's name to end with the passed string segment.
  let setNameEnding (ending: string) (logger: Logger): Logger =
    setPointName (PointName.setEnding ending logger.name) logger

  /// Applies a `Message -> Message` pipe to the logger's `logWithAck` function.
  let apply (middleware: Model.LogaryMessageBase -> unit) (logger: Logger): Logger =
    { new LoggerWrapper(logger) with
        override x.logWithAck (waitForBuffers, message) =
          let m =
            match message with
            | :? LogaryMessageBase as bm -> bm
            | _ -> message.getAsBase Model.Event
          middleware m
          logger.logWithAck (waitForBuffers, m)
    }
    :> Logger

[<AutoOpen>]
module LoggerEx =
  type Logger with
    [<CompiledName "Log">]
    member x.log(message, ?builder): unit =
      builder |> Option.iter (fun f -> f message)
      Logger.log x message

    [<CompiledName "LogBP">]
    member x.logBP(message, ?builder) : Alt<bool> =
      builder |> Option.iter (fun f -> f message)
      Logger.logBP x message

    [<CompiledName "LogAck">]
    member x.logAck(message, ?builder): Promise<bool> =
      builder |> Option.iter (fun f -> f message)
      Logger.logAck x message

    [<CompiledName "Apply">]
    member x.apply transform: Logger =
      Logger.apply transform x

    /// Also see: `eventBP`, `eventAck`, `eventWithAck` and the `log*` alternatives.
    [<CompiledName "Event">]
    member x.event(event: string, ?builder): unit =
      let m = Model.Event(event)
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    /// Also see: `event`, `eventAck`, `eventWithAck` and the `log*` alternatives.
    [<CompiledName "EventBP">]
    member x.eventBP(event: string, ?builder): Alt<unit> =
      let m = Model.Event(event)
      builder |> Option.iter (fun f -> f m)
      Logger.logBP x m |> Alt.Ignore

    /// Also see: `event`, `eventBP`, `eventWithAck` and the `log*` alternatives.
    [<CompiledName "EventAck">]
    member x.eventAck(event: string, ?builder): Promise<unit> =
      let m = Model.Event(event)
      builder |> Option.iter (fun f -> f m)
      Logger.logAck x m >>-*. ()

    /// Also see: `event`, `eventBP`, `eventAck` and the `log*` alternatives.
    [<CompiledName "EventWithAck">]
    member x.eventWithAck(event: string, ?builder): LogResult =
      let m = Model.Event event
      builder |> Option.iter (fun f -> f m)
      x.logWithAck(true, m)

    // verbose, debug, info-level events:

    [<CompiledName "Verbose">]
    member x.verbose(message, ?builder) =
      let m = (Model.Event(message, None, level=Verbose))
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "Debug">]
    member x.debug(message, ?builder) =
      let m = (Model.Event(message, None, level=Debug))
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "Info">]
    member x.info(message, ?builder) =
      let m = (Model.Event(message, None, level=Info))
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    // the below have -Ack and -BP overloads

    [<CompiledName "Warn">]
    member x.warn(message, ?builder): unit =
      let m = (Model.Event(message, None, level=Warn))
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "Warn">]
    member x.warn(message: string, e: exn, ?builder): unit =
      let m = Model.Event(message, None, level=Warn, error=e.toErrorInfo())
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "WarnBP">]
    member x.warnBP(message, ?builder): Alt<bool> =
      let m = (Model.Event(message, None, level=Warn))
      builder |> Option.iter (fun f -> f m)
      Logger.logBP x m

    [<CompiledName "WarnBPIgnore">]
    member x.warnBPIgnore(message, ?builder): Alt<unit> =
      x.warnBP(message, ?builder=builder) |> Alt.Ignore

    [<CompiledName "WarnAck">]
    member x.warnAck(message, ?builder): Promise<bool> =
      let m = (Model.Event(message, None, level=Warn))
      builder |> Option.iter (fun f -> f m)
      Logger.logAck x m

    [<CompiledName "WarnAckIgnore">]
    member x.warnAckIgnore(message, ?builder): Promise<unit> =
      x.warnAck(message, ?builder=builder) >>-*. ()


    [<CompiledName "Error">]
    member x.error(message, ?builder): unit =
      let m = Model.Event(message, None, level=Error)
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "Error">]
    member x.error(message: string, e: exn, ?builder): unit =
      let m = Model.Event(message, None, level=Error, error=e.toErrorInfo())
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "ErrorBP">]
    member x.errorBP(message, ?builder): Alt<bool> =
      let m = Model.Event(message, None, level=Error)
      builder |> Option.iter (fun f -> f m)
      Logger.logBP x m

    [<CompiledName "ErrorBPIgnore">]
    member x.errorBPIgnore(message, ?builder): Alt<unit> =
      x.errorBP(message, ?builder=builder) |> Alt.Ignore

    [<CompiledName "ErrorAck">]
    member x.errorAck(message, ?builder): Promise<bool> =
      let m = Model.Event(message, None, level=Error)
      builder |> Option.iter (fun f -> f m)
      Logger.logAck x m

    [<CompiledName "ErrorAckIgnore">]
    member x.errorAckIgnore(message, ?builder): Promise<unit> =
      x.errorAck(message, ?builder=builder) >>-*. ()


    [<CompiledName "Fatal">]
    member x.fatal(message, ?builder): unit =
      let m = Model.Event(message, None, level=Fatal)
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "Fatal">]
    member x.fatal(message: string, e: exn, ?builder): unit =
      let m = Model.Event(message, None, level=Fatal, error=e.toErrorInfo())
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "FatalBP">]
    member x.fatalBP(message, ?builder): Alt<bool> =
      let m = Model.Event(message, None, level=Fatal)
      builder |> Option.iter (fun f -> f m)
      Logger.logBP x m

    [<CompiledName "FatalBPIgnore">]
    member x.fatalBPIgnore(message, ?builder): Alt<unit> =
      x.fatalBP(message, ?builder=builder) |> Alt.Ignore

    [<CompiledName "FatalAck">]
    member x.fatalAck(message, ?builder): Promise<bool> =
      let m = Model.Event(message, None, level=Fatal)
      builder |> Option.iter (fun f -> f m)
      Logger.logAck x m

    [<CompiledName "FatalAckIgnore">]
    member x.fatalAckIgnore(message, ?builder): Promise<unit> =
      x.fatalAck(message, ?builder=builder) >>-*. ()

    // gauges

    [<CompiledName "Gauge">]
    member x.gauge(gauge, labels, ?builder): unit =
      let m = Model.GaugeMessage(gauge, labels)
      builder |> Option.iter (fun f -> f m)
      Logger.log x m

    [<CompiledName "GaugeBP">]
    member x.gaugeBP(gauge, labels, ?builder): Alt<unit> =
      let m = Model.GaugeMessage(gauge, labels)
      builder |> Option.iter (fun f -> f m)
      Logger.logBP x m |> Alt.Ignore

    [<CompiledName "GaugeAck">]
    member x.gaugeAck(gauge, labels, ?builder): Promise<unit> =
      let m = Model.GaugeMessage(gauge, labels)
      builder |> Option.iter (fun f -> f m)
      Logger.logAck x m >>-*. ()

    [<CompiledName "GaugeWithAck">]
    member x.gaugeWithAck(gauge, labels, ?builder): LogResult =
      let m = Model.GaugeMessage(gauge, labels)
      builder |> Option.iter (fun f -> f m)
      x.logWithAck(true, m)


    // spans/timers:

    [<CompiledName "Time">]
    member x.time (fn: 'a -> 'b, ?measurement: string, ?builder,
                   [<CallerMemberName>] ?memberName: string, [<CallerFilePath>] ?file: string,
                   [<CallerLineNumber>] ?lineNo: int): 'a -> 'b =

      let measurement =
        measurement
          |> Option.bind nullIsNone
          |> Option.orElse memberName
          |> Option.defaultValue "time"

      let log (ts: EpochNanoSeconds) (dur: Gauge) =
        let labels = Map [
          "logger", x.name.ToString()
          "gauge_name", measurement
        ]
        let name = x.name |> PointName.setEnding measurement
        let m = Model.GaugeMessage(dur, labels, timestamp=ts, level=Debug, name=name)
        m.addCallerInfo(defaultArg memberName "time", ?file=file, ?lineNo=lineNo)
        builder |> Option.iter (fun f -> f m)
        x.log m

      fun input ->
        let ts = Global.getTimestamp()
        let res, dur = Stopwatch.time (fun () -> fn input)
        log ts dur
        res


    [<CompiledName "TimeJob">]
    member x.timeJob (xJ: Job<'a>, ?measurement: string, ?builder,
                     [<CallerMemberName>] ?memberName: string, [<CallerFilePath>] ?file: string,
                     [<CallerLineNumber>] ?lineNo: int): Job<'a> =

      let measurement =
        measurement
          |> Option.bind nullIsNone
          |> Option.orElse memberName
          |> Option.defaultValue "time"

      let cb ts dur =
        let labels = Map [
          "logger", x.name.ToString()
          "gauge_name", measurement
        ]
        let name = x.name |> PointName.setEnding measurement
        let m = Model.GaugeMessage(dur, labels, timestamp=ts, level=Debug, name=name)
        m.addCallerInfo(defaultArg memberName "timeJob", ?file=file, ?lineNo=lineNo)
        builder |> Option.iter (fun f -> f m)
        m

      let onComplete ts dur = x.log (cb ts dur); Job.unit()

      Job.timeJob onComplete xJ

    [<CompiledName "TimeAlt">]
    member x.timeAlt (xA: Alt<'a>, ?measurement: string, ?builder,
                      [<CallerMemberName>] ?memberName: string, [<CallerFilePath>] ?file: string,
                      [<CallerLineNumber>] ?lineNo: int): Alt<'a> =

      let measurement =
        measurement
          |> Option.bind nullIsNone
          |> Option.orElse memberName
          |> Option.defaultValue "time"

      let cb ts wasACKed dur =
        let outcome = if wasACKed then "ack" else "nack"
        let labels = Map [
          "logger", x.name.ToString()
          "gauge_name", measurement
          "outcome", outcome
        ]
        let name = x.name |> PointName.setEnding measurement
        let m = Model.GaugeMessage(dur, labels, timestamp=ts, level=Debug, name=name)
        m.setField("outcome", outcome)
        m.addCallerInfo(defaultArg memberName "timeAlt", ?file=file, ?lineNo=lineNo)
        builder |> Option.iter (fun f -> f m)
        m

      let onComplete ts dur = x.log(cb ts true dur); Alt.unit()
      let onNack ts dur = x.log(cb ts false dur); Alt.unit()

      Alt.timeJob onComplete onNack xA


    member x.timeTask(taskFactory: CancellationToken -> Task<'a>, token: CancellationToken,
                      ?measurement: string, ?builder,
                      [<CallerMemberName>] ?memberName: string, [<CallerFilePath>] ?file: string,
                      [<CallerLineNumber>] ?lineNo: int): Task<'a> =
      let measurement =
        measurement
          |> Option.bind nullIsNone
          |> Option.orElse memberName
          |> Option.defaultValue "time"

      let labels = Map [
        "logger", x.name.ToString()
        "gauge_name", measurement
      ]

      let markFinished ranToCompletion started =
        let name = x.name |> PointName.setEnding measurement
        let finished = Stopwatch.getTimestamp()
        let g = Gauge.ofStopwatchTicks (finished - started)
        let outcome = if ranToCompletion then "ack" else "nack"
        let labels = labels |> Map.add "outcome" outcome
        let m = Model.GaugeMessage(g, labels, timestamp=started, level=Debug, name=name)
        m.addCallerInfo(defaultArg memberName "timeTask", ?file=file, ?lineNo=lineNo)
        m.setField("outcome", outcome)
        x.log(m, ?builder=builder)

      task {
        let started = Stopwatch.getTimestamp()
        try
          let! res = taskFactory token
          markFinished true started
          return res
        with :? OperationCanceledException as oce ->
          markFinished false started
          return oce.reraise()
      }


    [<CompiledName "Scoped">]
    member x.scoped label =
      match x with
      | :? SpanLogger as s ->
        s.startChild(label)
      | _ ->
        x.startSpan(label)

    [<CompiledName "Scoped">]
    member x.scoped(label: PointName) =
      x.scoped(label.ToString())


    // Deferred message constructor methods:

    [<CompiledName "LogWithBP">]
    member x.logWithBP logLevel messageFactory =
      if logLevel >= x.level then x.logBP (messageFactory logLevel) |> Alt.Ignore
      else Alt.unit ()


    [<CompiledName "DebugWithBP">]
    member x.debugWithBP messageFactory =
      if x.level >= Debug then x.logBP (messageFactory Debug) |> Alt.Ignore
      else Alt.unit ()

    [<CompiledName "DebugWithAck">]
    member x.debugWithAck messageFactory =
      if x.level >= Debug then x.logAck (messageFactory Debug) >>-*. ()
      else Promise.unit


    [<CompiledName "InfoWithBP">]
    member x.infoWithBP messageFactory =
      if x.level >= Info then x.logBP (messageFactory Info) |> Alt.Ignore
      else Alt.unit ()

    [<CompiledName "InfoWithAck">]
    member x.infoWithAck messageFactory =
      if x.level >= Info then x.logAck (messageFactory Info) >>-*. ()
      else Promise.unit


    [<CompiledName "WarnWithBP">]
    member x.warnWithBP messageFactory =
      if x.level >= Warn then x.logBP (messageFactory Warn) |> Alt.Ignore
      else Alt.unit ()

    [<CompiledName "WarnWithAck">]
    member x.warnWithAck messageFactory =
      if x.level >= Warn then x.logAck (messageFactory Warn) >>-*. ()
      else Promise.unit


    [<CompiledName "ErrorWithBP">]
    member x.errorWithBP messageFactory =
      if x.level >= Error then x.logBP(messageFactory Error) |> Alt.Ignore
      else Alt.unit ()

    [<CompiledName "ErrorWithAck">]
    member x.errorWithAck messageFactory =
      if x.level >= Error then x.logAck (messageFactory Error) >>-*. ()
      else Promise.unit


    [<CompiledName "FatalWithBP"; Obsolete "It doesn't make sense to use a deferred message callback for Fatal level logs. Rewrite as logger.fatalBP(message) or logger.fatalBP(message, fun m -> m.setField(\"key\", value))">]
    member x.fatalWithBP messageFactory = x.logBP (messageFactory Fatal) |> Alt.Ignore

    [<CompiledName "FatalWithAck"; Obsolete "It doesn't make sense to use a deferred message callback for Fatal level logs. Rewrite as logger.fatalAck(message) or logger.fatalAck(message, fun m -> m.setField(\"key\", value))">]
    member x.fatalWithAck messageFactory = x.logAck(messageFactory Fatal) >>-*. ()


module private Example =
  let doBigComputerStuff () =
    42

  let logIn (logger: Logger) =
    logger.event("User logged in", fun e -> e.setField("user", Value.Str "12345"))

    use scope = logger.scoped "computing the meaning to life"
    let res = doBigComputerStuff ()
    scope.info("Found solution, it's all in the logs!", fun m -> m.setField("res", res))
    res
