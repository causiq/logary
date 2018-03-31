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

  // NOTE all extension methods are in CSharp.fs, so they've been removed from this file
  // to avoid polluting the API unecessarily.

  /// Log a message, but don't await all targets to flush.
  let inline log (logger: Logger) logLevel messageFactory: Alt<unit> =
    if logLevel >= logger.level then
      logger.logWithAck logLevel messageFactory ^-> ignore
    else
      Alt.always ()

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

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds. As an exception to the rule,
  /// it is allowed to pass `nameEnding` as null to this function. This
  /// function returns the full schabang; i.e. it will let you wait for
  /// acks if you want. If you do not start/commit to the Alt, the
  /// logging of the gauge will never happen.
  let timeWithAckT (logger: Logger)
                   (nameEnding: string)
                   (transform: Message -> Message)
                   (f: 'input -> 'res)
                   : 'input -> 'res * Alt<Promise<unit>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      let res, message = Message.time name f input
      let message = transform message
      res, logWithAck logger message.level (fun _ -> message)

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds. As an exception to the rule,
  /// it is allowed to pass `nameEnding` as null to this function. This
  /// function returns the full schabang; i.e. it will let you wait for
  /// acks if you want. If you do not start/commit to the Alt, the
  /// logging of the gauge will never happen.
  let timeWithAck (logger: Logger)
                  (nameEnding: string)
                  (f: 'input -> 'res)
                  : 'input -> 'res * Alt<Promise<unit>> =
    timeWithAckT logger nameEnding id f

  let time (logger: Logger)
           (nameEnding: string)
           (f: 'input -> 'res)
           : 'input -> 'res * Alt<unit> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      let res, message = Message.time name f input
      res, log logger message.level (fun _ -> message)

  let timeX (logger: Logger)
            (f: 'input -> 'res)
            : 'input -> 'res * Alt<unit> =
    time logger null f

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Scaled(Seconds, 10^9). Finally
  /// transform the message using the `transform` function.
  let timeSimpleT (logger: Logger)
                  (nameEnding: string)
                  (transform: Message -> Message)
                  (f: 'input -> 'res)
                  : 'input -> 'res =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      let res, message = Message.time name f input
      logSimple logger (transform message)
      res

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Scaled(Seconds, 10^9).
  let timeSimple (logger: Logger)
                 (nameEnding: string)
                 (f: 'input -> 'res)
                 : 'input -> 'res =
    timeSimpleT logger nameEnding id f

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Scaled(Seconds, 10^9).
  let timeSimpleX (logger: Logger)
                  (f: 'input -> 'res)
                  : 'input -> 'res =
    timeSimpleT logger null id f

  [<CompiledName "TimeWithAck"; Extension>]
  let timeAsyncWithAck (logger: Logger)
                       (nameEnding: string)
                       (f: 'input -> Async<'res>)
                       : 'input -> Async<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAsync name f input |> Async.map (fun (res, message) ->
      res, logWithAck logger message.level (fun _ -> message))

  [<CompiledName "TimeSimple"; Extension>]
  let timeAsyncSimple (logger: Logger)
                      (nameEnding: string)
                      (f: 'input -> Async<'res>)
                      : 'input -> Async<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAsync name f input |> Async.map (fun (res, message) ->
      logSimple logger message
      res)

  [<CompiledName "TimeWithAck"; Extension>]
  let timeJobWithAck (logger: Logger)
                     (nameEnding: string)
                     (f: 'input -> Job<'res>)
                     : 'input -> Job<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeJob name f input |> Job.map (fun (res, message) ->
      res, logWithAck logger message.level (fun _ -> message))

  [<CompiledName "TimeSimple"; Extension>]
  let timeJobSimple (logger: Logger)
                    (nameEnding: string)
                    (f: 'input -> Job<'res>)
                    : 'input -> Job<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeJob name f input |> Job.map (fun (res, message) ->
      logSimple logger message
      res)

  [<CompiledName "TimeWithAck"; Extension>]
  let timeAltWithAck (logger: Logger)
                     (nameEnding: string)
                     (f: 'input -> Alt<'res>)
                     : 'input -> Alt<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAlt name f input ^-> fun (res, message) ->
      res, logWithAck logger message.level (fun _ -> message)

  [<CompiledName "TimeSimple"; Extension>]
  let timeAltSimple (logger: Logger)
                    (nameEnding: string)
                    (f: 'input -> Alt<'res>)
                    : 'input -> Alt<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAlt name f input ^-> fun (res, message) ->
      logSimple logger message
      res

  // corresponds to CSharp.TimeWithAck
  let timeTaskWithAckT (logger: Logger)
                       (nameEnding: string)
                       (transform: Message -> Message)
                       (f: 'input -> Task<'res>)
                       : 'input -> Task<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      (Message.timeTask name f input).ContinueWith(fun (task: Task<'res * Message>) ->
        let res, message = task.Result
        let message = transform message
        res, logWithAck logger message.level (fun _ -> message))

  // corresponds to CSharp.TimeWithAck
  let timeTaskWithAck logger nameEnding (f: 'input -> Task<'res>) =
    timeTaskWithAckT logger nameEnding id f

  // corresponds to CSharp.TimeSimple
  let timeTaskSimpleT (logger: Logger)
                      (nameEnding: string)
                      (transform: Message -> Message)
                      (f: 'input -> Task<'res>)
                      : 'input -> Task<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      (Message.timeTask name f input).ContinueWith(fun (task: Task<'res * Message>) ->
        let res, message = task.Result
        logSimple logger (transform message)
        res)

  let timeScopeT (logger: Logger) (nameEnding: string) (transform: Message -> Message): TimeScope =
    let name = logger.name |> PointName.setEnding nameEnding
    let bisections : (int64 * string) list ref = ref []

    let sw = Stopwatch.StartNew()

    let addSpan (m, i) (span: int64 (* ticks *), label: string) =
      let spanName = PointName [| PointName.format name ; "span"; string i |]
      let spanLabelName = PointName.setEnding "label" spanName

      let m' =
        m
        |> Message.addGauge (PointName.format spanName) (Ticks.toGauge span)
        |> Message.setContext (PointName.format spanLabelName) label

      m', i + 1L

    let addSpans m =
      if !bisections = [] then m else
      !bisections |> List.fold addSpan (m, 0L) |> fst

    let stop (sw: Stopwatch) (decider: Duration -> LogLevel) =
      sw.Stop()
      let level = Duration.FromTicks sw.ElapsedTicks |> decider
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
        member x.Dispose () =
          let message = stop sw (fun _ -> Debug)
          logSimple logger message

        member x.elapsed =
          Duration.FromTimeSpan sw.Elapsed

        member x.bisect label =
          bisect sw label

        member x.stop decider =
          let m = stop sw decider
          logger.logWithAck m.level (fun _ -> transform m)

        member x.logWithAck logLevel messageFactory =
          logger.logWithAck logLevel (messageFactory >> transform)

        member x.name =
          name

        member x.level = logger.level
    }

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

    // Utilities for Job

    /// Print the ToString representation of the Job before and after it is executed.
    member x.beforeAfter atLevel (xJ: Job<'x>): Job<'x> =
      job {
        x.logWith atLevel (fun level -> Message.eventX (sprintf "Before %O" xJ) level)
        let! res = xJ
        x.logWith atLevel (fun level -> Message.eventX (sprintf "After %O" xJ) level)
        return res
      }