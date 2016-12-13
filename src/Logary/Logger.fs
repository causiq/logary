namespace Logary

open Hopac
open NodaTime
open System
open System.Runtime.CompilerServices

/// See the docs on the funtions for descriptions on how Ack works in conjunction
/// with the promise.
type Logger =
  inherit Named

  /// Write a message to the Logger. The returned value represents the commit
  /// point that Logary has acquired the message. The alternative is always
  /// selectable (through `Alt.always ()` if the logger filtered out the message
  /// due to a Rule).
  ///
  /// If the Message was not filtered through a Rule, but got sent onwards, the
  /// promise is there to denote the ack that all targets have successfully
  /// flushed the message. If you do not commit to the Alt then it will not be
  /// logged.
  ///
  /// If you choose to not await the Promise/Ack it makes no difference, since
  /// it will be garbage collected in the end, whether it's awaited or not.
  abstract logWithAck : LogLevel -> (LogLevel -> Message) -> Alt<Promise<unit>>

  /// Logs with the specified log level with backpressure via Logary's
  /// buffers.
  ///
  /// Calls to this function will block the caller only while executing the
  /// callback (if the level is active).
  ///
  /// The returned async value will yield when the message has been added to
  /// the buffers of Logary.
  ///
  /// You need to start the (cold) Alt value for the logging to happen.
  ///
  /// You should not do blocking/heavy operations in the callback.
  abstract log : LogLevel -> (LogLevel -> Message) -> Alt<unit>

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged.
  abstract level : LogLevel

/// A disposable interface to use with `use` constructs and to create child-
/// contexts. Since it inherits Logger, you can pass this scope down into child
/// function calls. This interface should dovetail with how Zipkin/Dapper
/// manages parent/child spans.
type LoggerScope =
  inherit IDisposable
  inherit Logger

type TimeScope =
  inherit LoggerScope

  /// Gets the currently elapsed duration of this time scope scope.
  abstract elapsed : Duration

  //abstract bisect : (LogLevel -> Message) -> Alt<Promise<unit>>

/// The Logger module provides functions for expressing how a Message should be
/// logged.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); Extension>]
module Logger =
  open Hopac
  open Hopac.Infixes
  open System
  open System.Threading.Tasks
  open System.Diagnostics
  open Logary

  /// How many milliseconds Logary should wait for placing messages in the RingBuffer
  let defaultTimeout = 5000u

  /////////////////////
  // Logging methods //
  /////////////////////

  let private ifLevel (logger : Logger) level otherwise f =
    if logger.level <= level then
      f ()
    else
      otherwise

  // NOTE all extension methods are in CSharp.fs, so they've been removed from this file
  // to avoid polluting the API unecessarily.

  /// Log a message, but don't await all targets to flush.
  let log (logger : Logger) logLevel messageFactory : Alt<unit> =
    logger.log logLevel messageFactory

  let private simpleTimeout millis loggerName =
    timeOutMillis millis
    |> Alt.afterFun (fun msg ->
      Console.Error.WriteLine("Logary message timed out. This means that you have an underperforming Logary target. {0}Logger: {1}",
                              Environment.NewLine, loggerName))

  /// Log a message, but don't await all targets to flush. Also, if it takes more
  /// than 5 seconds to add the log message to the buffer; simply drop the message.
  /// Returns true if the message was successfully placed in the buffers, or
  /// false otherwise.
  let logWithTimeout (logger : Logger) (millis : uint32) logLevel messageFactory : Alt<bool> =
    //printfn "logWithTimeout %i! %A" millis msg.value
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
  let logSimple (logger : Logger) msg : unit =
    start (logWithTimeout logger defaultTimeout msg.level (fun _ -> msg) ^->. ())

  /// Log a message, which returns a promise. The first Alt denotes having the
  /// Message placed in all Targets' buffers. The inner Promise denotes having
  /// the message properly flushed to all targets' underlying "storage". Targets
  /// whose rules do not match the message will not be awaited.
  let logWithAck (logger : Logger) logLevel messageFactory : Alt<Promise<unit>> =
    logger.logWithAck logLevel messageFactory

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds. As an exception to the rule,
  /// it is allowed to pass `nameEnding` as null to this function. This
  /// function returns the full schabang; i.e. it will let you wait for
  /// acks if you want. If you do not start/commit to the Alt, the
  /// logging of the gauge will never happen.
  let timeWithAckT (logger : Logger)
                   (nameEnding : string)
                   (transform : Message -> Message)
                   (f : 'input -> 'res)
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
  let timeWithAck (logger : Logger)
                  (nameEnding : string)
                  (f : 'input -> 'res)
                  : 'input -> 'res * Alt<Promise<unit>> =
    timeWithAckT logger nameEnding id f

  let time (logger : Logger)
           (nameEnding : string)
           (f : 'input -> 'res)
           : 'input -> 'res * Alt<unit> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      let res, message = Message.time name f input
      res, log logger message.level (fun _ -> message)

  let timeX (logger : Logger)
            (f : 'input -> 'res)
            : 'input -> 'res * Alt<unit> =
    time logger null f

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Scaled(Seconds, 10^9). Finally
  /// transform the message using the `transform` function.
  let timeSimpleT (logger : Logger)
                  (nameEnding : string)
                  (transform : Message -> Message)
                  (f : 'input -> 'res)
                  : 'input -> 'res =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      let res, message = Message.time name f input
      logSimple logger (transform message)
      res

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Scaled(Seconds, 10^9).
  let timeSimple (logger : Logger)
                 (nameEnding : string)
                 (f : 'input -> 'res)
                 : 'input -> 'res =
    timeSimpleT logger nameEnding id f

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Scaled(Seconds, 10^9).
  let timeSimpleX (logger : Logger)
                  (f : 'input -> 'res)
                  : 'input -> 'res =
    timeSimpleT logger null id f

  [<CompiledName "TimeWithAck"; Extension>]
  let timeAsyncWithAck (logger : Logger)
                       (nameEnding : string)
                       (f : 'input -> Async<'res>)
                       : 'input -> Async<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAsync name f input |> Async.map (fun (res, message) ->
      res, logWithAck logger message.level (fun _ -> message))

  [<CompiledName "TimeSimple"; Extension>]
  let timeAsyncSimple (logger : Logger)
                      (nameEnding : string)
                      (f : 'input -> Async<'res>)
                      : 'input -> Async<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAsync name f input |> Async.map (fun (res, message) ->
      logSimple logger message
      res)

  [<CompiledName "TimeWithAck"; Extension>]
  let timeJobWithAck (logger : Logger)
                     (nameEnding : string)
                     (f : 'input -> Job<'res>)
                     : 'input -> Job<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeJob name f input |> Job.map (fun (res, message) ->
      res, logWithAck logger message.level (fun _ -> message))

  [<CompiledName "TimeSimple"; Extension>]
  let timeJobSimple (logger : Logger)
                    (nameEnding : string)
                    (f : 'input -> Job<'res>)
                    : 'input -> Job<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeJob name f input |> Job.map (fun (res, message) ->
      logSimple logger message
      res)

  [<CompiledName "TimeWithAck"; Extension>]
  let timeAltWithAck (logger : Logger)
                     (nameEnding : string)
                     (f : 'input -> Alt<'res>)
                     : 'input -> Alt<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAlt name f input ^-> fun (res, message) ->
      res, logWithAck logger message.level (fun _ -> message)

  [<CompiledName "TimeSimple"; Extension>]
  let timeAltSimple (logger : Logger)
                    (nameEnding : string)
                    (f : 'input -> Alt<'res>)
                    : 'input -> Alt<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      Message.timeAlt name f input ^-> fun (res, message) ->
      logSimple logger message
      res

  // corresponds to CSharp.TimeWithAck
  let timeTaskWithAckT (logger : Logger)
                       (nameEnding : string)
                       (transform : Message -> Message)
                       (f : 'input -> Task<'res>)
                       : 'input -> Task<'res * Alt<Promise<unit>>> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      (Message.timeTask name f input).ContinueWith(fun (task : Task<'res * Message>) ->
        let res, message = task.Result
        let message = transform message
        res, logWithAck logger message.level (fun _ -> message))

  // corresponds to CSharp.TimeWithAck
  let timeTaskWithAck logger nameEnding (f : 'input -> Task<'res>) =
    timeTaskWithAckT logger nameEnding id f

  // corresponds to CSharp.TimeSimple
  let timeTaskSimpleT (logger : Logger)
                      (nameEnding : string)
                      (transform : Message -> Message)
                      (f : 'input -> Task<'res>)
                      : 'input -> Task<'res> =
    let name = logger.name |> PointName.setEnding nameEnding
    fun input ->
      (Message.timeTask name f input).ContinueWith(fun (task : Task<'res * Message>) ->
        let res, message = task.Result
        logSimple logger (transform message)
        res)

  let timeScopeT (logger : Logger)
                 (nameEnding : string)
                 (transform : Message -> Message)
                 : TimeScope =
    let name = logger.name |> PointName.setEnding nameEnding
    let sw = Stopwatch.StartNew()
    { new TimeScope with
        member x.Dispose () =
          sw.Stop()
          let value, units = sw.toGauge()
          let message = Message.gaugeWithUnit name units value
          logSimple logger message

        member x.elapsed =
          Duration.FromTimeSpan sw.Elapsed

        member x.logWithAck logLevel messageFactory =
          logger.logWithAck logLevel messageFactory

        member x.log logLevel messageFactory =
          logger.log logLevel messageFactory

        member x.level =
          logger.level

        member x.name =
          name
    }

  let apply (middleware : Message -> Message) (logger : Logger) : Logger =
    { new Logger with
      member x.log logLevel messageFactory =
        logger.log logLevel (messageFactory >> middleware)
      member x.logWithAck logLevel messageFactory =
        logger.logWithAck logLevel (messageFactory >> middleware)
      member x.level = logger.level
      member x.name = logger.name }

/// Syntactic sugar on top of Logger for use of curried factory function
/// functions.
[<AutoOpen>]
module LoggerEx =
  open Hopac.Infixes

  let private lwa (x : Logger) lvl f =
    let ack = IVar ()
    start (x.logWithAck lvl f
           |> Alt.afterJob id
           |> Alt.afterJob (IVar.fill ack))
    ack :> Promise<_>

  type Logger with
    member x.verbose (messageFactory : LogLevel -> Message) : unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Verbose messageFactory ^->. ())

    /// Log with backpressure
    member x.verboseWithBP (messageFactory : LogLevel -> Message) : Alt<unit> =
      x.log Verbose messageFactory

    member x.verboseWithAck (messageFactory : LogLevel -> Message) : Promise<unit> =
      lwa x Verbose messageFactory

    member x.debug (messageFactory : LogLevel -> Message) : unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Debug messageFactory ^->. ())

    /// Log with backpressure
    member x.debugWithBP (messageFactory : LogLevel -> Message) : Alt<unit> =
      x.log Debug messageFactory

    member x.debugWithAck (messageFactory : LogLevel -> Message) : Promise<unit> =
      lwa x Debug messageFactory

    member x.info messageFactory : unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Info messageFactory ^->. ())

    /// Log with backpressure
    member x.infoWithBP messageFactory : Alt<unit> =
      x.log Info messageFactory

    member x.infoWithAck (messageFactory : LogLevel -> Message) : Promise<unit> =
      lwa x Info messageFactory

    member x.warn messageFactory : unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Warn messageFactory ^->. ())

    /// Log with backpressure
    member x.warnWithBP messageFactory : Alt<unit> =
      x.log Warn messageFactory

    member x.warnWithAck (messageFactory : LogLevel -> Message) : Promise<unit> =
      lwa x Warn messageFactory

    member x.error messageFactory : unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Error messageFactory ^->. ())

    /// Log with backpressure
    member x.errorWithBP messageFactory : Alt<unit> =
      x.log Error messageFactory

    member x.errorWithAck (messageFactory : LogLevel -> Message) : Promise<unit> =
      lwa x Error messageFactory

    member x.fatal messageFactory : unit =
      start (Logger.logWithTimeout x Logger.defaultTimeout Fatal messageFactory ^->. ())

    /// Log with backpressure
    member x.fatalWithBP messageFactory : Alt<unit> =
      x.log Fatal messageFactory

    member x.fatalWithAck (messageFactory : LogLevel -> Message) : Promise<unit> =
      lwa x Fatal messageFactory

    member x.logSimple message : unit =
      Logger.logSimple x message

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
type NullLogger() =
  let instaPromise =
    Alt.always (Promise (())) // new promise with unit value
  let insta =
    Alt.always ()
  interface Logger with
    member x.logWithAck logLevel messageFactory = instaPromise
    member x.log logLevel messageFactory = insta
    member x.level = Fatal
    member x.name = PointName.ofList [ "Logary"; "NullLogger" ]