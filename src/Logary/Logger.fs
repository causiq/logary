namespace Logary

open Hopac
open System.Runtime.CompilerServices

/// See the docs on Logger.log for a description on how Ack works in conjunction
/// with the promise.
type Logger =
  inherit Named

  /// Also see `Logger.logDebugWithAck` and `Logger.log`.
  abstract logVerboseWithAck : (LogLevel -> Message) -> Alt<Promise<unit>>

  /// Deferred debug-levelled log; only evaluates the callback if any rule for
  /// applicative to this logger causes its input at debug-level to be handed to
  /// a target.
  ///
  /// Also see `Logger.log`.
  abstract logDebugWithAck : (LogLevel -> Message) -> Alt<Promise<unit>>

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
  abstract logWithAck : Message -> Alt<Promise<unit>>

  /// Like logWithAck, but doesn't wait for the log line to finish being
  /// persisted. This should be fine as long as your app is up 'for a while'
  /// after calling this, or if you flush the registry before shutting down
  /// your app (mostly; there will be a small interval before the job start
  /// during which Logary will not have the value in its data structures).
  ///
  /// If you don't know which of the functions on Logger to use, you may
  /// use this one and it "will just work".
  ///
  /// (Using this function means that you always commit to sending the log
  /// message; this means that if any of your targets' incoming message buffers
  /// are full, this call will hang forever)
  abstract logSimple : Message -> unit

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged.
  abstract level : LogLevel

/// The Logger module provides functions for expressing how a Message should be
/// logged.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); Extension>]
module Logger =
  open System
  open System.Diagnostics
  open Logary
  open NodaTime

  /////////////////////
  // Logging methods //
  /////////////////////

  let private ensureName (logger : Logger) (msg : Message) =
    match msg.name with
    | PointName [||] ->
      Message.setName logger.name msg

    | _  ->
      msg

  let private ifLevel (logger : Logger) level otherwise f =
    if logger.level <= level then
      f ()
    else
      otherwise

  /// Write a message.
  [<CompiledName "Log"; Extension>]
  let log (logger : Logger) msg : Alt<unit> =
    logger.logWithAck (ensureName logger msg)
    |> Alt.afterFun ignore
 
  /// Write a message but don't wait for the message to finish being logged,
  /// instead start the Alt on the Hopac scheduler.
  [<CompiledName "LogSimple"; Extension>]
  let logSimple (logger : Logger) msg : unit =
    Alt.choose [
      log logger msg
      timeOutMillis 5000
    ]
    |> start

  [<CompiledName "LogWithAck"; Extension>]
  let logWithAck (logger : Logger) msg : Alt<Promise<unit>> =
    logger.logWithAck (ensureName logger msg)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Verbose level.
  [<CompiledName "LogVerbose"; Extension>]
  let logVerbose (logger : Logger) msgFactory : Alt<unit> =
    ifLevel logger Verbose (Alt.always ()) <| fun _ ->
      logger.logVerboseWithAck (msgFactory >> ensureName logger)
      |> Alt.afterFun ignore

  [<CompiledName "LogVerboseWithAck"; Extension>]
  let logVerboseWithAck (logger : Logger) fMsg =
    logger.logVerboseWithAck (fMsg >> ensureName logger)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Debug level.
  [<CompiledName "LogDebug"; Extension>]
  let logDebug (logger : Logger) fMessage =
    ifLevel logger Debug (Alt.always ()) <| fun _ ->
      logger.logDebugWithAck (fMessage >> ensureName logger)
      |> Alt.afterFun ignore

  [<CompiledName "LogDebugWithAck"; Extension>]
  let logDebugWithAck (logger : Logger) fMsg =
    logger.logDebugWithAck (fMsg >> ensureName logger)

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds. As an exception to the rule,
  /// it is allowed to pass lastBitPath as null to this function.
  [<CompiledName "Time"; Extension>]
  let time (logger : Logger) lastBitPath f =
    // NOTE: lastBitPath MAY BE NULL!!!
    let setLastBit = function
      | { name = PointName segments } as m
        when not (lastBitPath = null)
          && not (String.isEmpty lastBitPath) ->
        { m with name = PointName (Array.append segments [| lastBitPath |]) }

      | m ->
        m

    try
      let res, message = Message.time logger.name f
      log logger (setLastBit message) |> start
      res
    with _ ->
      reraise ()

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds.
  [<CompiledName "Time"; Extension>]
  let timeSimple (logger : Logger) f =
    time logger null f

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
type NullLogger() =
  let instaPromise =
    Alt.always (Promise (())) // new promise with unit value
  interface Logger with
    member x.logVerboseWithAck messageFactory = instaPromise
    member x.logDebugWithAck messageFactory = instaPromise
    member x.logWithAck message = instaPromise
    member x.logSimple message = ()
    member x.level = Fatal
    member x.name = PointName.ofList [ "Logary"; "Internals"; "NullLogger" ]
