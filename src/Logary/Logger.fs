namespace Logary

open Hopac
open System.Runtime.CompilerServices

/// See the docs on Logger.log for a description on how Ack works in conjunction
/// with the promise.
type Logger =
  inherit Named

  /// Also see `Logger.logDebugWithAck` and `Logger.log`.
  abstract logVerboseWithAck : (unit -> Message) -> Alt<Promise<unit>>

  /// Deferred debug-levelled log; only evaluates the callback if any rule for
  /// applicative to this logger causes its input at debug-level to be handed to
  /// a target.
  ///
  /// Also see `Logger.log`.
  abstract logDebugWithAck : (unit -> Message) -> Alt<Promise<unit>>

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

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged.
  abstract level : LogLevel

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

  [<CompiledName "LogWithAck"; Extension>]
  let logWithAck (logger : Logger) msg : Alt<Promise<unit>> =
    logger.logWithAck (ensureName logger msg)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Verbose level.
  [<CompiledName "LogVerbose"; Extension>]
  let logVerbose (logger : Logger) fMessage : Alt<unit> =
    ifLevel logger Verbose (Alt.always ()) <| fun _ ->
      logger.logVerboseWithAck (fMessage >> ensureName logger)
      |> Alt.afterFun ignore

  [<CompiledName "LogVerboseWithAck"; Extension>]
  let logVerboseWithAck (logger : Logger) fMsg =
    logger.logVerboseWithAck (fMsg >> ensureName logger)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Debug level.
  [<CompiledName "LogDebug"; Extension>]
  let logDebug (logger : Logger) fMessage =
    ifLevel logger Verbose (Alt.always ()) <| fun _ ->
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