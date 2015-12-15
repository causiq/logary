namespace Logary

open Hopac

/// See the docs on Logger.log for a description on how Ack works in conjunction
/// with the promise.
type Logger =
  inherit Named

  /// Also see `Logger.log`.
  abstract logVerbose        : (unit -> Message) -> Alt<unit>

  /// Also see `Logger.logDebugWithAck` and `Logger.log`.
  abstract logVerboseWithAck : (unit -> Message) -> Alt<Promise<unit>>

  /// Also see `Logger.log`.
  abstract logDebug          : (unit -> Message) -> Alt<unit>

  /// Deferred debug-levelled log; only evaluates the callback if any rule for
  /// applicative to this logger causes its input at debug-level to be handed to
  /// a target.
  ///
  /// Also see `Logger.log`.
  abstract logDebugWithAck   : (unit -> Message) -> Alt<Promise<unit>>

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

  /// Write a message to the Logger. The returned value represents the commit
  /// point that Logary has acquired the message. The alternative is always
  /// selectable (through `Alt.always ()` if the logger filtered out the message
  /// due to a Rule).
  abstract log        : Message -> Alt<unit>

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged.
  abstract level      : LogLevel

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Logger =
  open System
  open Logary

  /////////////////////
  // Logging methods //
  /////////////////////

  let private ensureName (logger : Logger) (msg : Message) =
    match msg.name with
    | PointName [] -> 
      Message.setName logger.name msg
      
    | _  ->
      msg

  /// Write a message.
  [<CompiledName "Log">]
  let log (logger : Logger) msg =
    logger.log (ensureName logger msg)

  [<CompiledName "LogWithAck">]
  let logWithAck (logger : Logger) msg =
    logger.logWithAck (ensureName logger msg)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Verbose level.
  [<CompiledName "LogVerbose">]
  let logVerbose (logger : Logger) fMsg =
    logger.logVerbose (fMsg >> ensureName logger)

  [<CompiledName "LogVerboseWithAck">]
  let logVerboseWithAck (logger : Logger) fMsg =
    logger.logVerboseWithAck (fMsg >> ensureName logger)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Debug level.
  [<CompiledName "LogDebug">]
  let logDebug (logger : Logger) fMsg =
    logger.logDebug (fMsg >> ensureName logger)

  [<CompiledName "LogDebugWithAck">]
  let logDebugWithAck (logger : Logger) fMsg =
    logger.logDebugWithAck (fMsg >> ensureName logger)

  /// Write a verbose log entry to the logger
  [<CompiledName "Verbose">]
  let verbose logger = log logger << Message.event LogLevel.Verbose

  /// Write a debug log entry to the logger
  [<CompiledName "Debug">]
  let debug logger = log logger << Message.event LogLevel.Debug

  /// Write an info log entry to the logger
  [<CompiledName "Info">]
  let info logger = log logger << Message.event LogLevel.Info

  /// Write a warn log entry
  [<CompiledName "Warn">]
  let warn logger = log logger << Message.event LogLevel.Warn

  /// Write an error log entry
  [<CompiledName "Error">]
  let error logger = log logger << Message.event LogLevel.Error

  /// Write a fatal log entry
  [<CompiledName "Fatal">]
  let fatal logger = log logger << Message.event LogLevel.Fatal
