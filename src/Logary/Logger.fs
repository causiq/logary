namespace Logary

type Logger =
  inherit Named

  /// Write a Verbose message
  abstract logVerbose : (unit -> Message) -> unit

  /// Write a Debug message
  abstract logDebug   : (unit -> Message) -> unit

  /// Write a message to the logger.
  abstract log        : Message -> unit

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged
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

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Verbose level.
  [<CompiledName "LogVerbose">]
  let logVerbose (logger : Logger) fMsg =
    logger.logVerbose (fMsg >> ensureName logger)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Debug level.
  [<CompiledName "LogDebug">]
  let logDebug (logger : Logger) fMsg =
    logger.logDebug (fMsg >> ensureName logger)

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
