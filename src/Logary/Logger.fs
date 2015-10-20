namespace Logary

open Logary.DataModel

type Logger =
  inherit Named

  /// Write a Verbose log line
  abstract LogVerbose : (unit -> Message) -> unit

  /// Write a Debug log line
  abstract LogDebug   : (unit -> Message) -> unit

  /// Write a log line to the logger.
  abstract Log        : Message -> unit

  /// Write a measure to the logger.
  abstract Measure    : Message -> unit

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged
  abstract Level      : LogLevel

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Logger =
  open System

  open Logary
  open Logary.DataModel

  /////////////////////
  // Logging methods //
  /////////////////////

  let private setContext (logger : Logger) (msg : Message) =
    Message.Context.serviceSet logger.Name msg

  /// Write a log entry from a log line.
  [<CompiledName "Log">]
  let log (logger : Logger) msg =
    logger.Log (setContext logger msg)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Verbose level.
  [<CompiledName "LogVerbose">]
  let logVerbose (logger : Logger) fMsg =
    logger.LogVerbose (fMsg >> setContext logger)

  /// Write a debug log line, given from the fLine callback, if the logger
  /// accepts line with Debug level.
  [<CompiledName "LogDebug">]
  let logDebug (logger : Logger) fMsg =
    logger.LogDebug (fMsg >> setContext logger)

  /// Write a measure
  [<CompiledName "Measure">]
  let ``measure`` (logger : Logger) msg =
    logger.Measure (setContext logger msg)

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
