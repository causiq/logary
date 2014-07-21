namespace Logary

/// Main interface used to log LogLines and Metrics.
type logger =
  inherit Named

  /// Write a log line to the logger.
  abstract Log     : logline -> unit

  /// Write a measure to the logger.
  abstract Measure : ``measure`` -> unit

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged
  abstract Level   : LogLevel

/// API for writing logs and measures.
/// For gauges that run continuously based on a timer, have a look at
/// the Registry module. To get a logger, have a look at the Logger module.
///
/// If you are using I recommend doing:
/// `Install-Package Intelliplan.Logary.CSharp` instead of dealing with the
/// interop problems that you will get from using this module directly.
module Logger =
  open System.Diagnostics

  open NodaTime

  open Logary
  open Logary.LogLine

  /////////////////////
  // Logging methods //
  /////////////////////

  /// Write a log entry from a log line.
  [<CompiledName "Log">]
  let log (logger : logger) line =
    (line : logline)
    |> fun l -> match l.path with "" -> { l with path = logger.Name } | _ -> l
    |> logger.Log

  /// Write a verbose log entry to the logger
  [<CompiledName "Verbose">]
  let verbose logger = log logger << create' Verbose

  /// Write a debug log entry to the logger
  [<CompiledName "Debug">]
  let debug logger = log logger << create' Debug

  /// Write an info log entry to the logger
  [<CompiledName "Info">]
  let info logger = log logger << create' Info

  /// Write a warn log entry
  [<CompiledName "Warn">]
  let warn logger = log logger << create' Warn

  /// Write an error log entry
  [<CompiledName "Error">]
  let error logger = log logger << create' Error

  /// Write a fatal log entry
  [<CompiledName "Fatal">]
  let fatal logger = log logger << create' Fatal
