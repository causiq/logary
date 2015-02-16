namespace Suave.Logging

open Logary

/// Some mapping functions for Suave LogLevels
module SuaveLogLevel =
  open System

  // deliberatly not opening Suave, to keep types specific

  /// Convert a suave log level to a logary log level
  let to_logary : Suave.Logging.LogLevel -> Logary.LogLevel = function
    | Suave.Logging.LogLevel.Verbose -> LogLevel.Verbose
    | Suave.Logging.LogLevel.Debug   -> LogLevel.Debug
    | Suave.Logging.LogLevel.Info    -> LogLevel.Info
    | Suave.Logging.LogLevel.Warn    -> LogLevel.Warn
    | Suave.Logging.LogLevel.Error   -> LogLevel.Error
    | Suave.Logging.LogLevel.Fatal   -> LogLevel.Fatal

module SuaveLogLine =
  open System

  open NodaTime

  /// Convert a Suave LogLine to a Logary LogLine.
  let to_logary (l : Suave.Logging.LogLine) =
    { data          = Map.empty
      message       = l.message
      ``exception`` = l.``exception``
      level         = l.level |> SuaveLogLevel.to_logary
      tags          = []
      path          = l.path
      timestamp     =  NodaTime.Instant.FromDateTimeUtc(DateTime(l.ts_utc_ticks, DateTimeKind.Utc)) }

/// An adapter that takes a Logary logger and forwards all Suave logs to it. A simple implementation:
///
/// if logger.Level >= to_logary_level level then
///   f_line () |> to_logary_line |> Log.log logger
///
type SuaveAdapter(logger : Logger) =
  interface Suave.Logging.Logger with
    member x.Log level f_line =
      // here it's important the Level of the logger is well tuned
      if SuaveLogLevel.to_logary level >= logger.Level then
        f_line () |> SuaveLogLine.to_logary |> Logger.log logger
