module Logary.Suave

open System

open Logary
// deliberatly not opening Suave, to keep types specific

/// Convert a suave log level to a logary log level
let to_logary_level : Suave.Log.LogLevel -> Logary.LogLevel = function
  | Suave.Log.LogLevel.Verbose -> LogLevel.Verbose
  | Suave.Log.LogLevel.Debug   -> LogLevel.Debug
  | Suave.Log.LogLevel.Info    -> LogLevel.Info
  | Suave.Log.LogLevel.Warn    -> LogLevel.Warn
  | Suave.Log.LogLevel.Error   -> LogLevel.Error
  | Suave.Log.LogLevel.Fatal   -> LogLevel.Fatal

/// Convert a Suave LogLine to a Logary LogLine.
let to_logary_line (l : Suave.Log.LogLine) =
  { data          = Map.empty
    message       = l.message
    ``exception`` = l.``exception``
    level         = l.level |> to_logary_level
    tags          = []
    path          = l.path
    timestamp     = NodaTime.Instant.FromDateTimeUtc(DateTime(l.ts_utc_ticks, DateTimeKind.Utc)) }

/// An adapter that takes a Logary logger and forwards all Suave logs to it. A simple implementation:
///
/// if logger.Level >= to_logary_level level then
///   f_line () |> to_logary_line |> Log.log logger
///
type SuaveAdapter(logger : Logger) =
  interface Suave.Log.Logger with
    member x.Log level f_line =
      // here it's important the Level of the logger is well tuned
      if to_logary_level level >= logger.Level then
        f_line () |> to_logary_line |> Logger.log logger
