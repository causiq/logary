namespace logibit.hawk

/// Some mapping functions for Hawk LogLevels
module internal HawkLogLevel =
  open System

  open Logary

  // deliberatly not opening Hawk, to keep types specific

  /// Convert a suave log level to a logary log level
  let to_logary : logibit.hawk.Logging.LogLevel -> LogLevel = function
    | logibit.hawk.Logging.LogLevel.Verbose -> LogLevel.Verbose
    | logibit.hawk.Logging.LogLevel.Debug   -> LogLevel.Debug
    | logibit.hawk.Logging.LogLevel.Info    -> LogLevel.Info
    | logibit.hawk.Logging.LogLevel.Warn    -> LogLevel.Warn
    | logibit.hawk.Logging.LogLevel.Error   -> LogLevel.Error
    | logibit.hawk.Logging.LogLevel.Fatal   -> LogLevel.Fatal

module internal HawkLogLine =
  open System

  open NodaTime

  open Logary

  /// Convert a Suave LogLine to a Logary LogLine.
  let to_logary (l : logibit.hawk.Logging.LogLine) =
    { data          = l.data
      message       = l.message
      ``exception`` = None
      level         = l.level |> HawkLogLevel.to_logary
      tags          = []
      path          = l.path
      timestamp     = l.timestamp }

open Logary

type HawkAdapter(logger : Logger) =
  interface logibit.hawk.Logging.Logger with
    member x.Verbose f_line =
      if Verbose >= logger.Level then
        f_line () |> HawkLogLine.to_logary |> Logger.log logger
    member x.Debug f_line =
      if Debug >= logger.Level then
        f_line () |> HawkLogLine.to_logary |> Logger.log logger
    member x.Log line =
      if HawkLogLevel.to_logary line.level >= logger.Level then
        line |> HawkLogLine.to_logary |> Logger.log logger
