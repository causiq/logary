namespace logibit.hawk

/// Some mapping functions for Hawk LogLevels
module internal HawkLogLevel =
  open System
  open Logary

  // deliberatly not opening Hawk, to keep types specific

  /// Convert a suave log level to a logary log level
  let toLogary : logibit.hawk.Logging.LogLevel -> LogLevel = function
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
  open Logary.Operators

  /// Convert a Suave LogLine to a Logary LogLine.
  let toLogary (l : logibit.hawk.Logging.LogLine) : Message =
    Message.event (HawkLogLevel.toLogary l.level) l.message
    |> Message.setName (PointName.parse l.path)
    |> Message.setTicks l.timestamp.Ticks

open Logary

type HawkAdapter(logger : Logger) =
  interface logibit.hawk.Logging.Logger with
    member x.Verbose fLine =
      (fLine >> HawkLogLine.toLogary) |> Logger.logVerbose logger
    member x.Debug fLine =
      (fLine >> HawkLogLine.toLogary |> Logger.logDebug logger)
    member x.Log line =
      line |> HawkLogLine.toLogary |> Logger.log logger