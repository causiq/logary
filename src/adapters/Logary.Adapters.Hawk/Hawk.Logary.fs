namespace Logibit.Hawk

/// Some mapping functions for Hawk LogLevels
module internal HawkLogLevel =
  open System
  open Logary

  // deliberatly not opening Hawk, to keep types specific

  /// Convert a suave log level to a logary log level
  let toLogary : Logibit.Hawk.Logging.LogLevel -> LogLevel = function
    | Logibit.Hawk.Logging.LogLevel.Verbose -> LogLevel.Verbose
    | Logibit.Hawk.Logging.LogLevel.Debug   -> LogLevel.Debug
    | Logibit.Hawk.Logging.LogLevel.Info    -> LogLevel.Info
    | Logibit.Hawk.Logging.LogLevel.Warn    -> LogLevel.Warn
    | Logibit.Hawk.Logging.LogLevel.Error   -> LogLevel.Error
    | Logibit.Hawk.Logging.LogLevel.Fatal   -> LogLevel.Fatal

module internal HawkLogLine =
  open System
  open NodaTime
  open Logary
  open Logary.Operators

  /// Convert a Suave LogLine to a Logary LogLine.
  let toLogary (l : Logibit.Hawk.Logging.LogLine) : Message =
    Message.event (HawkLogLevel.toLogary l.level) l.message
    |> Message.setName (PointName.parse l.path)
    |> Message.setFieldsFromMap l.data
    |> Message.setTicks l.timestamp.Ticks

open Logary
open Hopac

type HawkAdapter(logger : Logger) =
  interface Logibit.Hawk.Logging.Logger with
    member x.Verbose fLine =
      (fLine >> HawkLogLine.toLogary)
      |> Logger.logVerbose logger
      |> queue

    member x.Debug fLine =
      fLine >> HawkLogLine.toLogary
      |> Logger.logDebug logger
      |> queue

    member x.Log line =
      line
      |> HawkLogLine.toLogary
      |> Logger.log logger
      |> queue