namespace FsSql

open Hopac
open Logary

/// Some mapping functions for Hawk LogLevels
module internal FsSqlLogLevel =
  open System

  /// Convert a suave log level to a logary log level
  let toLogary : FsSql.Logging.LogLevel -> LogLevel = function
    | FsSql.Logging.LogLevel.Verbose -> LogLevel.Verbose
    | FsSql.Logging.LogLevel.Debug   -> LogLevel.Debug
    | FsSql.Logging.LogLevel.Info    -> LogLevel.Debug
    | FsSql.Logging.LogLevel.Warn    -> LogLevel.Warn
    | FsSql.Logging.LogLevel.Error   -> LogLevel.Error
    | FsSql.Logging.LogLevel.Fatal   -> LogLevel.Fatal

module internal FsSqlLogLine =
  open System
  open NodaTime

  /// Convert a Suave LogLine to a Logary LogLine.
  let toLogary (l : FsSql.Logging.LogLine) =
    Message.event (FsSqlLogLevel.toLogary l.level) l.message
    |> Message.setName (PointName.parse l.path)
    |> Message.setUTCTicks l.timestamp

type FsSqlAdapter(logger : Logger) =
  interface FsSql.Logging.Logger with
    member x.Verbose lineFactory =
      ignore >> lineFactory >> FsSqlLogLine.toLogary
      |> Logger.logVerbose logger
      |> start

    member x.Debug lineFactory =
      ignore >> lineFactory >> FsSqlLogLine.toLogary
      |> Logger.logDebug logger
      |> start

    member x.Log line =
      line
      |> FsSqlLogLine.toLogary
      |> Logger.log logger
      |> start