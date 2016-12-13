namespace FsSql

open Hopac
open Hopac.Infixes
open Logary

/// Some mapping functions for FsSql LogLevels
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
    |> Message.setName (if l.path <> "" then PointName.parse l.path else PointName.ofSingle "FsSql")
    |> Message.setUTCTicks l.timestamp

type FsSqlAdapter(logger : Logger) =
  interface FsSql.Logging.Logger with
    member x.Verbose lineFactory =
      ignore >> lineFactory >> FsSqlLogLine.toLogary
      |> Logger.logWithTimeout logger Logger.defaultTimeout Verbose
      |> fun x -> start (x ^->. ())

    member x.Debug lineFactory =
      ignore >> lineFactory >> FsSqlLogLine.toLogary
      |> Logger.logWithTimeout logger Logger.defaultTimeout Debug
      |> fun x -> start (x ^->. ())

    member x.Log message =
      message
      |> FsSqlLogLine.toLogary
      |> logger.logSimple
