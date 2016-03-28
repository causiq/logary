namespace Suave.Logging

open Logary
open Hopac

/// Some mapping functions for Suave LogLevels
module SuaveLogLevel =
  open System

  // deliberatly not opening Suave, to keep types specific

  /// Convert a suave log level to a logary log level
  let toLogary : Suave.Logging.LogLevel -> Logary.LogLevel = function
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
  let toLogary (l : Suave.Logging.LogLine) : Message =
    Message.event (SuaveLogLevel.toLogary l.level) l.message
    |> Message.setName (PointName.parse l.path)
    |> Message.setTicks l.tsUTCTicks
    |> (l.``exception`` |> Option.fold (fun s t -> Message.addExn t) id)

/// An adapter that takes a Logary logger and forwards all Suave logs to it. A simple implementation:
///
/// if logger.Level >= toLogaryLevel level then
///   fLine () |> toLogary |> Log.log logger
///
type SuaveAdapter(logger : Logger) =
  interface Suave.Logging.Logger with
    member x.Log level fLine =
      // here it's important the Level of the logger is well tuned
      fLine >> SuaveLogLine.toLogary
      |> Logger.logDebug logger
      |> queue