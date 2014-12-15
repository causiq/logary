namespace EventStore.ClientAPI.Common.Log

// in https://github.com/EventStore/EventStore/tree/dev/src/EventStore.ClientAPI/Common/Log
// are all the defaults

open System
open System.Globalization

open EventStore.ClientAPI

open Logary

module internal Impl =

  let invariantCulture = CultureInfo.InvariantCulture

  let log (logger : Logger) =
    LogLine.setPath logger.Name
    >> Logger.log logger

  let fmt formatProvider format args = String.Format(formatProvider, format, args)

  let write'' logger formatProvider format level ex args =
    fmt formatProvider format args
    |> LogLine.create' level
    |> fun line ->
      match ex with
      | None -> line
      | Some ex -> line |> LogLine.setExn ex
    |> Logger.log logger

open Impl

/// Adapter that logs into logary. Usage:
///
/// ```
/// open EventStore.ClientAPI
/// open EventStore.ClientAPI.Common.Log // here's the logger
/// //
/// connBuilder.UseCustomLogger(lm.GetLogger("EventStore"))
/// ```
///
/// Happy logging!
type LogaryLogger(logger : Logger) =
  let write'' = write'' logger
  interface ILogger with
    member x.Error (format, args) =
      write'' invariantCulture format Error None args
    member x.Error (ex, format, args) =
      write'' invariantCulture format Error (Some ex) args
    member x.Debug(format, args) =
      write'' invariantCulture format Debug None args
    member x.Debug(ex, format, args) =
      write'' invariantCulture format Debug (Some ex) args
    member x.Info(format, args) =
      write'' invariantCulture format Info None args
    member x.Info(ex, format, args) =
      write'' invariantCulture format Info (Some ex) args