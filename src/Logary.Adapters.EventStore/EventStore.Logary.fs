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
  
  let handle_internal_exception logger ex text =
        LogLine.error text
        |> LogLine.setExn ex
        |> Logger.log logger
        |> ignore
        text

  let fmt (internal_logger : Logger) formatProvider format args = 
    try
      String.Format(formatProvider, format, args)
    with
      | :? FormatException as ex ->
        handle_internal_exception internal_logger ex "EventStore.Logary.String.FormatException"
      | :? ArgumentNullException as ex ->
        handle_internal_exception internal_logger ex "EventStore.Logary.String.ArgumentNullException"
        
  let write'' logger internal_logger formatProvider format level ex args =
    fmt internal_logger formatProvider format args
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
type EventStoreAdapter(logger : Logger, internal_logger : Logger) =
  let write'' = write'' logger internal_logger
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