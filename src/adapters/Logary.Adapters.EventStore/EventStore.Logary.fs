namespace EventStore.ClientAPI.Common.Log

// in https://github.com/EventStore/EventStore/tree/dev/src/EventStore.ClientAPI/Common/Log
// are all the defaults

open System
open System.Globalization
open System.Diagnostics

open EventStore.ClientAPI

open Logary

module internal Impl =

  let invariantCulture = CultureInfo.InvariantCulture

  let log (logger : Logger) =
    LogLine.setPath logger.Name
    >> Logger.log logger

  let handle_internal_exception logger format args stackTrace =
    LogLine.error "String.Format exception"
    |> LogLine.setDatas [
      "stack_trace", box stackTrace
      "format", box format
      "args", box args
    ]
    |> Logger.log logger

  let fmt (internal_logger : Logger) formatProvider format (args : obj []) =
    let rec fmt' failed =
      try
        if not failed && args.Length = 0 then format
        elif not failed then String.Format(formatProvider, format, args)
        else
          let st = new StackTrace(true)
          handle_internal_exception internal_logger format args st
          format
      with
        | :? FormatException ->
          fmt' true
        | :? ArgumentNullException ->
          fmt' true
    fmt' false

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