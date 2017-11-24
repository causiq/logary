namespace EventStore.ClientAPI.Common.Log

// in https://github.com/EventStore/EventStore/tree/dev/src/EventStore.ClientAPI/Common/Log
// are all the defaults

open System
open System.Globalization
open System.Diagnostics
open EventStore.ClientAPI
open Logary
open Logary.Message
open Hopac

module internal Impl =

  let invariantCulture = CultureInfo.InvariantCulture

  let handleInternalException logger (format : string) args (stackTrace : StackTrace) =
    Logger.logSimple logger (
      eventFormat (Warn, format, args)
      |> setContext "stackFrames" (stackTrace.GetFrames())
    )

  let fmt (internalLogger : Logger) formatProvider format (args : obj []) =
    let rec fmt' failed =
      try
        if not failed && args.Length = 0 then format
        elif not failed then String.Format(formatProvider, format, args)
        else
          let st = new StackTrace(true)
          handleInternalException internalLogger format args st
          format
      with
        | :? FormatException ->
          fmt' true

        | :? ArgumentNullException ->
          fmt' true

    fmt' false

  let write'' (logger : Logger) formatProvider format level (ex : exn option) args =
    logger.logSimple (
      Message.event level (fmt logger formatProvider format args)
      |> (ex |> Option.fold (fun s -> addExn) id))

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
type EventStoreAdapter(logger : Logger) =
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