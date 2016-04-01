namespace EventStore.ClientAPI.Common.Log

// in https://github.com/EventStore/EventStore/tree/dev/src/EventStore.ClientAPI/Common/Log
// are all the defaults

open System
open System.Globalization
open System.Diagnostics
open EventStore.ClientAPI
open Logary
open Hopac

module internal Impl =

  let invariantCulture = CultureInfo.InvariantCulture

  let log (logger : Logger) =
    Message.setName logger.name
    >> Logger.log logger

  let handleInternalException logger (format : string) args (stackTrace : StackTrace) =
    Message.eventFormat (Warn, format, args)
    |> Message.setFieldFromObject "stackFrames" (stackTrace.GetFrames())
    |> Logger.log logger
    |> start

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

  let write'' logger formatProvider format level (ex : exn option) args =
    fmt logger formatProvider format args
    |> Message.event level
    |> (ex |> Option.fold (fun s t -> Message.addExn t) id)
    |> Logger.log logger
    |> start

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