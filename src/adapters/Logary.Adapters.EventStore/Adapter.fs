namespace Logary.Adapters.EventStore

open System
open System.Globalization
open System.Diagnostics
open EventStore.ClientAPI
open Logary
open Logary.Message
open Hopac

module internal Impl =
  let invariantCulture = CultureInfo.InvariantCulture

  let handleInternalException logger (format: string) args (stackTrace: StackTrace) =
    Logger.logSimple logger (
      Message.eventFormat (Warn, format, args)
      |> setField "stackFrames" (stackTrace.GetFrames()))

  let formatWith (internalLogger: Logger) level format (args: obj []) =
    let rec formatInner failed =
      try
        if not failed && args.Length = 0 then Message.event level format
        elif not failed then Message.eventFormat(format, args)
        else
          let st = new StackTrace(true)
          handleInternalException internalLogger format args st
          Message.event Info format
      with
      | :? FormatException -> formatInner true
      | :? ArgumentNullException -> formatInner true

    formatInner false

  let logTo (logger: Logger) format level (ex: exn option) args =
    let m =
      let inner = formatWith logger level format args
      ex |> Option.fold (fun s t -> addExn t s) inner
    logger.logSimple m

open Impl

/// Adapter that logs into Logary. Usage:
///
/// ```
/// open EventStore.ClientAPI
/// open Logary.Adapters.EventStore // here's the logger
/// //
/// connBuilder.UseCustomLogger(EventStoreAdapter(lm.GetLogger("EventStore")))
/// ```
///
/// Happy logging!
type EventStoreAdapter(logger: Logger) =
  let logTo = logTo logger
  interface ILogger with
    member x.Error (format, args) =
      logTo format Error None args
    member x.Error (ex, format, args) =
      logTo format Error (Some ex) args
    member x.Debug(format, args) =
      logTo format Debug None args
    member x.Debug(ex, format, args) =
      logTo format Debug (Some ex) args
    member x.Info(format, args) =
      logTo format Info None args
    member x.Info(ex, format, args) =
      logTo format Info (Some ex) args