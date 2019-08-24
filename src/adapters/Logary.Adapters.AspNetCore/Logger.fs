namespace Logary.Adapters.AspNetCore

open System
open Microsoft.Extensions.Logging
open Logary
open System.Collections.Generic;

[<AutoOpen>]
module LoggerAdapter =

  type MSLogLevel = Microsoft.Extensions.Logging.LogLevel

  type Microsoft.Extensions.Logging.LogLevel with
    member x.logaryLevel =
      match x with
      | MSLogLevel.Trace -> Verbose
      | MSLogLevel.Information -> Info
      | MSLogLevel.Debug -> Debug
      | MSLogLevel.Warning -> Warn
      | MSLogLevel.Error -> Error
      | MSLogLevel.Critical -> Fatal
      | MSLogLevel.None -> Fatal
      | _ -> Fatal


  type LoggerAdaption(name: string, logger: Logger) =
    let processEvent (eventId: EventId) m =
      let eventIdInfo = string eventId
      if eventIdInfo <> "0" then
        m |> Message.setContext "event-id" eventIdInfo
      else m

    let processState (state: 't) (ex: exn) (formatter: Func<'t, exn, string>) m =
      match box state with
      | :? IEnumerable<KeyValuePair<string, obj>> as kvSeq ->
        let kvList = List.ofSeq kvSeq
        let messageTpl = kvList |> List.tryFindBack (fun kv -> kv.Key = "{OriginalFormat}") |> Option.map (fun kv -> kv.Value.ToString())
        let fields = kvList |> List.map (fun kv -> kv.Value ) |> Array.ofSeq
        match messageTpl with
        | Some tpl ->
          m |> Message.setEvent tpl |> Message.setFields fields
        | None ->
          if isNull formatter then
            m |> Message.setField "state" kvList
          else
            let customFormatted = formatter.Invoke(state, ex)
            m |> Message.setEvent customFormatted |> Message.setContext "state" kvList
      | _ ->
        if isNull formatter then
          m |> Message.setField "state" state
        else
          let customFormatted = formatter.Invoke(state, ex)
          m |> Message.setEvent customFormatted |> Message.setContext "state" state


    interface ILogger with
      member x.BeginScope<'t> (state: 't): IDisposable =
        logger.startSpan("Scope with {state}", transform=processState state null null)
        :> IDisposable

      member x.IsEnabled (level: MSLogLevel) =
        if level = MSLogLevel.None then false
        else level.logaryLevel >= logger.level

      member x.Log<'t> (level: MSLogLevel, eventId: EventId, state: 't, ex: exn, formatter: Func<'t,exn,string>) =
        Message.eventX "{state}"
        >> if isNull ex then id else Message.addExn ex
        >> processEvent eventId
        >> processState state ex formatter
        |> logger.logWith level.logaryLevel
