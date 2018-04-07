namespace Logary.AspNetCore

open System
open Microsoft.Extensions.Logging
open Logary
open System.Collections.Generic;
open Logary.Internals.Aether.Optics

module LoggerAdapter =

  type MSLogLevel = Microsoft.Extensions.Logging.LogLevel

  let mapLogLevel (level: MSLogLevel) =
    match level with
    | MSLogLevel.Trace -> Verbose
    | MSLogLevel.Information -> Info
    | MSLogLevel.Debug -> Debug
    | MSLogLevel.Warning -> Warn
    | MSLogLevel.Error -> Error
    | MSLogLevel.Critical -> Fatal
    | MSLogLevel.None -> Fatal
    | _ -> Fatal
  


  type LoggerAdaption(name: string, logManager: LogManager) =
    let logger = logManager.getLogger (PointName.parse name)
    
    let processEvent (eventId: EventId) m =
      let eventIdInfo = string eventId
      if eventIdInfo <> "0" then 
        m |> Message.setContext "event-id" eventIdInfo
      else m

    let processState (state: 't) (ex: exn) (formatter: Func<'t,exn,string>) m =
      match box state with
      | :? IEnumerable<KeyValuePair<string, obj>> as kvSeq ->
        let messageTpl = kvSeq |> Seq.tryFindBack (fun kv -> kv.Key = "{OriginalFormat}") |> Option.map (fun kv -> kv.Value)
        let fields = kvSeq |> Seq.map (fun kv -> kv.Value ) |> Array.ofSeq 
        match messageTpl with
        | Some tpl -> m |> Message.setEvent tpl |> Message.setFields fields
        | None -> m |> Message.setField "state" state

      | _ ->
        if isNull formatter then
          m |> Message.setField "state" state
        else
          let customFormated = formatter.Invoke(state, ex)
          m |> Message.setField "state" customFormated


    interface ILogger with
      member x.BeginScope<'t> (state: 't) : IDisposable =
        logManager.beginScope (lazy(box state))

      member x.IsEnabled (level: MSLogLevel) =
        if level = MSLogLevel.None then false
        else mapLogLevel level >= logger.level

      member x.Log<'t> (level: MSLogLevel, eventId: EventId, state: 't, ex: exn, formatter: Func<'t,exn,string>) =
        Message.eventX"{state}"
        >> Message.addExn ex
        >> processEvent eventId
        >> processState state ex formatter
        |> logger.logWith (mapLogLevel level)