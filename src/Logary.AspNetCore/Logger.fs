namespace Logary.AspNetCore

open System
open Microsoft.Extensions.Logging
open Logary
open System.Collections.Generic;
open Logary.Internals.Aether.Optics

module LoggerAdapter =

  type msLogLevel = Microsoft.Extensions.Logging.LogLevel

  let mapLogLevel (level: msLogLevel) =
    match level with
    | msLogLevel.Trace -> Verbose
    | msLogLevel.Information -> Info
    | msLogLevel.Debug -> Debug
    | msLogLevel.Warning -> Warn
    | msLogLevel.Error -> Error
    | msLogLevel.Critical -> Fatal
    | msLogLevel.None -> Fatal
    | _ -> Fatal
  


  type LoggerAdaption(name: string, logManager: LogManager, scopeProvider: IExternalScopeProvider) =
    let logger = logManager.getLogger (PointName.parse name)
    
    let processEvent (eventId: EventId) m =
      let eventIdInfo = string eventId
      if eventIdInfo <> "0" then 
        m |> Message.setContext "EventId" eventIdInfo
      else m

    let processState (state: 't) (ex: exn) (formatter: Func<'t,exn,string>) m =
      match box state with
      | :? IEnumerable<KeyValuePair<string, obj>> as kvSeq ->
        let messageTpl = kvSeq |> Seq.tryFindBack (fun kv -> kv.Key = "{OriginalFormat}") |> Option.map (fun kv -> kv.Value)
        let fields = kvSeq |> Seq.map (fun kv -> kv.Value ) |> Array.ofSeq 
        match messageTpl with
        | Some tpl -> m |> Message.setEvent tpl |> Message.setFields fields
        | None -> m |> Message.setField "State" state

      | _ ->
        if isNull formatter then
          m |> Message.setField "State" state
        else
          let customFormated = formatter.Invoke(state, ex)
          m |> Message.setField "State" customFormated


    interface ILogger with
      member x.BeginScope<'t> (state: 't) : IDisposable =
        let d1 = logManager.beginScope String.Empty (lazy(state))
        let d2 = scopeProvider.Push state
        {
          new IDisposable with
            member x.Dispose () =
              do d1.Dispose()
              do d2.Dispose()
        }

      member x.IsEnabled (level: msLogLevel) = 
        if level = msLogLevel.None then false
        else mapLogLevel level >= logger.level

      member x.Log<'t> (level: msLogLevel, eventId: EventId, state: 't, ex: exn, formatter: Func<'t,exn,string>) =
        if not ((x :> ILogger).IsEnabled level) then
          do ()
        else
          Message.event (mapLogLevel level) "{State}"
          |> Message.addExn ex
          |> processEvent eventId
          |> processState state ex formatter
          |> logger.logSimple