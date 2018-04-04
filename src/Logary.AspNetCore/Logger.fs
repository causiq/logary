namespace Logary.AspNetCore

open System
open Microsoft.Extensions.Logging
open Logary

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
        if not x.IsEnabled level then do ()
        else

      
        ()