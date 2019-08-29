namespace Logary.Adapters.AspNetCore
open Logary
open Logary.Configuration
open Microsoft.Extensions.Logging
open Hopac

[<ProviderAlias("Logary")>]
type LogaryLoggerProvider(logary: LogManager, shutdownOnDispose: bool) =
  interface ILoggerProvider with
    member x.CreateLogger (name: string): ILogger =
      let logger = logary.getLogger name
      new LoggerAdapter(logger) :> ILogger

    member x.Dispose () =
      if shutdownOnDispose then
        let task = logary.shutdown () |> Hopac.startAsTask
        task.ConfigureAwait(false).GetAwaiter().GetResult()
      else
        do ()


