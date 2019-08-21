namespace Logary.Adapters.AspNetCore
open Logary
open Logary.Configuration
open Microsoft.Extensions.Logging
open Hopac

[<ProviderAlias("Logary")>]
type LogaryLoggerProvider(m: LogManager, needDispose: bool) =
  interface ILoggerProvider with
    member x.CreateLogger (name: string): ILogger =
      let logger = m.getLogger name
      new LoggerAdaption(name, logger) :> ILogger

    member x.Dispose () =
      eprintfn "test"

      if needDispose then
        // dispose logary
        (m.shutdown () |> Hopac.startAsTask).ConfigureAwait(false).GetAwaiter().GetResult()
      else
        do ()


