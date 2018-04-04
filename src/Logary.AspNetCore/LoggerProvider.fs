namespace Logary.AspNetCore

open Microsoft.Extensions.Logging
open Logary
open System.Threading
open Logary.AspNetCore.LoggerAdapter

[<ProviderAlias("Logary")>]
type LogaryLoggerProvider(m: LogManager, needDispose: bool) =

  let mutable (provider: IExternalScopeProvider) = null

  interface ISupportExternalScope with
    member x.SetScopeProvider (scopeProvider: IExternalScopeProvider) = provider <- scopeProvider
    
  interface ILoggerProvider with
    member x.CreateLogger (name: string) : ILogger =
      let scopeProvider =
        if isNull provider then 
          new LoggerExternalScopeProvider() :> IExternalScopeProvider
        else provider
      new LoggerAdaption(name, m, scopeProvider) :> ILogger
      
    member x.Dispose () =
      if needDispose then
        // dispose logary
        let shutdownTask = m.shutdown() |> Logary.CSharp.Alt.toTask CancellationToken.None
        shutdownTask.ConfigureAwait(false) |> ignore
        shutdownTask.Result
      else 
        do ()


