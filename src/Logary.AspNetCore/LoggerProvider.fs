namespace Logary.AspNetCore

open Microsoft.Extensions.Logging
open Logary
open System.Threading
open Logary.AspNetCore.LoggerAdapter
open System

[<ProviderAlias("Logary")>]
type LogaryLoggerProvider(m: LogManager, needDispose: bool) =


  interface ISupportExternalScope with
    member x.SetScopeProvider (scopeProvider: IExternalScopeProvider) =
      m.wrapScope {
        new ILogScope with
          member x.collect () = 
            let mutableStateList = new ResizeArray<obj>()
            do scopeProvider.ForEachScope(new Action<obj,ResizeArray<obj>>(fun data state -> state.Add(data)), mutableStateList)
            mutableStateList |> List.ofSeq
          member x.push lazyData = scopeProvider.Push(lazyData.Value)
          member x.wrap scope = ()
      } 
    
  interface ILoggerProvider with
    member x.CreateLogger (name: string) : ILogger =
      new LoggerAdaption(name, m) :> ILogger
      
    member x.Dispose () =
      if needDispose then
        // dispose logary
        let shutdownTask = m.shutdown() |> Logary.CSharp.Alt.toTask CancellationToken.None
        shutdownTask.ConfigureAwait(false) |> ignore
        shutdownTask.Result
      else 
        do ()


