namespace Microsoft.Extensions.Logging

open System
open System.Runtime.CompilerServices
open Logary
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

[<Extension; AutoOpen>]
module LogLevelEx =
  type MLogLevel = Microsoft.Extensions.Logging.LogLevel
  type LLogLevel = Logary.LogLevel

  type Microsoft.Extensions.Logging.LogLevel with
    member x.asLogary =
      match x with
      | MLogLevel.None | MLogLevel.Critical -> LLogLevel.Fatal
      | MLogLevel.Error -> LLogLevel.Error
      | MLogLevel.Warning -> LLogLevel.Warn
      | MLogLevel.Information -> LLogLevel.Info
      | MLogLevel.Debug -> LLogLevel.Debug
      | _ | MLogLevel.Trace -> LogLevel.Verbose

type LogaryLogger(inner: Logger) =
  interface ILogger with
    member x.BeginScope<'TState>(_: 'TState) =
      inner.scoped inner.name :> _

    member x.IsEnabled logLevel =
      inner.level <= logLevel.asLogary

    member x.Log<'TState> (logLevel, eventId, state: 'TState, ex, formatter) =
      // don't log more verbose log messages than we have to
      if inner.level > logLevel.asLogary then () else
      // closure-capture variables and send to Logary
      let message =
        let text = formatter.Invoke(state, ex)
        Model.EventMessage(text, level=logLevel.asLogary)
      inner.log message


type LogaryLoggerFactory(accessor: IHttpContextAccessor, logary: LogManager) =
  interface ILoggerFactory with
    member x.AddProvider provider =
      failwithf "You should not use the ASP.Net providers when logging with Logary. Logary has its own \"providers\" called \"targets\". Have a look at https://github.com/logary/logary and https://logary.tech for documentation or file an issue if your target is missing"

    member x.CreateLogger name =
      match accessor.HttpContext.spanLogger with
      | None ->
        let inner = logary.getLogger name
        LogaryLogger(inner) :> ILogger
      | Some logger -> // here we have a nice span logger! 😋
        LogaryLogger(logger) :> ILogger

    member x.Dispose () = ()


type LogaryStartupFilter() =
  interface IStartupFilter with
    member x.Configure(next: Action<IApplicationBuilder>) =
      let build (next: Action<IApplicationBuilder>) (b: IApplicationBuilder) =
        b.UseMiddleware<LogaryMiddleware>() |> ignore
        next.Invoke b
      Action<_>(build next)


module internal Impl =
  let addServices (logary: LogManager) _ (s: IServiceCollection) =
    s.AddHttpContextAccessor()
     .AddSingleton(logary)
     .AddSingleton(logary.getLogger(logary.runtimeInfo.service))
     .AddTransient<IStartupFilter, LogaryStartupFilter>()
     .AddSingleton<ILoggerFactory, LogaryLoggerFactory>()
    |> ignore


// EITHER use:
[<Extension; AutoOpen>]
module IHostBuilderEx =
  open Impl

  [<Extension; CompiledName "UseLogary">]
  let useLogary (builder: IHostBuilder, logary: LogManager) =
     let cb = Action<_,_>(addServices logary)
     builder.ConfigureServices(cb)

  type IHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)


// OR, you can use:
[<Extension; AutoOpen>]
module IWebHostBuilderEx =
  open Impl

  [<Extension; CompiledName "UseLogary">]
  let useLogary (builder: IWebHostBuilder, logary: LogManager) =
     let cb = Action<_>(addServices logary ())
     builder.ConfigureServices(cb)

  type IWebHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)