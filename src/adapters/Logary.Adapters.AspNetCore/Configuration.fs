namespace Logary.Configuration

open Logary
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging

/// Adds the Logary middlewares
type LogaryStartupFilter() =
  interface IStartupFilter with
    member x.Configure(next: Action<IApplicationBuilder>) =
      let build (next: Action<IApplicationBuilder>) (builder: IApplicationBuilder) =
        // Alternative to https://docs.microsoft.com/en-us/aspnet/core/fundamentals/middleware/write?view=aspnetcore-3.1
        builder
          .UseMiddleware<CaptureRouteDataMiddleware>()
          .UseMiddleware<SpanMiddleware>()
          .UseMiddleware<DurationHistogramMiddleware>()
          .UseMiddleware<RequestCountMiddleware>()
          |> ignore
        next.Invoke builder
      Action<_>(build next)


module internal Impl =
  let addLogaryServices (logary: LogManager) (s: IServiceCollection) =
    // TODO: extension method IServiceCollection.AddLogary()
    s.AddHttpContextAccessor()
     .AddSingleton(logary)
     .AddSingleton(logary.getLogger(logary.runtimeInfo.resource.service))
     .AddTransient<IStartupFilter, LogaryStartupFilter>()
     .AddSingleton<ILoggerFactory, LogaryLoggerFactory>()
    |> ignore


// TODO: refactor to https://docs.microsoft.com/en-us/aspnet/core/fundamentals/middleware/write?view=aspnetcore-3.1
// TODO: UseXXXMiddleware for every possible middleware that is optional
// TODO: UseInternalLogary()
// TODO: UseLogary(logary) on the right interface
// TODO: UseLogary(configurator) as a configurator

// EITHER use:
[<Extension; AutoOpen>]
module IHostBuilderEx =
  open Impl

  [<Extension; CompiledName "UseLogary">]
  let useLogary (builder: IHostBuilder, logary: LogManager) =
     let cb = Action<_>(addLogaryServices logary)
     builder
       .ConfigureLogging(Action<ILoggingBuilder> (fun logging -> logging.ClearProviders() |> ignore))
       .ConfigureServices(cb)

  type IHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)


// OR, you can use:
[<Extension; AutoOpen>]
module IWebHostBuilderEx =
  open Impl

  [<Extension; CompiledName "UseLogary">]
  let useLogary (builder: IWebHostBuilder, logary: LogManager) =
     let cb = Action<_>(addLogaryServices logary)
     builder
       .SuppressStatusMessages(true)
       .ConfigureLogging(Action<ILoggingBuilder> (fun logging -> logging.ClearProviders() |> ignore))
       .ConfigureServices(cb)

  type IWebHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)
