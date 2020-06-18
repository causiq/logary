namespace Logary.Configuration

open Logary
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open System
open System.Runtime.CompilerServices

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
  let addLogaryServices (logary: LogManager) _ (s: IServiceCollection) =
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
     let cb = Action<_,_>(addLogaryServices logary)
     builder.ConfigureServices(cb)

  type IHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)


// OR, you can use:
[<Extension; AutoOpen>]
module IWebHostBuilderEx =
  open Impl

  [<Extension; CompiledName "UseLogary">]
  let useLogary (builder: IWebHostBuilder, logary: LogManager) =
     let cb = Action<_>(addLogaryServices logary ())
     builder.ConfigureServices(cb)

  type IWebHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)
