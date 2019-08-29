namespace Microsoft.Extensions.Logging

open Logary
open Logary.Adapters.AspNetCore
open System
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module IServiceCollectionEx =
  [<Extension; CompiledName "AddLogary">]
  let addLogary (services: IServiceCollection, logary: LogManager) =
    services.AddSingleton<LogManager>(logary)

[<AutoOpen; Extension>]
module ILoggingBuilderEx =
  [<Extension; CompiledName "AddLogary">]
  let addLogary (x: ILoggingBuilder, logary: LogManager): ILoggingBuilder =
    // provider add from builder doesn't dispose from framework
    // https://github.com/aspnet/Extensions/blob/2.2.0/src/Logging/Logging/src/LoggerFactory.cs#L39-L42
    x.AddProvider(new LogaryLoggerProvider(logary, false))

[<AutoOpen; Extension>]
module ILoggerFactoryEx =
  /// `needDispose` : whether or not when ILoggerFactory dispose
  [<Extension; CompiledName "AddLogary">]
  let addLogary (x: ILoggerFactory, logary: LogManager, needDispose: bool): ILoggerFactory =
    x.AddProvider(new LogaryLoggerProvider(logary, needDispose))
    x

[<AutoOpen; Extension>]
module IWebHostBuilderEx =
  [<Extension; CompiledName "AddLogary">]
  let addLogary (builder: IWebHostBuilder, logary: LogManager) =
     builder
       .ConfigureServices(Action<_>(fun services ->
         IServiceCollectionEx.addLogary(services, logary) |> ignore
       ))
