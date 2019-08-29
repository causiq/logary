namespace Logary.Adapters.AspNetCore

open Logary
open Microsoft.Extensions.Logging
open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module ILoggingBuilderExtensions =
  [<Extension; CompiledName "AddLogary">]
  let addLogary (x: ILoggingBuilder, m: LogManager): ILoggingBuilder =
    // provider add from builder doesn't dispose from framework
    // https://github.com/aspnet/Extensions/blob/2.2.0/src/Logging/Logging/src/LoggerFactory.cs#L39-L42
    x.AddProvider(new LogaryLoggerProvider(m, false))

[<AutoOpen; Extension>]
module ILoggerFactoryExtensions =
  /// `needDispose` : whether or not when ILoggerFactory dispose
  [<Extension; CompiledName "AddLogary">]
  let addLogary (x: ILoggerFactory, m: LogManager, needDispose: bool): ILoggerFactory =
    x.AddProvider(new LogaryLoggerProvider(m, needDispose))
    x
