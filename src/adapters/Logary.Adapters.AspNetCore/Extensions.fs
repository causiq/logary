namespace Logary.Adapters.AspNetCore

open Logary.Configuration
open Microsoft.Extensions.Logging
open System.Runtime.CompilerServices


[<AutoOpen;Extension>]
module ILoggingBuilderExtensions =

    [<Extension>]
    let AddLogary (x: ILoggingBuilder, m: LogManager) : ILoggingBuilder = 
      // provider add from builder doesn't dispose from framework
      // https://github.com/aspnet/Extensions/blob/2.2.0/src/Logging/Logging/src/LoggerFactory.cs#L39-L42
      x.AddProvider(new LogaryLoggerProvider(m, false))
      
[<AutoOpen;Extension>]
module ILoggerFactoryExtensions =

    /// `dispose` : whether or not when ILoggerFactory dispose
    [<Extension>]
    let AddLogary  (x:ILoggerFactory, m: LogManager, needDispose: bool) : ILoggerFactory =
      x.AddProvider(new LogaryLoggerProvider(m, needDispose))
      x
      