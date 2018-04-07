namespace Logary.AspNetCore

open Microsoft.Extensions.Logging
open System.Runtime.CompilerServices
open Logary

[<AutoOpen;Extension>]
module ILoggingBuilderExtensions =


    /// dispose: whether or not when this singleton DI compnent dispose
    [<Extension>]
    let AddLogary (x: ILoggingBuilder, m: LogManager, needDispose: bool) : ILoggingBuilder = 
      x.AddProvider(new LogaryLoggerProvider(m, needDispose))
      
[<AutoOpen;Extension>]
module ILoggerFactoryExtensions =

    /// dispose : whether or not when ILoggerFactory dispose
    [<Extension>]
    let AddLogary  (x:ILoggerFactory, m: LogManager, needDispose: bool) : ILoggerFactory =
      x.AddProvider(new LogaryLoggerProvider(m, needDispose))
      x
      