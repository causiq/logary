namespace Logary.AspNetCore

open Microsoft.Extensions.Logging
open Logary


module LoggingBuilderExtensions =

  type ILoggingBuilder with
    /// dispose: whether or not when this singleton DI compnent dispose
    member x.AddLogary  (m: LogManager, needDispose: bool) : ILoggingBuilder = 
      x.AddProvider(new LogaryLoggerProvider(m, needDispose))
      

  type ILoggerFactory with
    /// dispose : whether or not when ILoggerFactory dispose
    member x.AddLogary  (m: LogManager, needDispose: bool) : ILoggerFactory =
      x.AddProvider(new LogaryLoggerProvider(m, needDispose))
      x
      