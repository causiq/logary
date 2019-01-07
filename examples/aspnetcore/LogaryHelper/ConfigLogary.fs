namespace LogaryHelper

open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Targets.Jaeger


module ConfigLogary =

    // extendedTokeniser here is to show all fields, context and gauges as separate lines in the output.
    // you can use default to show message in one line as you want
    let literateConsoleConf = 
      { LiterateConsole.empty with tokenise = LiterateConsole.Tokenisers.extendedTokeniser }

    let create host =
      Config.create "Logary.AspNetCore.Sample" host
      |> Config.middleware Middleware.ambientSpanId // use this if you want support aspnet.logging.scope, scope in aspnet logging is a SpanInfo in logary
      |> Config.target (LiterateConsole.create literateConsoleConf "LiterateConsole") // config targets you want
      |> Config.target (JaegerTarget.create JaegerTarget.empty "JaegerTracing")
      |> Config.loggerMinLevel ".*" Verbose // use verbose here is to use asp.net core appsettings.json to contorll logging rules
      |> Config.buildAndRun
