////////////// SAMPLE LOGARY CONFIGURATION //////////
#I "bin/Debug"
#r "NodaTime.dll"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "FParsec.dll"
#r "Logary.dll"
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets


let logary =
  withLogaryManager "Logary.ConsoleApp" (
    withTargets [
      LiterateConsole.create LiterateConsole.empty "literate"

      // This target prints more info to the console than the literate one
      // Console.create (Console.empty) "console"
    ] >>
    withRules [
      Rule.createForTarget "literate"
      //Rule.createForTarget "console"
  ])
  |> Hopac.run

////////////// SAMPLE NLOG CONFIGURATION //////////

#r "NLog.dll"
#load "NLog.Targets.Logary.fs"
open NLog
open NLog.Targets
open NLog.Config
open NLog.Common
let config = LoggingConfiguration()
InternalLogger.LogToConsole <- true
InternalLogger.IncludeTimestamp <- true
let logaryT = new LogaryTarget(logary)
//logaryT.Logary <- logary
config.AddTarget("logary", logaryT)
let rule = LoggingRule("*", NLog.LogLevel.Debug, logaryT)
config.LoggingRules.Add rule
LogManager.Configuration <- config





// You'll get a logger in your app
let logger = LogManager.GetLogger("NLog.Example")

//////////// SAMPLE USAGE: ///////////////
logger.Info("Hello world")

// NLog's targets doesn't like this, but you can do it with the Logary target. Note that
// Logary doesn't evaluate any NLog layouts, but has its own template format.
logger.Info("Hello {user}! This is {0}.", "haf", "Mr M")

// you can also log data
let exceptiony() =
  let inner1() =
    failwith "Uh"
  let inner2() =
    inner1()
  let inner3() =
    inner2()
  try inner3() with e -> e

let evt =
  LogEventInfo.Create(LogLevel.Info, "NLog.Example.Custom", System.Globalization.CultureInfo.InvariantCulture,
                      "Hello world!")

evt.Properties.Add("user", "haf")
evt.Properties.Add("service", "web-alpha")
evt.Exception <- exceptiony()

logger.Log evt

LogManager.Shutdown()
logary.DisposeAsync() |> run