module Logary.Tests.Program

open System.Globalization
open System.Threading
open Expecto
open Logary
open Logary.Internals
open Logary.Configuration

[<Tests>]
let tests =
  testList "logary" [

    testList "utils" [
      testList "FsCheck" FsCheck.tests
    ]

    testList "internals" [
      testList "HashMap" HashMap.tests
      testList "Global" Global.tests
    ]

    testList "core" [
      testList "LogLevel" LogLevel.tests
      testList "Constants" Constants.tests
      testList "date and time" DateAndTime.tests
      testList "Value" Value.tests
      testList "Units" Units.tests
      testList "Gauge" Gauge.tests
      testList "PointName" PointName.tests
      testList "Message" Message.tests
      testList "NullLogger" NullLogger.tests
      testList "Logger" Logger.tests
      testList "Middleware" Middleware.tests
    ]

    testList "configuration" [
      testList "Config" Config.tests
      testList "Transformers" Transformers.tests
      testList "Registry" Tests.Registry.tests
    ]

    testList "Engine" Engine.tests

    testList "core targets" CoreTargets.tests
    testLabel "literate console" LiterateConsole.tokenisation
    testLabel "literate console" LiterateConsole.parts

    testList "formatting" [
      Formatting.MessageTemplates.tests
      Formatting.MessageWriters.tests
      Formatting.MessageWriters.stacktrace fsc
      Json.tests fsc
    ]

    testList "codecs" Codecs.tests
    testList "metric" Metric.tests
    testList "trace" Trace.tests
  ]

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig args