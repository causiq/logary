#I "bin/Release/net461"
#r "Hopac.Core"
#r "Hopac"
#r "Microsoft.ApplicationInsights"
#r "Microsoft.AI.DependencyCollector"
#r "Logary"
#load "Targets_AppInsights.fs"
open System
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Targets.ApplicationInsights
open Logary.Logger

let logger =
  Config.create "Rutta" "localhost"
  |> Config.targets [
      Logary.Targets.ApplicationInsights.create(
          { InstrumentationKey = ""; // Enter your instrumentation key here. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY,  https://docs.microsoft.com/azure/application-insights/app-insights-create-new-resource
            DeveloperMode = false;
            TrackDependencies = false;
            MappingConfiguration = allToTrace}
            //MappingConfiguration = {GaugeMapping = GaugeToMetrics; DerivedMapping = DerivedToMetrics; EventMapping = EventToEvent; }}
      ) "ApplicationInsights"
      LiterateConsole.create LiterateConsole.empty "console" ]
  |> Config.loggerLevels [ ".*", Verbose ]
  |> Config.processing (Events.events |> Events.sink [ "console" ])
  |> Config.ilogger (ILogger.Console Debug)
  |> Config.build
  |> run

let curLogger = Log.create "testAppInsights"
Message.eventInfo ("Hello World!")
|> Message.setField "user" "Henrik"
|> logSimple curLogger

// (Wait 5 minutes to Azure to display the new data...)
// Login your Azure Portal (https://portal.azure.com/).
// Go to your Application Insights.
// Click Overview -> Search.
// Your events should be visible under TRACE -category.


// Example with GaugeToMetrics mapping, using commented MappingConfiguration:
// let myFunc x = System.Threading.Thread.Sleep 3000; x+1
// let withPerfLog = Logger.time curLogger "MyTest"
// let res, perf = withPerfLog myFunc 3