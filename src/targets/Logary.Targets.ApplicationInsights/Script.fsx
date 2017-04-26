#I "./../../../packages"

#r "Hopac/lib/net45/Hopac.Core.dll"
#r "Hopac/lib/net45/Hopac.dll"
#r "Microsoft.ApplicationInsights/lib/net45/Microsoft.ApplicationInsights.dll"
#r "Microsoft.ApplicationInsights.DependencyCollector/lib/net45/Microsoft.AI.DependencyCollector.dll"

#r "./../../../src/Logary/bin/Debug/Logary.dll"

#load "Targets_AppInsights.fs"
open System
open Hopac
open Logary
open Logary.Targets.ApplicationInsights
open Logary.Configuration
open Logary.Logger

let logger = 
    withLogaryManager "testAppInsights" (
        withTargets [
            Logary.Targets.ApplicationInsights.create(
                { InstrumentationKey = ""; // Enter your instrumentation key here. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY,  https://docs.microsoft.com/azure/application-insights/app-insights-create-new-resource
                  DeveloperMode = false; 
                  TrackDependencies = false;
                  MappingConfiguration = allToTrace}
                  //MappingConfiguration = {GaugeMapping = GaugeToMetrics; DerivedMapping = DerivedToMetrics; EventMapping = EventToEvent; }}
            ) "ApplicationInsights"
        ] >> withRules [
            Rule.createForTarget "ApplicationInsights" |> Rule.setLevel LogLevel.Verbose
        ]
    ) |> run

let curLogger = Logary.Logging.getLoggerByName("testAppInsights")
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