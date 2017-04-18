#I "./../../../src/targets/Logary.Targets.AppInsights/bin/Debug"
#I "./../../../packages/"
#I "./../../../src/Logary/bin/Debug/"

#r "Hopac/lib/net45/Hopac.Core.dll"
#r "Hopac/lib/net45/Hopac.dll"
#r "Microsoft.ApplicationInsights/lib/net45/Microsoft.ApplicationInsights.dll"
#r "Microsoft.ApplicationInsights.DependencyCollector/lib/net45/Microsoft.AI.DependencyCollector.dll"
#r "Microsoft.ApplicationInsights.PerfCounterCollector/lib/net45/Microsoft.AI.PerfCounterCollector.dll"
#r "Microsoft.ApplicationInsights.WindowsServer/lib/net45/Microsoft.AI.WindowsServer.dll"
#r "Microsoft.ApplicationInsights.WindowsServer.TelemetryChannel/lib/net45/Microsoft.AI.ServerTelemetryChannel.dll"
#r "Logary.dll"

#load "Targets_AppInsights.fs"
open System
open Hopac
open Logary
open Logary.Targets.AppInsights
open Logary.Configuration
open Logary.Logger

let logger = 
    withLogaryManager "testAppInsights" (
        withTargets [
            Logary.Targets.AppInsights.create(
                { AppInsightsKey = ""; // Enter your instrumentation key here. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY
                    DeveloperMode = false; 
                    TrackDependencies = false }
            ) "ApplicationInsights"
        ] >> withRules [
            Rule.createForTarget "ApplicationInsights" |> Rule.setLevel LogLevel.Verbose
        ]
    ) |> run

Message.eventInfo ("Hello World!")
|> Message.setField "user" "Henrik"
|> logSimple (Logary.Logging.getLoggerByName("testAppInsights"))

// (Wait 5 minutes to Azure to display the new data...)
// Login your Azure Portal (https://portal.azure.com/). 
// Go to your Application Insights.
// Click Overview -> Search.
// Your events should be visible under TRACE -category.