module Program

open Suave
open Suave.Filters
open Suave.RequestErrors
open Suave.Successful
open Suave.Operators
open Hopac
open Logary
open Logary.Message
open Logary.Targets
open Logary.Adapters.Facade
open Logary.Configuration
open Logary.Prometheus.Exporter

[<EntryPoint>]
let main _ =
  let logary =
    Config.create "Logary.Examples.Suave" "localhost"
    |> Config.ilogger (ILogger.LiterateConsole Verbose)
    |> Config.targets [
      LiterateConsole.create LiterateConsole.empty "console"
      Jaeger.create { Jaeger.empty with jaegerPort = 30831us } "jaeger"
    ]
    |> Config.build
    |> run

  LogaryFacadeAdapter.initialise<Suave.Logging.Logger> logary

  let logger = Logary.Log.create "Logary.Examples.Suave"

  let app: WebPart =
    choose [
      GET >=> path "/hello" >=> spanLogger (fun logger ->
        logger.info (eventX "Returning from the first route: 'GET /'")
        OK (sprintf "Hello World! My route has been going for %O so far..." logger.elapsed))

      POST >=> path "/" >=> OK "{\"hello\": \"world\"}"

      request (fun req -> NOT_FOUND (sprintf "What are you trying to do? %s wasn't found, anyway! :-s" req.path))
    ]

  use metrics = Exporter.run (ExporterConf.create(logary.metricRegistry))
  startWebServer defaultConfig (withTracing logger app)

  0