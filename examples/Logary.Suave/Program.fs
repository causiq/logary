module Program

open System
open Suave
open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Logging
open Logary
open Logary.Adapters.Facade
open Logary.Configuration
open Logary.EventsProcessing
open Logary.Targets

[<EntryPoint>]
let main argv =
  let processing = 
    Events.stream 
    |> Events.subscribers [
         Events.events |> Events.sink ["console"]
       ]
    |> Events.toProcessing

  let logary = 
    Config.create "suave.example" "localhost"
    |> Config.ilogger (ILogger.LiterateConsole Verbose)
    |> Config.target (LiterateConsole.create LiterateConsole.empty "console")
    |> Config.processing processing
    |> Config.build
    |> Hopac.Hopac.run
    |> Registry.toLogManager

  
  LogaryFacadeAdapter.initialise<Suave.Logging.Logger> logary

  let logger = Suave.Logging.Log.create "logger.from.suave"
  let myapp : WebPart =
      choose [
          path "/" >=> GET >=> Successful.OK "Hello World!"
          path "/" >=> POST >=> Successful.OK "{\"hello\": \"world\"}"
          NOT_FOUND "What are you trying to do?"
      ]
      >=> logWithLevelStructured Logging.Info logger (fun context ->
          let fields : (string * obj) list = [
              "clientIP", box context.clientIpTrustProxy
              "requestMethod", box context.request.method
              "requestPathAndQuery", box context.request.url.PathAndQuery
              "requestId", box context.request.trace.traceId
              "httpStatusReason", box context.response.status.reason
              "httpStatusCode", box context.response.status.code
              "requestForm", box context.request.form ]
          "Client {@clientIP} HTTP {requestMethod} at \"{requestPathAndQuery}\" responded {httpStatusReason} ({httpStatusCode})",
          fields |> Map.ofList
      )

  let config = { defaultConfig with logger = logger }
  startWebServer config myapp

  0