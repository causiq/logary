module Logary.Ingestion.Http

open System.Text
open Hopac
open Logary
open Logary.Internals.Chiron
open Suave
open Suave.Filters
open Suave.Writers
open Suave.RequestErrors
open Suave.Successful
open Suave.Operators

type ReporterConfig =
  { rootPath: string
    next: Json -> Job<unit> }
  static member create rootPath next =
    { rootPath = rootPath; next = next }

let private printHelp (config: ReporterConfig): WebPart =
  let message = sprintf "You can post a JSON Objects to: %s" config.rootPath
  OK message >=> setMimeType "text/plain; charset=utf-8"

let private thx = ACCEPTED "true"
let private fail = Json.encode >> BAD_REQUEST

let private logMessage (config: ReporterConfig): WebPart =
  fun ctx ->
    async {
      match Json.parse (Encoding.UTF8.GetString ctx.request.rawForm) with
      | JsonResult.JPass json ->
        do! Job.toAsync (config.next json)
        return! thx ctx
      | JsonResult.JFail err ->
        return! fail err ctx
    }

let defaultConfig =
  let logger = Log.create "SuaveReporter"
  ReporterConfig.create "/i/logary" logger

let api (config: ReporterConfig): WebPart =
  choose [
    GET >=> path config.rootPath >=> printHelp config
    POST >=>  path config.rootPath >=> logMessage config >=> setMimeType "application/json; charset=utf-8"
  ]

let create (config: ReporterConfig) =
  startWebServer Suave.Web.defaultConfig (api config)