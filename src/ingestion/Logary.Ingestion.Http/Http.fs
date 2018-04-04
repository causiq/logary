module Logary.Ingestion.HTTP

open System.Text
open System.Threading
open Hopac
open Logary
open Logary.Message
open Logary.Formatting
open Logary.Internals
open Logary.Internals.Chiron
open Suave
open Suave.Filters
open Suave.Writers
open Suave.RequestErrors
open Suave.Successful
open Suave.Operators

type ReporterConfig =
  { rootPath: string
    cancelled: Promise<unit>
    suaveConfig: SuaveConfig
  }
  static member create (rootPath, ?cancelled, ?suaveConfig) =
    { rootPath = rootPath
      cancelled = defaultArg cancelled (IVar ())
      suaveConfig = defaultArg suaveConfig Suave.Web.defaultConfig
    }

let defaultConfig = ReporterConfig.create "/i/logary"

let private printHelp (config: ReporterConfig): WebPart =
  let message = sprintf "You can post a JSON Objects to: %s" config.rootPath
  OK message >=> setMimeType "text/plain; charset=utf-8"

let private ilogger = Log.create "Logary.Ingestion.HTTP"
let private thx: WebPart =
  ACCEPTED "true"
let private fail: string -> WebPart =
  Json.encode >> Json.format >> BAD_REQUEST

let private logMessage (next: Ingest): WebPart =
  fun ctx ->
    async {
      let input = Ingested.ofBytes ctx.request.rawForm
      let! res = Job.toAsync (next input)
      match res with
      | Ok () ->
        return! thx ctx
      | Result.Error err ->
        return! fail err ctx
    }

let api (config: ReporterConfig) (next: Ingest): WebPart =
  choose [
    GET >=> path config.rootPath >=> printHelp config
    POST >=>  path config.rootPath >=> logMessage next >=> setMimeType "application/json; charset=utf-8"
  ]

let private createAdaptedCTS (config: ReporterConfig) =
  let cts = new CancellationTokenSource()
  start (job {
    do! config.cancelled
    cts.Cancel()
  })
  cts

let create (config: ReporterConfig) (next: Ingest) =
  job {
    do ilogger.info (eventX "Starting HTTP log listener at {bindings}" >> setField "bindings" config.suaveConfig.bindings)
    use cts = createAdaptedCTS config
    let suaveConfig = { config.suaveConfig with cancellationToken = cts.Token }
    let started, listening = startWebServerAsync suaveConfig (api config next)
    do! Job.start (Job.fromAsync listening)
    do! config.cancelled
    do ilogger.info (eventX "Stopping HTTP log listener at {bindings}" >> setField "bindings" config.suaveConfig.bindings)
  }