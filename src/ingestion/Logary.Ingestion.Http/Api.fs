module Logary.Ingestion.Api

open Logary
open Suave
open Suave.Filters
open Suave.Writers
open Suave.RequestErrors
open Suave.Successful
open Suave.Operators

let private bind (f: 'a -> Choice<'b, 'c>) (v: Choice<'a, 'c>) =
  match v with
  | Choice1Of2 v -> f v
  | Choice2Of2 c -> Choice2Of2 c

type ReporterConfig =
  { transform: Message -> Message
    rootPath: string
    logger: Logger }

  static member create transform rootPath logger =
    { transform = transform
      rootPath = rootPath
      logger = logger }

let defaultConfig =
  let logger = Log.create "SuaveReporter"
  ReporterConfig.create id "/i/logary" logger

let private printHelp (config: ReporterConfig): WebPart =
  let message = sprintf "You can post a JSON structure to: %s" config.rootPath
  OK message >=> setMimeType "text/plain; charset=utf-8"

let private logMessage (config: ReporterConfig): WebPart =
  OK "TODO"

let api (config: ReporterConfig): WebPart =
  choose [
    GET >=> path config.rootPath >=> printHelp config
    POST >=>  path config.rootPath >=> logMessage config
  ]