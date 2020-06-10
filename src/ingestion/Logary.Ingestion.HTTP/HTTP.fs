﻿namespace Logary.CORS

open NodaTime

type HTTPOrigin = string

type OriginResult =
  /// Allow all origins
  | Star
  /// Allow the origin passed by the Origin request header
  | Origin of origin:string
  /// Don't return this header
  | Reject

  static member allowAll _ =
    Star

  static member allowRequester origin =
    Origin origin

  member x.asWebPart =
    match x with
    | Star -> setHeader "Access-Control-Allow-Origin" "*"
    | Origin origin -> setHeader "Access-Control-Allow-Origin" origin
    | Reject -> never // bails out of the pipeline

type CORSConfig =
  { /// Toggle CORS-support on/off. This is toggled on if you supply a `accessControlAllowOrigin` callback
    allowCORS: bool
    /// https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS#The_HTTP_response_headers
    accessControlAllowOrigin: HTTPOrigin -> OriginResult
    /// Access-Control-Request-Method -> your allowed methods
    accessControlAllowMethods: HttpMethod -> HttpMethod list
    /// Access-Control-Request-Headers -> your allowed Origin
    accessControlAllowHeaders: string list -> string list
    /// For how long does this policy apply?
    accessControlMaxAge: Duration
  }
  static member create(?accessControlAllowOrigin, ?accessControlAllowHeaders, ?accessControlMaxAge) =
    let allowCORS, acao, acah =
      Option.isSome accessControlAllowOrigin,
      accessControlAllowOrigin |> Option.defaultValue (fun _ -> Star),
      accessControlAllowHeaders |> Option.defaultValue id
    { allowCORS = Option.isSome accessControlAllowOrigin
      accessControlAllowOrigin = acao
      accessControlAllowHeaders = acah
      accessControlAllowMethods = fun _ -> HttpMethod.POST :: []
      accessControlMaxAge = defaultArg accessControlMaxAge (Duration.FromDays 1) }


module API =
  open System
  open Giraffe

  let withOrigin config next =
    warbler (fun ctx ->
      let o = ctx.request.header "origin" |> Choice.orDefault (fun () -> "http://localhost")
      let ao = config.accessControlAllowOrigin o
      next ao)

  let CORS (config: CORSConfig) =
    if config.allowCORS then
      warbler (fun ctx ->
        let h =
          ctx.request.header "access-control-request-headers"
          |> Choice.map (fun h  -> h.Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)
          |> Choice.orDefault (fun () -> "Content-Type" :: [])
        let ah =
          config.accessControlAllowHeaders h
          |> String.concat ", "
        let m =
          ctx.request.header "access-control-request-method"
          |> Choice.map HttpMethod.parse
          |> Choice.orDefault (fun () -> HttpMethod.POST)
        let am =
          config.accessControlAllowMethods m
          |> List.map (fun m -> m.ToString())
          |> String.concat ", "
        let aa = string config.accessControlMaxAge.TotalSeconds

        withOrigin config (fun ao -> ao.asWebPart)
        >=> setHeader "Access-Control-Allow-Methods" am
        >=> setHeader "Access-Control-Allow-Headers" ah
        >=> setHeader "Access-Control-Max-Age" aa)
        >=> setHeader "Content-Type" "text/plain; charset=utf-8"
        >=> OK ""
    else
      never


namespace Logary.Ingestion

open System.Net
open System.Threading
open Hopac
open Giraffe
open Logary
open Logary.Internals
open Logary.Internals.Chiron
open Logary.CORS

type HTTPConfig =
  { ilogger: Logger
    logary: LogManager
    cancelled: Promise<unit>
    rootPath: string
    webConfig: SuaveConfig
    onSuccess: HttpHandler
    onError: string -> WebPart
    corsConfig: CORSConfig
  }
  interface IngestServerConfig with
    member x.cancelled = x.cancelled
    member x.ilogger = x.ilogger

module internal Impl =
  let printHelp (config: HTTPConfig): WebPart =
    let message = sprintf "You can post a JSON Objects to: %s" config.rootPath
    OK message

  let onSuccess: WebPart =
    ACCEPTED "true"

  let onError: string -> WebPart =
    Json.String >> Json.format >> BAD_REQUEST

  let ingestWith (onSuccess, onError) (next: Ingest): WebPart =
    fun ctx ->
      async {
        let input = Ingested.ofBytes ctx.request.rawForm
        let! res = Job.toAsync (next input)
        match res with
        | Ok () ->
          return! onSuccess ctx
        | Result.Error err ->
          return! onError err ctx
      }

  let createAdaptedCTS (config: HTTPConfig) =
    let cts = new CancellationTokenSource()
    start (job {
      do! config.cancelled
      cts.Cancel()
    })
    cts

type HTTPConfig with
  static member create(rootPath, logary, ilogger, ?cancelled: Promise<unit>, ?endpoint, ?onSuccess, ?onError, ?corsConfig) =
    let binding =
      let ep = defaultArg endpoint (IPEndPoint (IPAddress.Loopback, 8888))
      HttpBinding.create HTTP ep.Address (ep.Port |> uint16)

    { rootPath = rootPath
      logary = logary
      ilogger = ilogger
      cancelled = cancelled |> Option.defaultWith (fun () -> upcast IVar ())
      onSuccess = defaultArg onSuccess Impl.onSuccess
      onError = defaultArg onError Impl.onError
      webConfig = { defaultConfig with bindings = binding :: []; hideHeader = true }
      corsConfig = defaultArg corsConfig (CORSConfig.create())
    }

module HTTP =
  open Impl
  open Suave.Filters
  open Suave.Operators
  open Suave.Writers

  let api (config: HTTPConfig) next: WebPart =
    setMimeType "application/json; charset=utf-8" >=> choose [
      GET >=> path config.rootPath >=> printHelp config

      OPTIONS
        >=> API.CORS config.corsConfig

      POST
        >=> path config.rootPath
        >=> API.withOrigin config.corsConfig (fun ao -> ao.asWebPart)
        >=> Impl.ingestWith (config.onSuccess, config.onError) next
    ]

  let recv (started: IVar<unit>, shutdown: IVar<unit>) config next =
    job {
      let logging =
        { Suave.Logging.Global.defaultConfig with
            getLogger = fun name ->
              config.ilogger
              |> Logger.setNameEnding (String.concat "-" name)
              |> LoggerAdapter.createGeneric<Suave.Logging.Logger>
        }
      Suave.Logging.Global.initialiseIfDefault logging
      do config.ilogger.info("Starting HTTP recv-loop at {bindings}. CORS enabled={allowCORS}" >> setField "bindings" config.webConfig.bindings >> setField "allowCORS" config.corsConfig.allowCORS)
      use cts = createAdaptedCTS config
      let suaveConfig = { config.webConfig with cancellationToken = cts.Token }
      let webStarted, webListening = startWebServerAsync suaveConfig (api config next)
      do! Job.start (Job.fromAsync webListening |> Job.bind (IVar.fill shutdown))
      do! Job.start (Job.fromAsync webStarted |> Job.bind (fun _ -> IVar.fill started ()))
      do! config.cancelled
      do config.ilogger.info (Model.EventMessage "Stopping HTTP recv-loop at {bindings}" >> setField "bindings" config.webConfig.bindings)
    }

  let create: ServerFactory<HTTPConfig> =
    IngestServer.create recv
