namespace Logary.Ingestion.HTTP

open System.Text
open System.Threading
open Hopac
open Giraffe
open Logary
open Logary.Configuration
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Ingestion
open Logary.Ingestion.HTTP.CORS
open FSharp.Control.Tasks.V2
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.IO

type HTTPConfig =
  { ilogger: Logger
    logary: LogManager
    cancelled: Promise<unit>
    rootPath: string
    bindings: BindingList
    onSuccess: HttpHandler
    onError: string -> HttpHandler
    corsConfig: CORSConfig
  }
  member x.bindingsAsURLs() =
    x.bindings |> Seq.map string |> String.concat ","

  interface IngestServerConfig with
    member x.cancelled = x.cancelled
    member x.ilogger = x.ilogger
    member x.bindings = x.bindings

module internal Impl =
  open RequestErrors

  let printHelp (config: HTTPConfig): HttpHandler =
    let message = sprintf "You can post a JSON Objects to: %s" config.rootPath
    text message

  let onSuccess: HttpHandler =
    setStatusCode 201 >=> json true

  let onError: string -> HttpHandler =
    Json.String >> Json.format >> BAD_REQUEST

  /// https://github.com/Microsoft/Microsoft.IO.RecyclableMemoryStream
  let private manager = RecyclableMemoryStreamManager(ThrowExceptionOnToArray = true)

  let ingestWith (onSuccess, onError) (ingest: Ingest): HttpHandler =
    let getResult (ctx: HttpContext) =
      task {
        // https://github.com/Microsoft/Microsoft.IO.RecyclableMemoryStream#usage
        use ms = manager.GetStream "Logary.Ingestion.HTTP.ingestWith"
        try
          do! ctx.Request.Body.CopyToAsync(ms)
          let input = Ingested.ofBytes (ms.GetBuffer())
          return! Job.ToTask (ingest input)
        finally
          ms.Dispose()
      }

    fun next ctx ->
      task {
        match! getResult ctx with
        | Ok () ->
          return! onSuccess next ctx
        | Result.Error err ->
          return! onError err next ctx
      }

  let createAdaptedCTS (config: HTTPConfig) =
    let cts = new CancellationTokenSource()
    start (config.cancelled |> Alt.afterFun cts.Cancel)
    cts

type HTTPConfig with
  static member create(rootPath, logary, ilogger, ?cancelled: Promise<unit>, ?bindings, ?onSuccess, ?onError, ?corsConfig) =
    { rootPath = rootPath
      logary = logary
      ilogger = ilogger
      bindings =
        bindings
          |> Option.defaultWith (fun () ->
            [ Binding.create("http", "0.0.0.0", 8080us)
              Binding.create("https", "0.0.0.0", 8443us) ] |> BindingList.create)
      cancelled = cancelled |> Option.defaultWith (fun () -> upcast IVar ())
      onSuccess = defaultArg onSuccess Impl.onSuccess
      onError = defaultArg onError Impl.onError
      corsConfig = defaultArg corsConfig (CORSConfig.create())
    }

module HTTP =
  open Impl
  open System

  let api (config: HTTPConfig) ingest: HttpHandler =
    setHttpHeader "content-type" "application/json; charset=utf-8" >=> choose [
      GET
        >=> route config.rootPath
        >=> printHelp config

      OPTIONS
        >=> API.CORS config.corsConfig

      POST
        >=> route config.rootPath
        >=> API.withOrigin config.corsConfig (fun ao -> ao.asHttpHandler)
        >=> Impl.ingestWith (config.onSuccess, config.onError) ingest
    ]

  let internal errorHandler (logger: Logger) (ex: Exception) _ =
    logger.error("An unhandled exception has occurred while executing the request.", ex)
    clearResponse >=> setStatusCode 500 >=> text ex.Message

  let internal configureApp (config: HTTPConfig, ingest: Ingest) (app: IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    let tmp = if env.EnvironmentName = "Development" then app.UseDeveloperExceptionPage()
              else app.UseGiraffeErrorHandler(errorHandler config.ilogger)
    tmp.UseGiraffe(api config ingest)

  let internal configureServices (services: IServiceCollection) =
    services.AddGiraffe()
      |> ignore

  let recv (started: IVar<unit>, shutdown: IVar<unit>) (config: HTTPConfig) ingest =
    config.ilogger.info("Starting HTTP recv-loop at {bindings}. CORS enabled={allowCORS}", fun m ->
      m.setField("bindings", config.bindings)
      m.setField("allowCORS", config.corsConfig.allowCORS))

    let host =
      WebHostBuilder()
        .UseKestrel(fun o -> o.AllowSynchronousIO <- false)
        .UseLogary(config.logary)
        .ConfigureServices(configureServices)
        .Configure(Action<_> (configureApp (config, ingest)))
        .UseUrls(config.bindingsAsURLs())
        .Build()

    let cts = new CancellationTokenSource()
    start (shutdown |> Alt.afterFun cts.Dispose)

    job {
      do! IVar.fill started ()
      do! Job.fromUnitTask (fun () -> host.RunAsync cts.Token)
    }

  let create: ServerFactory<HTTPConfig> =
    IngestServer.create recv
