namespace Logary.Metric.Prometheus

open System
open System.IO
open System.Security.Cryptography.X509Certificates
open System.Text
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.Builders
open Hopac
open Logary
open Logary.Configuration
open Logary.Ingestion
open Logary.Metric
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection

type ExporterConf =
  { nameMetric: string -> string
    logary: LogManager
    metricRegistry: MetricRegistry
    configureApp: IApplicationBuilder -> unit
    configureServices: IServiceCollection -> unit
    binding: Binding
    certificate: X509Certificate2 option
    urlPath: string
  }

  static member create(logary: LogManager, ?registry, ?urlPath, ?binding, ?certificate, ?configureApp, ?configureServices) =
    let (Binding (_, nic, port)) = binding |> Option.defaultValue (Binding (Scheme "http", NIC "+", Port 9121us))
    let scheme = if certificate.IsSome then Scheme "https" else Scheme "http"
    let binding = Binding (scheme, nic, port)
    { nameMetric = defaultMetricNamer
      logary = logary
      metricRegistry = defaultArg registry logary.metrics
      configureApp = defaultArg configureApp ignore
      configureServices = defaultArg configureServices ignore
      binding = binding
      certificate = certificate
      urlPath = defaultArg urlPath "/metrics"
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExporterConf =
  [<Literal>]
  let DefaultContentType = "text/plain; version=0.0.4; charset=utf-8"

  let withConfigure configure conf =
    { conf with configureApp = configure }

module Exporter =
  let writePrometheusPage (writer: TextWriter) (conf: ExporterConf): Task =
    task {
      let metrics = conf.metricRegistry.exportAll()
      for infoAndMetrics in metrics do
        do! writeMetric (writer, conf.nameMetric) infoAndMetrics
    } :> _

  let formatPrometheusPage (conf: ExporterConf): Task =
    task {
      let sb = StringBuilder ()
      use sw = new StringWriter(sb)
      do! writePrometheusPage sw conf
      return sb.ToString()
    } :> _

  let internal tryCancel (cts: CancellationTokenSource) =
    try cts.Cancel()
    with :? OperationCanceledException -> ()

type Exporter private (cts: CancellationTokenSource) =
  let mutable disposed = false
  let shutdownIV = IVar ()

  member __.stop (): Promise<unit> =
    if disposed then shutdownIV :> _ else
    try
      try
        Exporter.tryCancel cts
        shutdownIV :> _
      with e ->
        start (IVar.fillFailure shutdownIV e)
        shutdownIV :> _
    finally
      disposed <- true
      cts.Dispose()

  member val shutdown = shutdownIV :> Promise<unit> with get

  interface IDisposable with
    member x.Dispose() =
      x.stop() |> ignore

  /// Starts a web server with a CancellationToken.
  [<CompiledName "Start">]
  static member start(conf: ExporterConf, token) =
    let whenGettingRoot (context: HttpContext) =
      context.Request.Path.Value.Trim('/') = ""

    let redirectToMetrics (x: HttpContext) _ =
      x.Response.Redirect(conf.urlPath)
      Task.CompletedTask

    let respondWithMetrics (context: HttpContext) _: Task =
      task {
        context.Response.ContentType <- ExporterConf.DefaultContentType
        context.Response.StatusCode <- 200
        let sw = new StreamWriter(context.Response.Body, Encoding.UTF8, leaveOpen=true)
        do! Exporter.writePrometheusPage sw conf
        do! sw.DisposeAsync()
      } :> _

    let configureRoutes (_: WebHostBuilderContext) (app: IApplicationBuilder) =
      app.Map(PathString.op_Implicit conf.urlPath, fun app -> app.Use(Func<_,_,_> respondWithMetrics) |> ignore)
        |> ignore

      app.MapWhen(Func<_,_> whenGettingRoot, fun app -> app.Use(Func<_,_,_> redirectToMetrics) |> ignore)
        |> ignore

      app.UseRouting()
        |> ignore

      conf.configureApp app

    let configureKestrel (options: KestrelServerOptions) =
      match conf.certificate with
      | Some cert ->
        options.Listen(conf.binding.asEndpoint, fun options -> options.UseHttps(cert) |> ignore)
      | None ->
        ()

    let configureServices (_: WebHostBuilderContext) (services: IServiceCollection) =
      services.AddRouting()
        |> ignore

      services.Configure<KestrelServerOptions>(Action<_> configureKestrel)
        |> ignore

      conf.configureServices services

    let builder =
      WebHostBuilder()
        .UseKestrel(fun o -> o.AllowSynchronousIO <- false)
        .UseLogary(conf.logary)
        .ConfigureServices(Action<_, _> configureServices)
        .Configure(Action<_,_> configureRoutes)

    let builder =
      if conf.certificate.IsNone then builder.UseUrls(conf.binding.ToString())
      else builder

    let logger = conf.logary.getLogger "Logary.Metric.Prometheus"
    logger.info("Starting health web server at {binding}", fun m -> m.setField("binding", conf.binding))

    builder
      .Build()
      .StartAsync(token)

  /// Starts an Exporter with a Hopac Promise<unit> to signal the shutdown.
  [<CompiledName "StartServer">]
  static member startServer(conf, cancelled: Promise<unit>): Job<Exporter> =
    let cts = new CancellationTokenSource()
    start (cancelled |> Alt.afterFun (fun () -> Exporter.tryCancel cts))
    Job.fromUnitTask (fun () -> Exporter.start(conf, cts.Token))
      |> Job.map (fun () -> new Exporter(cts))
