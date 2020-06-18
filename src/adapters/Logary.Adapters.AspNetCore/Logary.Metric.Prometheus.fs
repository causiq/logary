namespace Logary.Metric.Prometheus

open System
open System.IO
open System.Security.Cryptography.X509Certificates
open System.Text
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.Builders
open Hopac
open Logary.Ingestion
open Logary.Metric
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection

type ExporterConf =
  { nameMetric: string -> string
    metricRegistry: MetricRegistry
    configure: IApplicationBuilder -> unit
    binding: Binding
    certificate: X509Certificate2 option
    urlPath: string
  }

  static member create(registry, ?urlPath, ?binding, ?certificate, ?configure) =
    let (Binding (_, nic, port)) = binding |> Option.defaultValue (Binding (Scheme "http", NIC "+", Port 9121us))
    let scheme = if certificate.IsSome then Scheme "https" else Scheme "http"
    let binding = Binding (scheme, nic, port)
    { nameMetric = defaultMetricNamer
      metricRegistry = registry
      configure = defaultArg configure ignore
      binding = binding
      certificate = certificate
      urlPath = defaultArg urlPath "/metrics"
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExporterConf =
  [<Literal>]
  let DefaultContentType = "text/plain; version=0.0.4; charset=utf-8"

  let withConfigure configure conf =
    { conf with configure = configure }

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

  /// Runs a server with a CancellationToken.
  [<CompiledName "Run">]
  static member run(conf: ExporterConf, token) =
    let whenGettingRoot (context: HttpContext) =
      context.Request.Path.Value.Trim('/') = ""

    let redirectToMetrics (x: HttpContext) _ =
      x.Response.Redirect(conf.urlPath)
      Task.CompletedTask

    let respondWithMetrics (context: HttpContext) _: Task =
      task {
        context.Response.ContentType <- ExporterConf.DefaultContentType
        context.Response.StatusCode <- 200
        use sw = new StreamWriter(context.Response.Body, Encoding.UTF8, leaveOpen=true)
        do! Exporter.writePrometheusPage sw conf
      } :> _

    let configureRoutes (_: WebHostBuilderContext) (app: IApplicationBuilder) =
      app.Map(PathString.op_Implicit conf.urlPath, fun app -> app.Use(Func<_,_,_> respondWithMetrics) |> ignore)
        |> ignore

      app.MapWhen(Func<_,_> whenGettingRoot, fun app -> app.Use(Func<_,_,_> redirectToMetrics) |> ignore)
        |> ignore

      conf.configure app

    let configureKestrel (options: KestrelServerOptions) =
      match conf.certificate with
      | Some cert ->
        options.Listen(conf.binding.asEndpoint, fun options -> options.UseHttps(cert) |> ignore)
      | None ->
        ()

    let configureServices (_: WebHostBuilderContext) (services: IServiceCollection) =
      services.Configure<KestrelServerOptions>(Action<_> configureKestrel)
        |> ignore

    let builder =
      WebHostBuilder()
        .UseKestrel()
        .Configure(Action<_,_> configureRoutes)
        .ConfigureServices(Action<_, _> configureServices)

    let builder =
      if conf.certificate.IsNone then builder.UseUrls(conf.binding.ToString())
      else builder

    builder
      .Build()
      .RunAsync(token)

  /// Starts an Exporter with a Hopac Promise<unit> to signal the shutdown.
  [<CompiledName "StartServer">]
  static member startServer(conf, cancelled: Promise<unit>): Job<Exporter> =
    let cts = new CancellationTokenSource()
    start (cancelled |> Alt.afterFun (fun () -> Exporter.tryCancel cts))
    Job.fromUnitTask (fun () -> Exporter.run(conf, cts.Token))
      |> Job.map (fun () -> new Exporter(cts))
