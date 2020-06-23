namespace Logary.Services.Rutta

open Logary.Ingestion
open Logary.Metric
open Logary.Metric.Prometheus
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection

module Health =
  open System.Runtime.InteropServices
  open System
  open fszmq

  let healthMessage osDesc =
    let time = DateTime.UtcNow.ToString("o")
    sprintf "Rutta %s running on '%s'. ZMQ v%O. %s"
      AssemblyVersionInformation.AssemblyFileVersion
      RuntimeInformation.OSDescription
      ZMQ.version
      time

  open Giraffe
  open Successful
  open Hopac

  module App =
    let api = GET >=> route "/health" >=> warbler (fun _ -> OK (healthMessage ()))

  let startServerInner (metrics: MetricRegistry) (binding: Binding) =
    let useGiraffe (x: IApplicationBuilder) = x.UseGiraffe App.api
    let addGiraffe (x: IServiceCollection) = x.AddGiraffe() |> ignore
    let conf = ExporterConf.create(metrics, binding=binding, configureApp=useGiraffe, configureServices=addGiraffe)
    Exporter.startServer(conf, IVar ())
      |> run
      :> IDisposable

  /// Start a web server on the passed ip and port and return the IDisposable
  /// token to use to shut the server down.
  let startServer metrics (binding: Binding option) =
    match binding with
    | None ->
      { new IDisposable with member x.Dispose() = () }
    | Some binding ->
      startServerInner metrics binding