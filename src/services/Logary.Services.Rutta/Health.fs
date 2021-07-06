namespace Logary.Services.Rutta

open System.IO
open System.Reflection
open Logary
open Logary.Ingestion
open Logary.Metric.Prometheus
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection

module Health =
  open System.Runtime.InteropServices
  open System
  open fszmq

  let readResource file =
    let assembly = Assembly.GetExecutingAssembly()
    let resourceName = $"Logary.Services.Rutta.%s{file}"
    use stream = assembly.GetManifestResourceStream(resourceName)
    use reader = new StreamReader(stream)
    reader.ReadToEnd()

  let healthMessage () =
    let time = DateTime.UtcNow.ToString("o")
    let gitCommit = readResource "git-commit.txt"
    $"Rutta %s{AssemblyVersionInformation.AssemblyFileVersion} @ %s{gitCommit.Trim('\n')} running on '%s{RuntimeInformation.OSDescription}'. ZMQ v{ZMQ.version}. %s{time}"

  open Giraffe
  open Successful
  open Hopac

  module App =
    let api = GET >=> route "/health" >=> warbler (fun _ -> OK (healthMessage ()))

  let startServerInner (internalLogary: LogManager) (binding: Binding) =
    let useGiraffe (x: IApplicationBuilder) = x.UseGiraffe App.api
    let addGiraffe (x: IServiceCollection) = x.AddGiraffe() |> ignore
    let conf = ExporterConf.create(internalLogary, binding=binding, configureApp=useGiraffe, configureServices=addGiraffe)
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