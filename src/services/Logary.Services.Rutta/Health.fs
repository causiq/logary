namespace Logary.Services.Rutta

open Logary.Ingestion

module Health =
  open System.Runtime.InteropServices
  open System
  open System.Threading
  open fszmq

  let healthMessage osDesc =
    let time = DateTime.UtcNow.ToString("o")
    sprintf "Rutta %s running on '%s'. ZMQ v%O. %s"
      AssemblyVersionInformation.AssemblyFileVersion
      RuntimeInformation.OSDescription
      ZMQ.version
      time

  module App =
    let app =
      GET >=> path "/health" >=> warbler (fun _ -> OK (healthMessage ()))
      // TODO: /metrics
      // TODO: port to Giraffe

  let startServerInner (Binding (Scheme _, nic, Port port)) =
    let cts = new CancellationTokenSource()

    let config =
      { defaultConfig with
          bindings = [ HttpBinding.create HTTP nic.asIPAddress port ]
          cancellationToken = cts.Token }

    let ready, handle = startWebServerAsync config App.app
    Async.Start handle
    Async.RunSynchronously ready |> ignore

    { new IDisposable with member x.Dispose() = cts.Cancel () }

  /// Start a web server on the passed ip and port and return the IDisposable
  /// token to use to shut the server down.
  let startServer (binding: Binding option) =
    match binding with
    | None ->
      { new IDisposable with member x.Dispose() = () }
    | Some binding ->
      startServerInner binding