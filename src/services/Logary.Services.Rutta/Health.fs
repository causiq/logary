namespace Logary.Services.Rutta

module Health =
  open System.Runtime.InteropServices
  open System
  open System.Net
  open System.Threading
  open Suave
  open Suave.Operators
  open Suave.Filters
  open Suave.Successful
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
//      GET >=> path "/metrics" >=> ...

  let startServerInner (binding: IPEndPoint) =
    let cts = new CancellationTokenSource()

    let config =
      { defaultConfig with
          bindings = [ HttpBinding.create HTTP binding.Address (uint16 binding.Port) ]
          cancellationToken = cts.Token }

    let ready, handle = startWebServerAsync config App.app
    Async.Start handle

    { new IDisposable with member x.Dispose() = cts.Cancel () }

  /// Start a Suave web server on the passed ip and port and return the IDisposable
  /// token to use to shut the server down.
  let startServer (binding: IPEndPoint option) =
    match binding with
    | None ->
      { new IDisposable with member x.Dispose() = () }
    | Some binding ->
      startServerInner binding