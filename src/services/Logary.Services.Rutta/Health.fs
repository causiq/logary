namespace Logary.Services.Rutta

module Health =
  open Logary
  open System
  open System.Net
  open System.Threading
  open Suave
  open Suave.Operators
  open Suave.Filters
  open Suave.Successful

  module App =

    open System.IO
    open System.Reflection

    let app =
      GET >=> path "/health" >=> OK (sprintf "Logary Rutta %s" AssemblyVersionInformation.AssemblyVersion)

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