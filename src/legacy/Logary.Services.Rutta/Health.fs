namespace Logary.Services.Rutta

module Health =
  open Logary
  open System
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

  /// Start a Suave web server on the passed ip and port and return a disposable
  /// token to use to shut the server down.
  let startServer ip port =
    let cts = new CancellationTokenSource()

    let config =
      { defaultConfig with
          bindings = [ HttpBinding.createSimple HTTP ip port ]
          cancellationToken = cts.Token }

    let ready, handle = startWebServerAsync config App.app
    Async.Start handle

    { new IDisposable with member x.Dispose() = cts.Cancel () }
