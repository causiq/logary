namespace Logary.Ingestion

open System.Net
open System.Threading
open Hopac
open Logary
open Logary.Configuration
open Logary.Message
open Logary.Formatting
open Logary.Internals
open Logary.Internals.Chiron
open Suave
open Suave.Filters
open Suave.Writers
open Suave.RequestErrors
open Suave.Successful
open Suave.Operators

type HTTPConfig =
  { ilogger: Logger
    logary: LogManager
    cancelled: Promise<unit>
    rootPath: string
    webConfig: SuaveConfig
    onSuccess: WebPart
    onError: string -> WebPart
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
    Json.encode >> Json.format >> BAD_REQUEST
  
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
  static member create (rootPath, logary, ilogger, ?cancelled: Promise<unit>, ?endpoint: IPEndPoint, ?onSuccess, ?onError) =
    let binding =
      let ep = defaultArg endpoint (IPEndPoint (IPAddress.Loopback, 8888))
      HttpBinding.create HTTP ep.Address (ep.Port |> uint16) 
    { rootPath = rootPath
      logary = logary
      ilogger = ilogger
      cancelled = cancelled |> Option.defaultWith (fun () -> upcast IVar ())
      onSuccess = defaultArg onSuccess Impl.onSuccess
      onError = defaultArg onError Impl.onError
      webConfig = { defaultConfig with bindings = binding :: [] }
    }

module HTTP =
  open Impl
  open Logary.Adapters.Facade
  
  let api (config: HTTPConfig) next: WebPart =
    setMimeType "application/json; charset=utf-8" >=> choose [
      GET >=> path config.rootPath >=> printHelp config
      POST >=> path config.rootPath >=> Impl.ingestWith (config.onSuccess, config.onError) next
    ]
    
  let recv (started: IVar<unit>, shutdown: IVar<unit>) config next =
    job {
      LogaryFacadeAdapter.initialise<Suave.Logging.Logger> config.logary
      do config.ilogger.info (eventX "Starting HTTP recv-loop at {bindings}" >> setField "bindings" config.webConfig.bindings)
      use cts = createAdaptedCTS config
      let suaveConfig = { config.webConfig with cancellationToken = cts.Token }
      let webStarted, webListening = startWebServerAsync suaveConfig (api config next)
      do! Job.start (Job.fromAsync webListening |> Job.bind (IVar.fill shutdown))
      do! Job.start (Job.fromAsync webStarted |> Job.bind (fun _ -> IVar.fill started ()))
      do! config.cancelled
      do config.ilogger.info (eventX "Stopping HTTP recv-loop at {bindings}" >> setField "bindings" config.webConfig.bindings)
    }
  
  let create: ServerFactory<HTTPConfig> =
    IngestServer.create recv