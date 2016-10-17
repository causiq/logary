namespace Logary.Services.Rutta
open System.Reflection
[<assembly: AssemblyTitle("Logary Rutta – a router/proxy/shipper for Windows and Unix")>]
()

open Argu

type Args =
  | Push_To of pushConnectToSocket:string
  | Pub_To of pubBindSocket:string
  | Router of pullBindSocket:string
  | Router_Sub of subConnectSocket:string
  | Router_Target of logaryTargetUri:string
  | Proxy of xsubConnectToSocket:string * xpubBindSocket:string
  | Health of ip:string * port:int
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Push_To _ -> "Runs Rutta in Shipper/PUSH mode (send Messages from a node to router)"
      | Pub_To _ -> "Runs Rutta in Shipper/PUB mode (send Messages from a node to proxy)"
      | Router _ -> "Runs Rutta in Router mode (PULL fan-in of Messages, forward to Target)."
      | Router_Sub _ -> "Runs Rutta in Router XSUB mode for PUB sockets to publish to."
      | Router_Target _ -> "Implied by --router. Specifies where the Router target should forward its data"
      | Proxy (_,_) -> "Runs Rutta in Proxy mode (XSUB/fan-in of Messages, forward to SUB sockets via XPUB). The first is the XSUB socket (in), the second is the XPUB socket (out)."
      | Health _ -> "Give Rutta binding information"

module Health =
  open Logary
  open System
  open System.Threading
  open Suave
  open Suave.Operators
  open Suave.Filters
  open Suave.Successful

  let app =
    GET >=> path "/health" >=> OK (sprintf "Logary Rutta %s" (App.getVersion ()))

  /// Start a Suave web server on the passed ip and port and return a disposable
  /// token to use to shut the server down.
  let startServer ip port =
    let cts = new CancellationTokenSource()

    let config =
      { defaultConfig with
          bindings = [ HttpBinding.mkSimple HTTP ip port ]
          cancellationToken = cts.Token }

    let ready, handle = startWebServerAsync config app
    Async.Start handle

    { new IDisposable with member x.Dispose() = cts.Cancel () }

module Shipper =

  open System
  open System.Threading
  open MBrace.FsPickler
  open NodaTime
  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Metric
  open Logary.Metrics
  open Logary.Target
  open Logary.Targets
  open Logary.Internals
  open Logary.Configuration
  open fszmq
  open fszmq.Socket
  open Logary.Targets.Shipper

  let private runLogary shipperConf =
    // TODO: handle with configuration, selection of what data points to
    // gather.
    let createMetric name metric =
      MetricConf.create (Duration.FromMilliseconds 400L) name metric

    use mre = new ManualResetEventSlim(false)
    use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

    use logary =
      withLogaryManager "Rutta" (
        withTargets [
          //Noop.create (Noop.empty) (PointName.ofSingle "noop")
          //Console.create (Console.empty) (PointName.ofSingle "console")
          create shipperConf "rutta-shipper"
        ]
        >> withMiddleware Middleware.host
        >> withMetrics [
          createMetric "systemMetrics" WinPerfCounters.systemMetrics
        ]
        >> withRules [
          //Rule.createForTarget (PointName.ofSingle "noop")
          //Rule.createForTarget (PointName.ofSingle "console")
          Rule.createForTarget "rutta-shipper"
        ]
        >> withInternalTargets Info [
          Console.create Console.empty "internal"
        ]
      )
      |> run

    mre.Wait()
    Choice.create ()

  let internal pushTo connectTo pars : Choice<unit, string> =
    printfn "%s" "spawning shipper in PUSH mode"
    runLogary (PushTo connectTo)

  let internal pubTo connectTo pars : Choice<unit, string> =
    printfn "%s" "spawning shipper in PUB mode"
    runLogary (PublishTo connectTo)

module Router =

  open Hopac
  open System
  open System.IO
  open Logary
  open Logary.Configuration
  open Logary.Targets
  open fszmq
  open MBrace.FsPickler
  open MBrace.FsPickler.Combinators

  type State =
    { zmqCtx    : Context
      receiver  : Socket
      forwarder : LogManager
      logger    : Logger }
    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.receiver :> IDisposable).Dispose()
        (x.forwarder :> IDisposable).Dispose()

  let private init binding targetUri createSocket mode : State =
    let context = new Context()
    let receiver = createSocket context    
    let config = Uri.parseConfig<InfluxDb.InfluxDbConf> InfluxDb.empty targetUri
    
    let forwarder =
      withLogaryManager (sprintf "Logary Rutta[%s]" mode) (
        withTargets [
          //Console.create Console.empty (PointName.ofSingle "console")
          InfluxDb.create config "influxdb"
        ]
        >> withRules [
          Rule.createForTarget "influxdb"
          //Rule.createForTarget (PointName.ofSingle "console")
        ]
      )
      |> run

    let targetLogger = forwarder.getLogger (PointName.parse "Logary.Services.Rutta.Router")

    { zmqCtx    = context
      receiver  = receiver
      forwarder = forwarder
      logger    = targetLogger }

  let rec private recvLoop receiver logger =
    match Socket.recvAll receiver with
    // note: sending empty messages
    | null | [||] -> ()
    | datas ->
      let message = Logary.Targets.Shipper.Serialisation.deserialise datas

      message
      |> Logger.logWithAck logger
      |> run
      |> queue

      recvLoop receiver logger

  let pullFrom binding = function
    | Router_Target target :: _ ->
      printfn "spawning router in PULL mode from %s" binding
      use state = init binding target Context.pull "PULL" 
      Socket.bind state.receiver binding
      Choice.create (recvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "unknown parameter(s) %A" x)

  let xsubBind binding = function
    | Router_Target target :: _ ->
      printfn "spawning router in SUB mode from %s" binding
      use state = init binding target Context.sub "SUB"
      Socket.subscribe state.receiver [""B]
      Socket.connect state.receiver binding
      Choice.create (recvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "unknown parameter(s) %A" x)

module Proxy =
  open Logary
  open fszmq
  open fszmq.Socket

  let proxy xsubBind xpubBind _ =
    printfn """Proxy usage: takes the XSUB read-socket (that PUB sockets CONNECT to) \
               and then the XPUB write-socket (that SUB sockets CONNECT to). \
               --proxy %s %s"""
            xsubBind xpubBind

    use context = new Context()
    use reader = Context.xsub context
    bind reader xsubBind

    use writer = Context.xpub context
    bind writer xpubBind

    printfn "%s" "spawning proxy"
    Choice.create (Proxying.proxy reader writer None)

module Program =

  open System
  open System.Threading
  open Topshelf
  
  let detailedParse : _ -> _ -> Choice<string * _ * _, _, string> = function
    // we already have a mode set
    | Choice1Of3 (modeName, start, pars) as curr -> function
      | Router_Target _ as par ->
        Choice1Of3 (modeName, start, par :: pars)

      // no mode cares about this:
      | Health _ ->
        curr

      // no other known flags that are not modes:
      | otherMode ->
        let msg =
          sprintf "%A given after having configured the '%s' mode; invalid parameters, exiting..."
            otherMode modeName
        Choice3Of3 msg

    // still collecting parameters
    | Choice2Of3 pars -> function
      | Push_To connect ->
        Choice1Of3 ("shipper push", Shipper.pushTo connect, pars)

      | Pub_To connect ->
        Choice1Of3 ("shipper pub", Shipper.pubTo connect, pars)

      | Router binding ->
        Choice1Of3 ("router pull", Router.pullFrom binding, pars)

      | Router_Sub binding ->
        Choice1Of3 ("router xsub", Router.xsubBind binding, pars)

      | Proxy (xsubBind, xpubBind) ->
        Choice1Of3 ("proxy", Proxy.proxy xsubBind xpubBind, pars)

      | Router_Target _ as par ->
        Choice2Of3 (par :: pars)

      | Health _ ->
        Choice2Of3 pars

    | Choice3Of3 msg ->
      fun _ -> Choice3Of3 msg

  let execute argv (exiting : ManualResetEventSlim) : int =
    let parser = ArgumentParser.Create<Args>()
    let parsed = parser.Parse argv

    parsed.GetAllResults()
    |> List.fold detailedParse (Choice2Of3 [])
    |> function
    // Choice1Of3 = mode found
    // Choice2Of3 = no mode found
    // Choice3Of3 = more than one mode found
    | Choice1Of3 (modeName, start, pars) ->
      use health =
        parsed.GetResult(<@ Health @>, defaultValue = ("127.0.0.1", 8888))
        ||> Health.startServer

      match start pars with
      | Choice1Of2 () ->
        exiting.Wait()
        0

      | Choice2Of2 error ->
        eprintfn "%s" error
        2

    | Choice2Of3 pars ->
      eprintfn "No mode given. You must pass one of: { --push-to, --pub-to, --router, --router-sub, --proxy } for Rutta to work."
      10

    | Choice3Of3 error ->
      eprintfn "%s" error
      20

  let startWindows argv : int =
    let exiting = new ManualResetEventSlim(false)

    let enqueue f =
      ThreadPool.QueueUserWorkItem(fun _ -> f ()) |> ignore

    let start hc =
      enqueue (fun _ -> execute argv exiting |> ignore)
      true

    let stop hc =
      exiting.Dispose()
      true

    Service.Default
    |> with_recovery (ServiceRecovery.Default |> restart (Time.s 5))
    |> with_start start
    |> with_stop (fun hc -> exiting.Set() ; stop hc)
    |> run

  let startUnix argv : int =
    let exiting = new ManualResetEventSlim(false)
    use sub = Console.CancelKeyPress.Subscribe(fun _ -> exiting.Set())
    execute argv exiting

  [<EntryPoint>]
  let main argv =
    let isDashed = argv.Length >= 1 && argv.[0] = "--"
    if Type.GetType "Mono.Runtime" <> null || isDashed then
      startUnix (if isDashed then argv.[1..] else argv)
    else
      startWindows argv
