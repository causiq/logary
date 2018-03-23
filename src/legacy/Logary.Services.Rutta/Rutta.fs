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
  | Router_Stream of streamBindSocket:string
  | Router_Target of logaryTargetUri:string
  | Proxy of xsubConnectToSocket:string * xpubBindSocket:string
  | Health of ip:string * port:int
  | No_Health
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Push_To _ -> "Runs Rutta in Shipper/PUSH mode (send Messages from a node to router)"
      | Pub_To _ -> "Runs Rutta in Shipper/PUB mode (send Messages from a node to proxy)"
      | Router _ -> "Runs Rutta in Router mode (PULL fan-in of Messages, forward to Target)."
      | Router_Sub _ -> "Runs Rutta in Router XSUB mode for PUB sockets to publish to."
      | Router_Stream _ -> "Runs Rutta in TCP STREAM server mode, that allows zmq to be used as a TCP socket. Send newline-separated JSON to this bound socket. See http://api.zeromq.org/4-0:zmq-socket#toc19"
      | Router_Target _ -> "Implied by --router. Specifies where the Router target should forward its data"
      | Proxy (_,_) -> "Runs Rutta in Proxy mode (XSUB/fan-in of Messages, forward to SUB sockets via XPUB). The first is the XSUB socket (in), the second is the XPUB socket (out)."
      | Health _ -> "Give Rutta health binding information"
      | No_Health _ -> "Don't bind a health endpoint on start."

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

module Shipper =

  open System
  open System.Threading
  open MBrace.FsPickler
  open NodaTime
  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Metrics
  open Logary.Target
  open Logary.Targets
  open Logary.Internals
  open Logary.Configuration
  open fszmq
  open fszmq.Socket
  open Logary.EventsProcessing

  let private runLogary shipperConf =

    use mre = new ManualResetEventSlim(false)
    use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())
    let hostName = System.Net.Dns.GetHostName()

    let systemMetrics =
      let sys = WinPerfCounters.systemMetrics (PointName.parse "sys")
      Events.events
      |> Pipe.tickTimer sys (TimeSpan.FromMilliseconds 400.)

    let processing =
      Events.compose [
        systemMetrics
        |> Pipe.map Array.toSeq
        |> Events.flattenToProcessing
        |> Events.sink ["rutta-shipper"]
      ]

    let logary =
      Config.create "Rutta" hostName
      |> Config.targets [
          //Noop.create (Noop.empty) (PointName.ofSingle "noop")
          //Console.create (Console.empty) (PointName.ofSingle "console")
          Shipper.create shipperConf "rutta-shipper"
        ]
      |> Config.loggerLevels [ ".*", Verbose ]
      |> Config.processing processing
      |> Config.ilogger (ILogger.Console Debug)
      |> Config.build
      |> Hopac.Hopac.run

    mre.Wait()
    Choice1Of2 ()

  let internal pushTo connectTo pars: Choice<unit, string> =
    printfn "%s" "spawning shipper in PUSH mode"
    runLogary (Shipper.PushTo connectTo)

  let internal pubTo connectTo pars: Choice<unit, string> =
    printfn "%s" "spawning shipper in PUB mode"
    runLogary (Shipper.PublishTo connectTo)

module Router =
  open Hopac
  open System
  open System.Text
  open System.IO
  open Chiron
  open Logary
  open Logary.Message
  open Logary.Configuration
  open Logary.Targets
  open Logary.EventsProcessing
  open Logary.Formatting
  open fszmq
  open MBrace.FsPickler
  open MBrace.FsPickler.Combinators

  /// TODO: move this module to Logary.Configuration.Uri
  module TargetConfig =
    let modu name = sprintf "Logary.Targets.%s" name
    let asm name = sprintf "Logary.Targets.%s" name
    let conf name = sprintf "%sConf" name

    let moduleNameConfigName modu asm conf =
      sprintf "%s, %s" modu asm,
      sprintf "%s+%s, %s" modu conf asm

    let moduleNameConfigNameAsm name =
      moduleNameConfigName (modu name) (asm name) (conf name)

    type DynamicConfig =
      { configType: Type
        moduleName: string
        moduleType: Type
      }
      /// Creates the default target configuration particular to the target.
      member x.getDefault () =
        if isNull x.moduleType then
          failwithf "Module '%s' did not resolve. Do you have its DLL next to rutta.exe?" x.moduleName

        let defaultEmpty = x.moduleType.GetProperty("empty")
        if isNull defaultEmpty then
          failwithf "Module '%s' did not have a default config value named 'empty'." x.moduleName

        defaultEmpty.GetValue(null)

      /// Creates the final Logary TargetConf value that logary uses to build the target.
      member x.createTargetConf (conf: obj) (name: string): TargetConf =
        if isNull x.moduleType then
          failwithf "Module '%s' did not have 'empty' conf-value. This should be fixed in the target's code."
                    x.moduleName

        let createMethod = x.moduleType.GetMethod("Create")
        if isNull createMethod then
          failwithf "Module '%s' did not have 'create' \"(name: string) -> (conf: 'conf) -> TargetConf\" function. This should be fixed in the target's code (with [<CompiledName \"Create\">] on itself)."
                    x.moduleName

        printfn "Invoking create on '%O'" createMethod
        createMethod.Invoke(null, [| conf; name |])
        :?> TargetConf

      static member create configType moduleName moduleType =
        //printfn "Create DynamicConfig with (configType=%A, moduleName=%A, moduleType=%A)" configType moduleName moduleType
        { configType = configType
          moduleName = moduleName
          moduleType = moduleType }

    let schemeToConfAndDefault =
      [ "influxdb",    moduleNameConfigNameAsm "InfluxDb"
        "stackdriver", moduleNameConfigNameAsm "Stackdriver"
        "console",     moduleNameConfigName (modu "LiterateConsole") "Logary" (conf "LiterateConsole")
      ]
      |> List.map (fun (scheme, (moduleName, configName)) ->
        let confType = Type.GetType configName
        let moduleType = Type.GetType moduleName
        scheme, DynamicConfig.create confType moduleName moduleType)
      |> Map

    let create (targetUri: Uri): TargetConf =
      printfn "Creating a new target from URI: '%O'" targetUri
      let scheme = targetUri.Scheme.ToLowerInvariant()
      match schemeToConfAndDefault |> Map.tryFind scheme with
      | None ->
        failwithf "Rutta has not get got support for '%s' targets" scheme
      | Some dynamicConfig ->
        let configDefault = dynamicConfig.getDefault ()
        Uri.parseConfig dynamicConfig.configType configDefault targetUri
        |> fun config -> dynamicConfig.createTargetConf config scheme

  type State =
    { zmqCtx: Context
      receiver: Socket
      forwarder: LogManager
      logger: Logger }
    interface IDisposable with
      member x.Dispose() =
        (x.zmqCtx :> IDisposable).Dispose()
        (x.receiver :> IDisposable).Dispose()

  let private init binding (targetUris: Uri list) createSocket mode: State =
    let context = new Context()
    let receiver = createSocket context

    let forwarder =
      let hostName = System.Net.Dns.GetHostName()
      let targets = targetUris |> List.map TargetConfig.create

      Config.create (sprintf "Logary Rutta[%s]" mode) hostName
      |> Config.targets targets
      |> Config.processing (
          Events.events
          |> Events.sink (targets |> List.map (fun t -> t.name)))
      |> Config.loggerMinLevel ".*" Verbose
      |> Config.ilogger (ILogger.LiterateConsole Debug)
      |> Config.build
      |> Hopac.Hopac.run

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

      Logger.logWithAck logger message.level (fun _ -> message)
      |> run
      |> start

      recvLoop receiver logger

  let pullFrom binding = function
    | Router_Target target :: _ ->
      printfn "Spawning router in PULL mode from %s" binding
      use state = init binding [ Uri target ] Context.pull "PULL"
      Socket.bind state.receiver binding
      Choice1Of2 (recvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "Unknown parameter(s) %A" x)

  let xsubBind binding = function
    | Router_Target target :: _ ->
      printfn "Spawning router in SUB mode from %s" binding
      use state = init binding [ Uri target ] Context.sub "SUB"
      Socket.subscribe state.receiver [""B]
      Socket.connect state.receiver binding
      Choice1Of2 (recvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "Unknown parameter(s) %A" x)

  let rec private streamRecvLoop receiver (logger: Logger) =
    // https://gist.github.com/lancecarlson/fb0cfd0354005098d579
    // https://gist.github.com/claws/7231548#file-czmq-stream-server-c-L21
    try
      let frame = Socket.recv receiver
      // Aborted socket due to closing process:
      if isNull frame then () else
      let bs = Socket.recv receiver
      // http://api.zeromq.org/4-1:zmq-socket#toc19
      // A connection was made
      if bs.Length = 0 then streamRecvLoop receiver logger else
      // printfn "Data received: %A" message
      use ms = new MemoryStream(bs)
      use sr = new StreamReader(ms, Encoding.UTF8)
      while not sr.EndOfStream do
        let line = sr.ReadLine()
        match Json.parse line |> JsonResult.bind Json.decodeMessage with
        | JPass message ->
          logger.logSimple message
        | JFail failure ->
          logger.verbose (eventX "JFail: {line} => {failure}" >> setField "line" line >> setField "failure" failure)
      //printfn "Looping..."

      streamRecvLoop receiver logger

    with :? ZMQError as zmq ->
      logger.info (eventX "Shutting down streamRecvLoop due to {exn}." >> setField "exn" zmq.Message >> addExn zmq)
      ()

  let streamBind binding = function
    | Router_Target target :: _ ->
      printfn "Spawning router in STREAM mode from %s" binding
      use state = init binding [ Uri target ] Context.stream "STREAM"
      Socket.bind state.receiver binding
      Choice1Of2 (streamRecvLoop state.receiver state.logger)

    | x ->
      Choice2Of2 (sprintf "Unknown parameter(s) %A" x)

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

    printfn "%s" "Spawning proxy"
    Choice1Of2 (Proxying.proxy reader writer None)

module Program =

  open System
  open System.Threading
  open Topshelf

  let detailedParse: _ -> _ -> Choice<string * _ * _, _, string> = function
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

      | Router_Stream binding ->
        Choice1Of3 ("router stream", Router.streamBind binding, pars)

      | Proxy (xsubBind, xpubBind) ->
        Choice1Of3 ("proxy", Proxy.proxy xsubBind xpubBind, pars)

      | Router_Target _ as par ->
        Choice2Of3 (par :: pars)

      | Health _ ->
        Choice2Of3 pars

      | No_Health _ ->
        Choice2Of3 pars

    | Choice3Of3 msg ->
      fun _ -> Choice3Of3 msg

  let execute argv (exiting: ManualResetEventSlim): int =
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

  let startWindows argv: int =
    let exiting = new ManualResetEventSlim(false)

    let enqueue f =
      ThreadPool.QueueUserWorkItem(fun _ -> f ()) |> ignore

    let start hc =
      enqueue (fun _ -> execute argv exiting |> ignore)
      true

    let stop hc =
      exiting.Dispose()
      true

    defaultService
    |> withRecovery (defaultServiceRecovery |> restart (Time.s 5))
    |> withStart start
    |> withStop (fun hc -> exiting.Set() ; stop hc)
    |> run

  let startUnix argv: int =
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