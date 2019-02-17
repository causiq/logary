module Logary.Services.Rutta.Program

open Argu
open System
open System.Configuration
open System.Threading
open System.Runtime.InteropServices
open Topshelf
open Logary
open Logary.Services.Rutta

let versionAndName = sprintf "Logary Rutta v%s" AssemblyVersionInformation.AssemblyVersion

let maybeSubcommand (argv: string[]): string[] =
  let found = ConfigurationManager.AppSettings.["subcommand"]
  if isNull found then argv else
  let subArgv = ConfigurationManager.AppSettings.[found]
  if isNull subArgv then [| yield found; yield! argv |] else
  let subCmdArgv = subArgv.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
  [| yield found; yield! subCmdArgv; yield! argv |]

let executeProxy (args: ParseResults<ProxySubCommand>) =
  Proxy.proxy (args.GetResult Xsub_Connect_To) (args.GetResult Xpub_Bind)

let executeRouter (ilevel: LogLevel) (args: ParseResults<RouterSubCommand>) =
  let listeners = args.GetResults RouterSubCommand.Listener
  let targets = args.PostProcessResults(RouterSubCommand.Target, Parsers.targetConfig)
  if List.isEmpty listeners || List.isEmpty targets then
    failwith "Router `--listener` arguments empty, or `--target` arguments empty"
  Router.start ilevel targets listeners

let executeShipper (cmdRes: ParseResults<ShipperSubCommand>) =
  match cmdRes.GetAllResults() |> List.head with
  | Pub_To binding ->
    Shipper.pubTo binding
  | Push_To connect ->
    Shipper.pushTo connect
    
let inline executeParser argv (exiting: ManualResetEventSlim) (subParser: ArgumentParser<'SubCommand>) executeCommand cmdRes =
  // --help is a workaround for https://github.com/fsprojects/Argu/issues/113#issuecomment-464390860
  if Array.contains "--help" argv then
    eprintfn "%s" (subParser.PrintUsage())
    0
  else
    try
      let results = subParser.Parse(argv.[1..], ignoreMissing=false, ignoreUnrecognized=false, raiseOnUsage=false)
      if results.IsUsageRequested then
        eprintfn "%s" (subParser.PrintUsage())
        21
      else
        use running = executeCommand cmdRes
        exiting.Wait()
        0
    // A subcommand without parameters; workaround for https://github.com/fsprojects/Argu/issues/113#issuecomment-464390860
    with :? ArguParseException as e ->
      eprintfn "%s" e.Message
      22
      
let executeInner argv exiting (parser: ArgumentParser<Args>) (results: ParseResults<Args>) =
  if results.Contains Version || results.IsUsageRequested then
    printfn "%s" (parser.PrintUsage())
    0
  else
    let ilevel = if results.Contains Args.Verbose then LogLevel.Verbose else LogLevel.Info
    use health = results.TryGetResult Args.Health |> Option.map Parsers.binding |> Health.startServer
    match results.TryGetSubCommand() with
    | Some (Proxy cmd) ->
      let subParser = parser.GetSubCommandParser Proxy
      executeParser argv exiting subParser executeProxy cmd
        
    | Some (Router cmd) ->
      let subParser = parser.GetSubCommandParser Router
      executeParser argv exiting subParser (executeRouter ilevel) cmd
      
    | Some (Shipper cmd) ->
      let subParser = parser.GetSubCommandParser Shipper
      executeParser argv exiting subParser executeShipper cmd

    | _
    | None ->
      eprintfn "%s" (parser.PrintUsage())
      10
      
let execute argv (exiting: ManualResetEventSlim): int =
  let argv = maybeSubcommand argv
  let parser = ArgumentParser.Create<Args>(programName = "rutta.exe", helpTextMessage = versionAndName, checkStructure = Help.checkStructure)
  try
    let results = parser.Parse(argv, ignoreMissing=false, ignoreUnrecognized=true, raiseOnUsage=false)
    executeInner argv exiting parser results
  with :? ArguParseException as e ->
    printfn "%s" e.Message
    11

///////////// WINDOWS //////////////
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
  |> withRecovery (
      defaultServiceRecovery
      |> restart (Time.s 5)
      |> restart (Time.s 5)
      |> restart (Time.s 5))
  |> withStart start
  |> withStop (fun hc -> exiting.Set() ; stop hc)
  |> run

///////////// UNIX //////////////
let startUnix argv: int =
  let exiting = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe(fun _ -> exiting.Set())
  execute argv exiting

////////////// BOTH //////////////
[<EntryPoint>]
let main argv =
  let osDesc = RuntimeInformation.OSDescription
  eprintfn "Rutta %s running on '%s'. ZMQ v%O." AssemblyVersionInformation.AssemblyFileVersion osDesc fszmq.ZMQ.version
  eprintfn ""
  let isDashed = argv.Length >= 1 && argv.[0] = "--"
  if osDesc.Contains "Linux" || osDesc.Contains "Unix" || osDesc.Contains "Darwin" || isDashed then
    startUnix (if isDashed then argv.[1..] else argv)
  else
    startWindows argv