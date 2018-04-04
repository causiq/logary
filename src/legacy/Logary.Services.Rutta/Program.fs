module Logary.Services.Rutta.Program

open System.Reflection
[<assembly: AssemblyTitle("Logary Rutta – a router/proxy/shipper for Windows and Unix")>]
()

open Argu
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

    | Router_TCP binding ->
      // TODO: support multiple Router_BINDING_TYPE arguments
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
  let parser = ArgumentParser.Create<Args>(programName = "rutta.exe")
  let parsed = parser.Parse(argv, ignoreUnrecognized=true)

  parsed.GetAllResults()
  |> List.fold detailedParse (Choice2Of3 [])
  |> function
  // Choice1Of3 = mode found
  // Choice2Of3 = no mode found
  // Choice3Of3 = more than one mode found
  | Choice1Of3 (modeName, start, pars) ->
    use health =
      parsed.GetResult(Health, defaultValue = ("127.0.0.1", 8888))
      ||> Health.startServer

    match start pars with
    | Choice1Of2 () ->
      exiting.Wait()
      0

    | Choice2Of2 error ->
      eprintfn "%s" error
      2

  | Choice2Of3 pars ->
    eprintfn "No mode given. You must pass one of: { --push-to, --pub-to, --router, --router-sub, --router-stream, --proxy } for Rutta to work."
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