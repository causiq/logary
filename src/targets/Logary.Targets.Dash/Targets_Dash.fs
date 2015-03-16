module Logary.Targets.Dash

open System.Threading

open FSharp.Actor

open Suave
open Suave.Types
open Suave.Web
open Suave.Http.Successful

open Logary
open Logary.Target
open Logary.Internals

type DashConf =
  { suaveConfig : SuaveConfig }

module internal Impl =

  type State =
    { webServer : CancellationTokenSource
    }

  let app = OK "Hello World"

  let formatDashInfo (startedInfo : Tcp.StartedData option []) =
    Array.fold (fun s t -> t |> function | None -> s | Some x -> s + sprintf "%O" x)
               "" startedInfo

  let loop (conf : DashConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let log = LogLine.setPath "Logary.Targets.Dash.loop" >> Logger.log ri.logger

    let rec init () = async {
      let listen, server = app |> startWebServerAsync conf.suaveConfig
      let cts = new CancellationTokenSource()
      Async.StartImmediate(server, cts.Token)
      let! started = listen
      LogLine.info (formatDashInfo started) |> log
      return! running { webServer = cts }
      }

    and running state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | Log l ->
        return! running state

      | Measure msr ->
        return! running state

      | Flush ackChan ->
        ackChan.Reply Ack
        return! running state

      | Shutdown ackChan ->
        ackChan.Reply Ack
        return shutdown state }

    and shutdown state =
      state.webServer.Dispose()
      ()

    init ()

/// Create a new dashboard target
let create conf = Target.TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# interop: create a new dashboard target
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name