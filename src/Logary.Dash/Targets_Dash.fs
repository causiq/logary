module Logary.Targets.Dash

open System.Reflection
open System.Threading

open FSharp.Actor

open Suave
open Suave.Types
open Suave.Web
open Suave.Http
open Suave.Http.Successful
open Suave.Http.Applicatives
open Suave.Http.Embedded
open Suave.Http.RequestErrors
open Suave.Http.Writers

open Logary
open Logary.Target
open Logary.Internals

type DashConf =
  { suaveConfig : SuaveConfig }

/// the default configuration for the web server
let empty =
  { suaveConfig = Suave.Web.default_config }

module internal Impl =

  type State =
    { webServer : CancellationTokenSource
    }

  let app =
    let from = Assembly.GetAssembly typeof<DashConf>
    choose [
      url "/" >>= resource from "index.full.html"
      url "/json" >>= set_mime_type "application/json" <|> OK "{}"
      url "/health" >>= set_mime_type "application/json" <|> OK "{}"
      NOT_FOUND "Did not find route"
      ]

  let formatDashInfo (startedInfo : Tcp.StartedData option []) =
    Array.fold (fun s t -> t |> function | None -> s | Some x -> s + sprintf "%O" x)
               "" startedInfo

  let loop (conf : DashConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let log = LogLine.setPath "Logary.Targets.Dash.loop" >> Logger.log ri.logger

    let rec init () = async {
      let listen, server = app |> web_server_async conf.suaveConfig
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
        return shutdown state
      }

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