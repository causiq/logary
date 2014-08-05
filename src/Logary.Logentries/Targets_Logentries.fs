module Logary.Targets.Logentries

open System
open System.IO
open System.Net.Sockets
open System.Text

open FSharp.Actor

open Logary
open Logary.Target
open Logary.Internals

/// Configuration for logentries
type LogentriesConf =
  { token      : string
    accountKey : string
    flush      : bool
    formatter  : Formatting.StringFormatter }

/// Empty logentries configuration
let empty =
  { token      = ""
    accountKey = ""
    flush      = false
    // see https://logentries.com/doc/search/#syntax
    formatter  = Formatting.JsonFormatter.Default () }

/// Logentries internal implementations
module internal Impl =
  open System
  open Logary.Logenties

  type State = { conn : TcpClient * Stream }

  [<Literal>]
  let LineSep = "\u2028"

  let Newlines = [ "\r\n"; "\n" ]

  let utf8 = Encoding.UTF8

  let munge (token : string) (msg : string) =
    String.Concat [|
      token
      ""
      (Newlines |> List.fold (fun (s : string) t -> s.Replace(t, LineSep)) msg)
      "\n"
    |]

  let loop (conf : LogentriesConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec init () = async {
      let client, stream = LeClient.create ()
      return! running { conn = client, upcast stream }
      }

     and running ({ conn = client, stream } as state) = async {
      let munge = munge conf.token
      let! msg, _ = inbox.Receive()
      match msg with
      | Log l ->
        // see https://logentries.com/doc/search/#syntax
        let msg = l |> conf.formatter.format |> munge |> utf8.GetBytes
        do! LeClient.send msg stream conf.flush
        return! running state

      | Measure msr ->
        // doing key-value-pair style
        let msg = sprintf "%s=%f" msr.m_path (msr |> Measure.getValueFloat)
                  |> munge |> utf8.GetBytes
        do! LeClient.send msg stream conf.flush
        return! running state

      | Flush ackChan ->
        ackChan.Reply Ack
        return! running state

      | Shutdown ackChan ->
        Try.safe "disposing Logentries clients" ri.logger <| fun () ->
          (stream :> IDisposable).Dispose()
          (client :> IDisposable).Dispose()
        ackChan.Reply Ack
        return ()
      }

    init ()

/// Create a new Logentries target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# Interop: Create a new Logentries target
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

/// Use with LogaryFactory.New( s => s.Target<Logentries.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  /// Specify your API token
  member x.Token(token : string) =
    Builder({ conf with token = token }, callParent)

  /// Specify your account key
  member x.AccountKey(key : string) =
    Builder({ conf with accountKey = key }, callParent)

  member x.ForceFlush() =
    Builder({ conf with flush = true }, callParent)

  member x.Formatter(formatter : Formatting.StringFormatter) =
    Builder({ conf with formatter = formatter }, callParent)

  member x.Done () =
    ! (callParent x)

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
