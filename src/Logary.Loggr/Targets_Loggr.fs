module Logary.Targets.Loggr

open FSharp.Actor

open Logary
open Logary.Target
open Logary.Internals

open Loggr

type LoggrConf =
  { /// Specify the log id of the log you're logging to in loggr.
    logKey  : string
    /// Specify the Loggr API key
    apiKey  : string
    /// Whether to use TLS; defaults to true
    useTLS  : bool
    /// the user key in the map of data of the log line; defaults to 'user'.
    userKey : string }

/// Empty loggr configuration
let empty =
  { logKey  = ""
    apiKey  = ""
    useTLS  = true
    userKey = "user" }

/// Internal target implementation
module internal Impl =

  type State = { client : LogClient }

  let loop (conf : LoggrConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec init () = async {
      let client = LogClient(conf.logKey, conf.apiKey, conf.useTLS)
      return! running { client = client }
      }
    and running state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | Log l ->
        let evt =
          match l.``exception`` with
          | None    -> Events.CreateFromVariable(l.data)
          | Some ex -> Events.CreateFromException ex
        evt.Text l.message |> ignore
        evt.Tags (l.tags |> Array.ofList) |> ignore
        evt.Timestamp (l.timestamp.ToDateTimeUtc ()) |> ignore
        evt.UseLogClient state.client |> ignore
        evt.User 
          (l.data |> Map.tryFind conf.userKey
            |> Option.bind (function :? string as s -> Some s | _ -> None)
            |> Option.fold (fun _ t -> t) "")
          |> ignore

        evt.Post true |> ignore
        return! running state

      | Measure msr ->
        let evt = Events.CreateFromVariable msr.m_data
        evt.Value (msr |> Measure.getValueFloat) |> ignore
        evt.UseLogClient state.client |> ignore
        evt.Post true |> ignore
        return! running state

      | Flush ackChan ->
        ackChan.Reply Ack
        return! running state
      | Shutdown ackChan ->
        ackChan.Reply Ack
        return shutdown state
      }
    and shutdown _ =
      ()

    init ()

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# Interop: Create a new Noop target
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

/// Use with LogaryFactory.New( s => s.Target<Loggr.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  /// Specify the key that the service user that the log line relates is, is
  /// is saved as in the dictionary or anonymous object given to the Log
  /// function.
  member x.DictionaryUserKey(key : string) =
    Builder({ conf with userKey = key }, callParent)

  /// Feed NSA some intel for free
  member x.DoNotUseSecureTransport() =
    Builder({ conf with useTLS = false }, callParent)

  /// Specify the Loggr API key
  member x.ApiKey(apiKey : string) =
    Builder({ conf with apiKey = apiKey }, callParent)

  /// Specify the log id of the log you're logging to in loggr.
  member x.LogKey(logKey : string) =
    Builder({ conf with logKey = logKey }, callParent)

  /// Finish configuring loggr. You need to call this or you can't complete.
  member x.Done() =
    ! ( callParent x )

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
