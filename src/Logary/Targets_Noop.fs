module Logary.Targets.Noop

open FSharp.Actor

open Logary
open Logary.Target
open Logary.Internals

type NoopConf =
  { isYes : bool }

let empty = { isYes = true }

module internal Impl =

  type State = { state : bool }

  let loop (conf : NoopConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec loop state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | Log l ->
        return! loop state
      | Measure msr ->
        return! loop state
      | Flush ackChan ->
        ackChan.Reply Ack
        return! loop state
      | Shutdown ackChan ->
        ackChan.Reply Ack
        return ()
      }

    loop { state = false }

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# Interop: Create a new Noop target
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.IsYes(yes : bool) =
    ! (callParent <| Builder({ conf with isYes = yes }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ isYes = false }, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
