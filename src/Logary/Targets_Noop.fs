module Logary.Targets.Noop

open Hopac

open Logary
open Logary.Target
open Logary.Internals

type NoopConf =
  { isYes : bool }

let empty = { isYes = true }

module internal Impl =

  type State = { state : bool }

  let loop (conf : NoopConf) (ri : RuntimeInfo) (reqCh : Ch<_>) =
    let rec loop state = job {
      let! msg = Ch.take reqCh
      match msg with
      | Log _ ->
        return! loop state
      | Flush ack ->
        do! IVar.fill ack Ack
        return! loop state
      | Shutdown ack ->
        do! IVar.fill ack Ack
        return ()
      }

    loop { state = false }

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# Interop: Create a new Noop target
[<CompiledName "Create">]
let createInterop (conf, name) =
  create conf name

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.IsYes(yes : bool) =
    ! (callParent <| Builder({ conf with isYes = yes }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ isYes = false }, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
