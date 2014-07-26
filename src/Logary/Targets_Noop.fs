module Logary.Targets.Noop

open FSharp.Actor

open Logary
open Logary.Target
open Logary.Internals

type NoopConf =
  { isYes : bool }

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

let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<TextWriter.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.IsYes(yes : bool) =
    ! (callParent <| Builder({ conf with isYes = yes }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ isYes = false }, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
