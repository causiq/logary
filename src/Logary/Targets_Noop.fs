module Logary.Targets.Noop

open Hopac
open Hopac.Infixes

open Logary
open Logary.Target
open Logary.Internals

type NoopConf =
  { isYes : bool }

let empty = { isYes = true }

module internal Impl =

  type State = { state : bool }

  let loop (conf : NoopConf) (ri : RuntimeInfo)
           (requests : BoundedMb<_>)
           (shutdown : Ch<_>) =
    let rec loop (state : State) : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          ack *<= () :> Job<_>

        BoundedMb.take requests ^=> function
          | Log (_, ack) ->
            job {
              do! ack *<= ()
              return! loop { state = not state.state }
            }

          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! loop { state = not state.state }
            }
      ] :> Job<_>

    loop { state = false }

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.IsYes(yes : bool) =
    ! (callParent <| Builder({ conf with isYes = yes }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ isYes = false }, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
