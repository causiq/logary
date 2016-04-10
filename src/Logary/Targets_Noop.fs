module Logary.Targets.Noop

open Hopac
open Hopac.Infixes

open Logary
open Logary.Target
open Logary.Internals

// This is representative of the config you 
// would use for the target you are creating
type NoopConf =
  { isYes : bool }

let empty = { isYes = true }

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =

  // This is a placeholder for specific implementations
  type State = { state : bool }

  let loop (conf : NoopConf) (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =
    let rec loop (state : State) : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          ack *<= () :> Job<_>

        // 'When there is a request' call this function
        RingBuffer.take requests ^=> function
          | Log (_, ack) ->
            job {
              // do something with the message
              // specific to the target you are creating

              // This is a simple acknowledgement using unit as the signal
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
