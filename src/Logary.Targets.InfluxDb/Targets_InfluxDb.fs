module Logary.Targets.InfluxDb

open Hopac
open Hopac.Infixes

open Logary
open Logary.Target
open Logary.Internals
open System

let msgToString (message : Message) : string =
  let valueToString = function
    | Gauge (Float v,u) -> float v
    | Derived (Float v,u) -> float v
    | _ -> failwith ""

  sprintf 
    "%O value=%.2fi %i" 
    message.name 
    (valueToString message.value)
    message.timestamp



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
           (requests : BoundedMb<_>)
           (shutdown : Ch<_>) =
    let rec loop (state : State) : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          ack *<= () :> Job<_>

        // 'When there is a request' call this function
        BoundedMb.take requests ^=> function
          | Log (msg, ack) ->
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
