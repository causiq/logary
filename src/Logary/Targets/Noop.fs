// DOCUMENT YOUR MODULE
module Logary.Targets.Noop

open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals

// This is representative of the config you would use for the target you are
// creating

/// DOCUMENT YOUR CONFIG
type NoopConf =
    /// Most often we agree – document your fields
  { isYes : bool }

let empty = { isYes = true }

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =

  // This is a placeholder for specific state that your target requires
  type State = { state : bool }

  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf : NoopConf) // the conf is specific to your target
           (ri : RuntimeInfo) // this one,
           (requests : RingBuffer<_>) // this one, and,
           (shutdown : Ch<_>) = // this one should always be taken in this order

    let rec loop (state : State) : Job<unit> =
      // Alt.choose will pick the channel/alternative that first gives a value
      Alt.choose [
        // When you get the shutdown value, you need to dispose of your resources
        // off of Hopac's execution context (see Scheduler.isolate below) and
        // then send a unit to the ack channel, to tell the requester that
        // you're done disposing.
        shutdown ^=> fun ack ->
          // do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          ack *<= () :> Job<_>

        // The ring buffer will fill up with messages that you can then consume below.
        // There's a specific ring buffer for each target.
        RingBuffer.take requests ^=> function
          // The Log discriminated union case contains a message which can have
          // either an Event or a Gauge `value` property.
          | Log (message, ack) ->
            job {
              // Do something with the `message` value specific to the target
              // you are creating.

              // This is a simple acknowledgement using unit as the signal
              do! ack *<= ()
              return! loop { state = not state.state }
            }

          // Since the RingBuffer is fair, when you receive the flush message, all
          // you have to do is ensure the previous Messages were successfully written
          // and then ack. Alternatively the caller can decide it's not worth the wait
          // and signal the nack, in which case you may try to abort the flush or
          // simply continue the flush in the background.
          | Flush (ackCh, nack) ->
            job {
              // Put your flush logic here...

              // then perform the ack
              do! Ch.give ackCh () <|> nack

              // then continue processing messages
              return! loop { state = not state.state }
            }
      // The target is always 'responsive', so we may commit to the alternative
      // by upcasting it to a job and returning that.
      ] :> Job<_>

    // start the inner loop by the exit of the outer loop function
    loop { state = false }

/// Create a new YOUR TARGET NAME HERE target
[<CompiledName "Create">]
let create conf name = TargetUtils.stdNamedTarget (Impl.loop conf) name

// The Builder construct is a DSL for C#-people. It's nice for them to have
// a DSL where you can't make mistakes. The general idea is that first 'new'
// is called, and you get the callback to that function. Then you can put
// methods on this Builder class which are exposed to the caller (configuration
// code).

/// Use with LogaryFactory.New( s => s.Target<YOUR TARGET NAME.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  // place your own configuration methods here

  member x.IsYes(yes : bool) =
    ! (callParent <| Builder({ conf with isYes = yes }, callParent))

  // your own configuration methods end here

  // c'tor, always include this one in your code
  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ isYes = false }, callParent)

  // this is called in the end, after calling all your custom configuration
  // methods (above) which in turn take care of making the F# record that
  // is the configuration, "just so"
  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
