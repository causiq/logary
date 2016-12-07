// DOCUMENT YOUR MODULE
module Logary.Targets.Stackdriver

open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals
open Google.Logging.V2
open Google.Api

/// A structure that enforces certain labeling invariants around monitored resources
type ResourceType = 
  /// Compute resources have a zone and an instance id
  | Compute of zone: string * instance : string

type StackdriverConf = {
  /// The name of the Stackdriver log to write
  logName : string
  /// The resource we are monitoring
  resource : ResourceType
  // any additional labels to be added to the messages
  labels : Map<string,string>
}

let empty : StackdriverConf = failwith "boom" 

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =

  type State = {
    logger : LoggingServiceV2Client
    resource : MonitoredResource
    labels : System.Collections.Generic.IDictionary<string,string>
  }

  let write messsage = failwith "boom"
  let createState conf = failwith "boomState"

  let dispose conf runtimeInfo state ackVar = 
    // do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
    ackVar *<= () :> Job<_>

  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf : StackdriverConf) // the conf is specific to your target
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
        shutdown ^=> dispose conf ri state

        // The ring buffer will fill up with messages that you can then consume below.
        // There's a specific ring buffer for each target.
        RingBuffer.take requests ^=> function
          // The Log discriminated union case contains a message which can have
          // either an Event or a Gauge `value` property.
          | Log (message, ack) ->
            job {
              // Do something with the `message` value specific to the target
              // you are creating.
              let entry = write message
              // This is a simple acknowledgement using unit as the signal
              do! Job.Scheduler.isolate (fun _ -> state.logger.WriteLogEntries(conf.logName, state.resource, state.labels, [|entry|]) |> ignore)
              do! ack *<= ()
              return! loop state
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
              return! loop state
            }
      // The target is always 'responsive', so we may commit to the alternative
      // by upcasting it to a job and returning that.
      ] :> Job<_>
    
    let state = createState conf
    // start the inner loop by the exit of the outer loop function
    loop state

/// Create a new StackDriver target
[<CompiledName "Create">]
let create conf name = TargetUtils.stdNamedTarget (Impl.loop conf) name

// The Builder construct is a DSL for C#-people. It's nice for them to have
// a DSL where you can't make mistakes. The general idea is that first 'new'
// is called, and you get the callback to that function. Then you can put
// methods on this Builder class which are exposed to the caller (configuration
// code).

/// Use with LogaryFactory.New( s => s.Target<Stackdriver.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  // place your own configuration methods here
  // your own configuration methods end here

  // c'tor, always include this one in your code
  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  // this is called in the end, after calling all your custom configuration
  // methods (above) which in turn take care of making the F# record that
  // is the configuration, "just so"
  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
