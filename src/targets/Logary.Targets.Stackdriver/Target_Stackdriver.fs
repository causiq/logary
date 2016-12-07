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
/// TODO: add more resource types.
/// SEE: https://cloud.google.com/logging/docs/api/v2/resource-list#service-names
/// SEE: https://cloud.google.com/logging/docs/migration-v2#monitored-resources
type ResourceType = 
  /// Compute resources have a zone and an instance id
  | ComputeInstance of zone: string * instance : string
  /// Containers have cluster/pod/namespace/instance data
  | Container of clusterName : string * namespaceId : string * instanceId : string * podId : string * containerName : string * zone : string

type StackdriverConf = {
  /// Google Cloud Project Id
  projectId : string
  /// Name of the log to which to write
  logId : string
  /// The resource we are monitoring
  resource : ResourceType
  /// Any additional user labels to be added to the messages
  labels : Map<string,string>
  /// how many messages to wait for before sending
  batchSize : uint32
}

let empty : StackdriverConf = failwith "boom" 

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =

  type State = {
    logName : string
    logger : LoggingServiceV2Client
    resource : MonitoredResource
    labels : System.Collections.Generic.IDictionary<string,string>
  }

  let mkLabels project resource =
    match resource with
    | ComputeInstance(zone, instance) -> [ "project_id", project
                                           "zone", zone
                                           "instanceId", instance ]|> dict
    | Container(cluster, ns, instance, pod, container, zone) -> [ "project_id", project
                                                                  "cluster_name", cluster
                                                                  "namespace_id", ns
                                                                  "instance_id", instance
                                                                  "pod_id", pod
                                                                  "container_name", container
                                                                  "zone", zone ] |> dict

  let mkResource t labels = 
    let r =  MonitoredResource(Type = t)
    r.Labels.Add(labels)
    r
  
  let mkMonitoredResource project resourceType =
    let labels = mkLabels project resourceType
    match resourceType with 
    | ComputeInstance(zone, instance) -> mkResource "gce_instance" labels
    | Container(cluster, ns, instance, pod, container, zone) -> mkResource "container" labels 
  
  let write (messsage : Message) : LogEntry = 
    let entry = LogEntry()
    entry

  let writeRequests (conf : StackdriverConf) (state : State) (reqs : TargetMessage[]) =
      let messages = ResizeArray<_>() // Updated by the loop.
      let acks = ResizeArray<_>() // updated by the loop
      
      for m in reqs do
        match m with
        | Log (message, ack) ->
          // Write and save the ack for later.
          acks.Add ack
          messages.Add <| write message
        | Flush (ackCh, nack) -> ()

      let writer = Job.Scheduler.isolate <| fun _ -> state.logger.WriteLogEntries(state.logName, state.resource, state.labels, messages)   
      let handler = Job.tryIn writer Job.result (Job.raises)
      handler >>= fun res -> Job.conIgnore acks 
  
  let createState (conf : StackdriverConf) = 
    { logger = LoggingServiceV2Client.Create()
      resource = mkMonitoredResource conf.projectId conf.resource
      labels = conf.labels |> Map.toSeq |> dict
      logName = sprintf "projects/%s/logs/%s" conf.projectId conf.logId }

  let dispose runtime state ackVar = 
    // do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
    ackVar *<= () :> Job<_>

  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf : StackdriverConf) // the conf is specific to your target
           (runtime : RuntimeInfo) // this one,
           (requests : RingBuffer<_>) // this one, and,
           (shutdown : Ch<_>) = // this one should always be taken in this order

    let rec loop (state : State) : Job<unit> =
      // Alt.choose will pick the channel/alternative that first gives a value
      Alt.choose [
        // When you get the shutdown value, you need to dispose of your resources
        // off of Hopac's execution context (see Scheduler.isolate below) and
        // then send a unit to the ack channel, to tell the requester that
        // you're done disposing.
        shutdown ^=> dispose runtime state

        // The ring buffer will fill up with messages that you can then consume below.
        // There's a specific ring buffer for each target.
        RingBuffer.takeBatch conf.batchSize requests ^=> (fun requests -> writeRequests conf state requests >>= fun res -> loop state)
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
