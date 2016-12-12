/// This module contains targets and configuration for logging to Google's Stackdriver logging and analytics package.
/// Documentation for Stackdriver Logging itself can be found here: https://cloud.google.com/logging/docs/ 
module Logary.Targets.Stackdriver

open Google.Api
open Google.Api.Gax.Grpc
open Google.Cloud.Logging.Type
open Google.Cloud.Logging.V2
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Target
open Logary.Utils.Chiron
open Logary.Utils.Chiron.Builder
open System.Collections.Generic
open System.Runtime.CompilerServices

[<assembly:InternalsVisibleTo("Logary.Targets.Stackdriver.Tests")>]
do()

/// A structure that enforces certain labeling invariants around monitored resources
/// TODO: add more resource types.
/// SEE: https://cloud.google.com/logging/docs/api/v2/resource-list#service-names
/// SEE: https://cloud.google.com/logging/docs/migration-v2#monitored-resources
type ResourceType = 
  /// Compute resources have a zone and an instance id
  | ComputeInstance of zone: string * instance : string
  /// Containers have cluster/pod/namespace/instance data
  | Container of clusterName : string * namespaceId : string * instanceId : string * podId : string * containerName : string * zone : string
  /// AppEngine services are isolated units segregated by application module id and the version of the application
  | AppEngine of moduleId : string * versionId : string
  static member createComputeInstance(zone, instance) = ComputeInstance(zone, instance)
  static member createContainer(cluster, ns, instance, pod, container, zone) = Container(cluster, ns, instance, pod, container, zone)
  static member createAppEngine(moduleId, version) = AppEngine(moduleId, version)

type StackdriverConf = 
  { /// Google Cloud Project Id
    projectId : string
    /// Name of the log to which to write
    logId : string
    /// Resource currently being monitored
    resource : ResourceType
    /// Additional user labels to be added to the messages
    labels : IDictionary<string,string>
    /// Maximum messages to wait for before sending
    maxBatchSize : uint32 }
  static member create (projectId, logId, resource, ?labels, ?batchSize) = 
    { projectId = projectId
      logId = logId
      resource = resource
      labels = defaultArg labels (Dictionary<_,_>() :> IDictionary<_,_>)
      maxBatchSize = defaultArg batchSize 10u }

let empty : StackdriverConf = StackdriverConf.create("", "", Unchecked.defaultof<ResourceType>) 

module internal Impl =
  
  let toSeverity = function
  | LogLevel.Debug -> LogSeverity.Debug
  | LogLevel.Error -> LogSeverity.Error
  | LogLevel.Fatal -> LogSeverity.Critical
  | LogLevel.Info -> LogSeverity.Info
  | LogLevel.Warn -> LogSeverity.Warning
  | LogLevel.Verbose -> LogSeverity.Debug

  let write (m : Message) : LogEntry =
    let values = 
      match Json.serialize m with
      | Object values -> values
      | otherwise -> failwithf "Expected Message to format to Object .., but was %A" otherwise
    
    let overrides = Map.add "name" (String <| string m.name) values
    let json = Google.Protobuf.WellKnownTypes.Struct.Parser.ParseJson (Json.format <| Object overrides)
    LogEntry(Severity = toSeverity m.level, JsonPayload = json)

  type State = { logName : string
                 logger : LoggingServiceV2Client
                 resource : MonitoredResource
                 cancellation : System.Threading.CancellationTokenSource
                 /// used as a wrapper around the above cancellation token
                 callSettings : CallSettings }

  let createLabels project = function
    // check out https://cloud.google.com/logging/docs/api/v2/resource-list for the list of resources
    // as well as required keys for each one
    | ComputeInstance(zone, instance) -> 
        [ "project_id", project
          "zone", zone
          "instanceId", instance ]|> dict
    | Container(cluster, ns, instance, pod, container, zone) -> 
        [ "project_id", project
          "cluster_name", cluster
          "namespace_id", ns
          "instance_id", instance
          "pod_id", pod
          "container_name", container
          "zone", zone ] |> dict
    | AppEngine(moduleId, versionId) -> 
        ["project_id", project
         "module_id", moduleId
         "version_id", versionId ] |> dict
  
  let resourceName = function
  | ComputeInstance _ -> 
    "gce_instance"
  | Container _ -> 
    "container"
  | AppEngine _ -> 
    "gae_app"
  
  let createMonitoredResource project resourceType =
    let r = MonitoredResource(Type = resourceName resourceType)
    r.Labels.Add(createLabels project resourceType)
    r

  let writeRequests (conf : StackdriverConf) (state : State) (reqs : TargetMessage[]) =
    let request = WriteLogEntriesRequest()
    request.LogName <- state.logName
    request.Labels.Add(conf.labels)
    request.Resource <- state.resource
    
    let acks = ResizeArray<_>() // updated by the loop
    for m in reqs do
      match m with
      | Log (message, ack) ->
        // Write and save the ack for later.
        acks.Add ack
        request.Entries.Add(write message)
      | Flush (ackCh, nack) -> ()
    
    job {
      let! response = state.logger.WriteLogEntriesAsync(request, state.callSettings)
      // execute all the acks without waiting for responses
      return! Job.conIgnore acks
    } 
  
  let createState (conf : StackdriverConf) =
    let source = new System.Threading.CancellationTokenSource()
    { logger = LoggingServiceV2Client.Create()
      resource = createMonitoredResource conf.projectId conf.resource
      logName = sprintf "projects/%s/logs/%s" conf.projectId (System.Net.WebUtility.UrlEncode conf.logId)
      cancellation = source
      callSettings = CallSettings.FromCancellationToken(source.Token) }

  let loop (conf : StackdriverConf)
           (runtime : RuntimeInfo) // this one,
           (requests : RingBuffer<_>) // this one, and,
           (shutdown : Ch<_>) = // this one should always be taken in this order

    let rec loop (state : State) : Job<unit> =
      Alt.choose [
        // either shutdown, or
        shutdown ^=> fun ack -> 
          state.cancellation.Cancel()
          ack *<= () :> Job<_>
        // there's a batch of messages to handle
        RingBuffer.takeBatch conf.maxBatchSize requests ^=> (fun requests -> writeRequests conf state requests >>= fun res -> loop state)
      ] :> Job<_>
    
    let state = createState conf
    loop state

/// Create a new StackDriver target
[<CompiledName "Create">]
let create conf name = TargetUtils.stdNamedTarget (Impl.loop conf) name

type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  /// Assign the projectId for this logger
  member x.ForProject(project) = 
    !(callParent <| Builder({ conf with projectId = project }, callParent))
  
  /// Name the feed that this logger will log to
  member x.Named(logName) = 
    !(callParent <| Builder({ conf with logId = logName }, callParent))
  
  /// Assign a Monitored Resource to this logger
  member x.ForResource(resource) =
    !(callParent <| Builder({ conf with resource = resource }, callParent))
  
  member x.WithLabel(key, value) = 
    conf.labels.Add(key, value)
    !(callParent <| Builder(conf, callParent))
  
  member x.WithLabels(labels : IDictionary<_,_>) = 
    for kvp in labels do
      conf.labels.Add(kvp.Key, kvp.Value)
    !(callParent <| Builder(conf, callParent))
  
  member x.BatchesOf(size) = 
    !(callParent <| Builder({ conf with maxBatchSize = size }, callParent))

  // c'tor, always include this one in your code
  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
