/// This module contains targets and configuration for logging to Google's Stackdriver logging and analytics package.
/// Documentation for Stackdriver Logging itself can be found here: https://cloud.google.com/logging/docs/ 
module Logary.Targets.Stackdriver

open Google.Api
open Google.Api.Gax.Grpc
open Google.Cloud.Logging.Type
open Google.Cloud.Logging.V2
open Google.Protobuf
open Google.Protobuf.Collections
open Google.Protobuf.WellKnownTypes
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
  // constant computed once here
  let messageKey = PointName [|"message"|]

  let toSeverity = function
  | LogLevel.Debug -> LogSeverity.Debug
  | LogLevel.Error -> LogSeverity.Error
  | LogLevel.Fatal -> LogSeverity.Critical
  | LogLevel.Info -> LogSeverity.Info
  | LogLevel.Warn -> LogSeverity.Warning
  | LogLevel.Verbose -> LogSeverity.Debug

  let rec toProtobufValue = function
    | Value.String s -> WellKnownTypes.Value.ForString(s)
    | Value.Bool b -> WellKnownTypes.Value.ForBool(b)
    | Value.Float f -> WellKnownTypes.Value.ForNumber(f)
    | Value.Int64 i -> WellKnownTypes.Value.ForNumber(float i)
    | Value.BigInt b -> WellKnownTypes.Value.ForNumber(float b)
    | Value.Binary (bytes, contentType) -> 
      // could not find a good way to convert these, will just send null value instead
      WellKnownTypes.Value.ForNull()
    | Value.Fraction (numerator,denominator) -> WellKnownTypes.Value.ForNumber(float(numerator/denominator))
    | Value.Object values -> 
      values |> Map.toSeq 
      |> Seq.fold (fun (s : WellKnownTypes.Struct) (k,v) -> s.Fields.[k] <- toProtobufValue v; s) (WellKnownTypes.Struct()) 
      |> WellKnownTypes.Value.ForStruct
    | Value.Array values -> 
      let valueArr = values |> List.map toProtobufValue |> List.toArray
      WellKnownTypes.Value.ForList valueArr

  and addToStruct (s : WellKnownTypes.Struct) (k : PointName, (Field(value,units))) : WellKnownTypes.Struct = 
    s.Fields.[string k] <- toProtobufValue value
    s

  let write (m : Message) : LogEntry =
    // labels are used in stackdriver for classification, so lets put context there.
    // they expect only simple values really in the labels, so it's ok to just stringify them
    let labels = m.context |> Map.map (fun k v -> Value.toString v) |> Map.toSeq |> dict

    let message = 
      // TODO: render the template with field values
      match m.value with
      | Event template -> template
      | _ -> ""

    // really only need to include fields and the formatted message template, because context went to labels
    let payloadFields = 
      m.fields 
      |> Map.add messageKey (Field.init message)
      |> Map.toSeq
      |> Seq.fold addToStruct (WellKnownTypes.Struct())

    let entry = LogEntry(Severity = toSeverity m.level, JsonPayload = payloadFields)
    entry.Labels.Add(labels)
    entry


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
          "instance_id", instance ]|> dict
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
        
        RingBuffer.take requests ^=> function
            | Log (message, ack) -> job {
                    let request = WriteLogEntriesRequest()
                    request.LogName <- state.logName
                    request.Labels.Add(conf.labels)
                    request.Resource <- state.resource
                    request.Entries.Add(write message)
                    do! Job.Scheduler.isolate (fun () -> state.logger.WriteLogEntries(request, state.callSettings) |> ignore)
                    do! ack *<= ()
                    return! loop state
                }
            | Flush (ackCh, nack) -> job {
                    do! Ch.give ackCh () <|> nack
                    return! loop state
                }
                    
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
