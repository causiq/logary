/// This module contains targets and configuration for logging to Google's Stackdriver logging and analytics package.
/// Documentation for Stackdriver Logging itself can be found here: https://cloud.google.com/logging/docs/
module Logary.Targets.Stackdriver

#nowarn "44"

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
open Logary.Configuration
open Logary.Internals
open Logary.Internals.TypeShape.Core
open Logary.Message
open Logary.Target
open Logary.Formatting
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.CompilerServices

[<assembly:InternalsVisibleTo("Logary.Targets.Stackdriver.Tests")>]
do()

/// A structure that enforces certain labeling invariants around monitored resources
/// TO CONSIDER: add more resource types.
/// SEE: https://cloud.google.com/logging/docs/api/v2/resource-list#service-names
/// SEE: https://cloud.google.com/logging/docs/migration-v2#monitored-resources
type ResourceType =
  /// An API provided by the producer.
  /// `service` (1): The API service name, such as "cloudsql.googleapis.com".
  /// `method` (2): The API method, such as "disks.list".
  /// `version`: The API version, such as "v1".
  /// `location`: The service specific notion of location. This can be the name of a zone, region, or "global".
  | Api of service:string * method:string * version:string * location:string
  /// A virtual machine instance hosted in Google Compute Engine (GCE).
  /// `instance_id` (1): The numeric VM instance identifier assigned by Compute Engine.
  /// `zone`: The Compute Engine zone in which the VM is running.
  | GCEInstance of zone:string * instance:string
  /// A Google Container Engine (GKE) container instance.
  /// `cluster_name` (1): An immutable name for the cluster the container is running in.
  /// `namespace_id` (2): Immutable ID of the cluster namespace the container is running in.
  /// `instance_id`: Immutable ID of the GCE instance the container is running in.
  /// `pod_id`: Immutable ID of the pod the container is running in.
  /// `container_name`: Immutable name of the container.
  /// `zone`: The GCE zone in which the instance is running.
  | Container of clusterName:string
               * namespaceId:string
               * instanceId:string
               * podId:string
               * containerName:string
               * zone:string
  /// A resource type used to indicate that a log is not associated with any specific resource.
  | Global
  /// AppEngine services are isolated units segregated by application module id
  /// and the version of the application
  | AppEngine of moduleId:string * versionId: string

  member x.key =
    match x with
    | Api _ ->
      "api"
    | GCEInstance _ ->
      "gce_instance"
    | Container _ ->
      "container"
    | Global ->
      "global"
    | AppEngine _ ->
      "gae_app"

  /// See https://cloud.google.com/logging/docs/api/v2/resource-list for the list of resources
  /// as well as required keys for each one
  member x.labels projectId =
    dict <|
    ("project_id", projectId) ::
    match x with
    | Api (service, method, version, location) ->
      [ "service", service
        "method", method
        "version", version
        "location", location ]

    | GCEInstance (zone, instance) ->
      [ "zone", zone
        "instance_id", instance ]

    | Container (cluster, ns, instance, pod, container, zone) ->
      [ "cluster_name", cluster
        "namespace_id", ns
        "instance_id", instance
        "pod_id", pod
        "container_name", container
        "zone", zone ]

    | Global ->
      []

    | AppEngine (moduleId, versionId) ->
      [ "module_id", moduleId
        "version_id", versionId ]

  member x.toMonitoredResource project: MonitoredResource =
    let r = MonitoredResource(Type = x.key)
    r.Labels.Add(x.labels project)
    r

  static member createGCEInstance(zone, instance) =
    GCEInstance(zone, instance)

  static member createComputeInstance (zone, instance) =
    GCEInstance (zone, instance)

  static member createContainer(cluster, ns, instance, pod, container, zone) =
    Container(cluster, ns, instance, pod, container, zone)

  static member createAppEngine(moduleId, version) =
    AppEngine(moduleId, version)

type StackdriverConf =
  { /// Google Cloud Project Id
    projectId: string
    /// Name of the log to which to write
    logId: string
    /// Resource currently being monitored
    resource: ResourceType
    /// Additional user labels to be added to the messages
    labels: HashMap<string,string>
    /// Maximum messages to send as a batch.
    maxBatchSize: uint16 }

  static member create (projectId, ?logId, ?resource, ?labels, ?batchSize) =
    { projectId = projectId
      logId = defaultArg logId "apps"
      resource = defaultArg resource Global
      labels = defaultArg labels HashMap.empty
      maxBatchSize = defaultArg batchSize 50us }

let empty: StackdriverConf =
  StackdriverConf.create "CHANGE_PROJECT_ID"

module internal Impl =
  // constant computed once here
  let messageKey = "message"
  let valueKey = "value"

  type LogLevel with
    member x.toSeverity(): LogSeverity =
      match x with
      | LogLevel.Debug -> LogSeverity.Debug
      | LogLevel.Error -> LogSeverity.Error
      | LogLevel.Fatal -> LogSeverity.Critical
      | LogLevel.Info -> LogSeverity.Info
      | LogLevel.Warn -> LogSeverity.Warning
      | LogLevel.Verbose -> LogSeverity.Debug

  type V = WellKnownTypes.Value

  // consider use destr whole msg context as TemplatePropertyValue, then send to WellKnownTypes
  let rec x2V (t: Type): 'T -> V =
    let wrap (k: 'a -> V) = fun inp -> k (unbox inp)

    let inline mkFieldPrinter (shape: IShapeMember<'DeclaringType>) =
      shape.Accept
        { new IMemberVisitor<'DeclaringType, string * ('DeclaringType -> V)> with
            member __.Visit (field: ShapeMember<'DeclaringType, 'Field>) =
              let fieldPrinter = toV<'Field>()
              field.Label, fieldPrinter << field.Project }

    match TypeShape.Create t with
    | Shape.Unit ->
      wrap (fun () -> V.ForString "()") // 'T = unit
    | Shape.String ->
      wrap (fun (s: string) -> V.ForString s)
    | Shape.Bool ->
      wrap (fun (b: bool) -> V.ForBool b)
    | Shape.Double ->
      wrap (fun f -> V.ForNumber f)
    | Shape.Int16 ->
      wrap (int16 >> float >> V.ForNumber)
    | Shape.Int32 ->
      wrap (int32 >> float >> V.ForNumber)
    | Shape.Int64 ->
      wrap (int64 >> float >> V.ForNumber)
    | Shape.UInt16 ->
      wrap (uint16 >> float >> V.ForNumber)
    | Shape.UInt32 ->
      wrap (uint32 >> float >> V.ForNumber)
    | Shape.UInt64 ->
      wrap (uint64 >> float >> V.ForNumber)
    | Shape.BigInt ->
      wrap ((fun (x: BigInteger) -> x) >> float >> V.ForNumber)
    | Shape.DateTime ->
      wrap (fun (b:DateTime) -> V.ForString (b.ToString("o")))
    | Shape.DateTimeOffset ->
      wrap (fun (b:DateTimeOffset) -> V.ForString (b.ToString("o")))

    | Shape.Array s when s.Rank = 1 ->
      s.Accept
        { new IArrayVisitor<'T -> V> with
            member __.Visit<'a> _ =
              wrap (fun xs ->
                xs
                |> List.map (fun x -> x2V (t.GetType()) x)
                |> List.toArray
                |> fun args -> V.ForList(args))
        }

    | Shape.FSharpOption s ->
      s.Accept
        { new IFSharpOptionVisitor<'T -> V> with
            member __.Visit<'a> () =
              wrap (function
                | None -> V.ForNull()
                | Some x ->
                  x2V (x.GetType()) x)
        }

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
      let mkUnionCasePrinter (s: ShapeFSharpUnionCase<'T>) =
        let fieldPrinters = s.Fields |> Array.map mkFieldPrinter
        fun (u: 'T) ->
          match fieldPrinters with
          | [||] ->
            V.ForString s.CaseInfo.Name

          | [|_, fp|] ->
            let str = Struct()
            str.Fields.[s.CaseInfo.Name] <- fp u
            V.ForStruct str

          | fps ->
            let values =
              fps
              |> Seq.map (fun (_, fp) -> fp u)
              |> Seq.toArray
              |> fun xs -> V.ForList(xs)

            let str = Struct()
            str.Fields.[s.CaseInfo.Name] <- values
            V.ForStruct str

      let casePrinters = shape.UnionCases |> Array.map mkUnionCasePrinter

      fun (u:'T) ->
        let printer = casePrinters.[shape.GetTag u]
        printer u

    | Shape.FSharpMap s when s.Key = shapeof<string> ->
      s.Accept
        { new IFSharpMapVisitor<'T -> V> with
            member __.Visit<'k, 'v when 'k : comparison> () =
              wrap (fun (m: Map<string, 'T>) ->
                let s = Struct()
                for KeyValue (k, v) in m do
                  let pbV = x2V (v.GetType()) v
                  s.Fields.[k] <- pbV
                V.ForStruct s)
        }

//    | Shape.FSharpSet s ->
//      s.Accept
//        { new IFSharpSetVisitor<'T -> V> with
//            member __.Visit<'a when 'a : comparison> () = // 'T = Set<'a>
//              wrap (fun (xs: Set<'a>) ->
//                xs
//                |> Array.ofSeq
//                |> Array.map (fun x -> x2V (x.GetType()) x)
//                |> fun args -> V.ForList(args))
//       }
    | Shape.Enumerable s ->
      match s.Element with
      | Shape.KeyValuePair ks when ks.Key = shapeof<string> ->
        s.Accept
          { new IEnumerableVisitor<'T -> V> with
              member __.Visit () =
                wrap (fun (xs: seq<_>) ->
                  let s = Struct()
                  xs |> Seq.iter (fun (KeyValue (key, value)) ->
                    s.Fields.[key] <- x2V (value.GetType()) value)
                  V.ForStruct s)
          }

      | _ ->
        s.Accept
          { new IEnumerableVisitor<'T -> V> with
              member __.Visit () =
                wrap (fun (xs: seq<_>) ->
                  let arr = ResizeArray<V>()
                  for x in xs do
                    // I can't make this not warn, but I want warnings in all of the file
                    let pbV = x2V (x.GetType()) (box x)
                    arr.Add pbV
                  V.ForList (arr.ToArray())
                )
          }

    | Shape.KeyValuePair s when s.Key = shapeof<string> ->
      s.Accept
        { new IKeyValuePairVisitor<'T -> V> with
            member x.Visit<'K, 'V> () =
              wrap (fun (kvp: KeyValuePair<'K, 'V>) ->
                let k: string = unbox kvp.Key
                let v = x2V (kvp.Value.GetType()) kvp.Value
                let s = Struct()
                s.Fields.[k] <- v
                V.ForStruct s
              )
        }

    | Shape.Exception s ->
      s.Accept
        { new IExceptionVisitor<'T -> V> with
            member __.Visit() =
              wrap (fun (e: exn) ->
                let s = Struct()

                if not (isNull e.Data) && e.Data.Count > 0 then
                  s.Fields.["data"] <- toV<System.Collections.IDictionary>() e.Data

                if not (isNull e.HelpLink) then
                  s.Fields.["helpLink"] <- toV<string>() e.HelpLink

                if not (isNull e.InnerException) then
                  s.Fields.["inner"] <- toV<exn>() e.InnerException

                if e.HResult <> Unchecked.defaultof<int> then
                  s.Fields.["hresult"] <- toV<int>() e.HResult

                s.Fields.["message"] <- toV<string>() e.Message
                s.Fields.["source"] <- toV<string>() e.Source

                if not (String.IsNullOrWhiteSpace e.StackTrace) then
                  let lines = DotNetStacktrace.parse e.StackTrace
                  s.Fields.["stacktrace"] <- toV<StacktraceLine[]>() lines

                if not (isNull e.TargetSite) then
                  s.Fields.["targetSite"] <- toV<string>() e.TargetSite.Name

                V.ForStruct s
              )
        }

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as s) ->
      let fieldPrinters = s.Fields |> Array.map mkFieldPrinter
      fun (r: 'T) ->
        let s = Struct()
        fieldPrinters |> Seq.iter (fun (label, fp) -> s.Fields.[label] <- fp r)
        V.ForStruct s

    | Shape.Poco (:? ShapePoco<'T> as shape) ->
      let propPrinters = shape.Properties |> Array.map mkFieldPrinter
      fun (r: 'T) ->
        let s = Struct()
        propPrinters |> Seq.iter (fun (label, fp) -> s.Fields.[label] <- fp r)
        V.ForStruct s

    // TODO: BitmapNodeN

    | other ->
      wrap (fun o -> V.ForString (string o))

  and toV<'T> (): 'T -> V =
    fun inp ->
      x2V typeof<'T> (box inp)

  let addToStruct (s: WellKnownTypes.Struct) (k, value: obj): WellKnownTypes.Struct =
    if isNull value then s else
    s.Fields.[k] <- x2V (value.GetType()) value
    s

  type Message with
    member x.toLogEntry () =
      // Labels are used in Stackdriver for classification, so lets put context there.
      // They expect only simple values really in the labels, so it's ok to just stringify them
      let labels =
        Message.getOthers x
        |> Seq.map (fun (k, v) -> k, string v)
        |> dict

      let formatted = Logary.MessageWriter.verbatim.format x

      let addMessageFields (values: seq<string * obj>): seq<_> =
        seq {
          yield messageKey, box formatted
          if formatted <> x.value then
            yield valueKey, box formatted
          yield! values
        }

      let payloadFields =
        Message.getAllFields x
        |> addMessageFields
        |> Seq.fold addToStruct (WellKnownTypes.Struct())

      let entry = LogEntry(Severity = x.level.toSeverity(), JsonPayload = payloadFields)
      entry.Timestamp <- Timestamp.FromDateTimeOffset (DateTimeOffset.ofEpoch x.timestamp)
      entry.Labels.Add labels
      entry

  type State =
    { logName: string
      logger: LoggingServiceV2Client
      resource: MonitoredResource
      labels: IDictionary<string, string>
      cancellation: System.Threading.CancellationTokenSource
      /// used as a wrapper around the above cancellation token
      callSettings: CallSettings }

    static member create (conf: StackdriverConf) =
      let source = new System.Threading.CancellationTokenSource()
      { logger = LoggingServiceV2Client.Create()
        resource = conf.resource.toMonitoredResource conf.projectId
        labels = conf.labels |> HashMap.toDictionary
        logName = sprintf "projects/%s/logs/%s" conf.projectId (System.Net.WebUtility.UrlEncode conf.logId)
        cancellation = source
        callSettings = CallSettings.FromCancellationToken(source.Token) }

  let writeBatch (state: State) (entries: LogEntry list): Job<unit> =
    Job.Scheduler.isolate (fun () ->
      let request = WriteLogEntriesRequest(LogName = state.logName, Resource = state.resource)
      request.Entries.AddRange entries
      request.Labels.Add state.labels
      state.logger.WriteLogEntries(request, state.callSettings)
      |> ignore)

  let loop (conf: StackdriverConf) (runtime: RuntimeInfo, api: TargetAPI) =
    runtime.logger.info (
      eventX "Started Stackdriver target with project {projectId}, writing to {logName}"
      >> setField "projectId" conf.projectId
      >> setField "logName" conf.logId)

    let rec loop (state: State): Job<unit> =
      Alt.choose [
        // either shutdown, or
        api.shutdownCh ^=> fun ack ->
          runtime.logger.verbose (eventX "Shutting down Stackdriver target")
          state.cancellation.Cancel()
          ack *<= () :> Job<_>

        RingBuffer.takeBatch conf.maxBatchSize api.requests ^=> fun messages ->
          let entries, acks, flushes =
            // Make a single pass over the array, accumulating the entries, acks and flushes.
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                message.toLogEntry() :: entries,
                ack *<= () :: acks,
                flushes
              | Flush (ackCh, nack) ->
                entries,
                acks,
                ackCh *<= () :: flushes)
              ([], [], [])

          job {
            do runtime.logger.verbose (eventX "Writing {count} messages" >> setField "count" entries.Length)
            do! writeBatch state entries
            do runtime.logger.verbose (eventX "Acking messages")
            do! Job.conIgnore acks
            do! Job.conIgnore flushes
            return! loop state
          }

      ] :> Job<_>

    loop (State.create conf)

/// Create a new StackDriver target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

type Builder(conf, callParent: Target.ParentCallback<Builder>) =

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
    !(callParent <| Builder({ conf with labels = conf.labels |> HashMap.add key value }, callParent))

  member x.WithLabels(labels: IDictionary<_,_>) =
    let labels' = labels |> Seq.fold (fun hm (KeyValue (key, value)) -> hm |> HashMap.add key value) conf.labels
    !(callParent <| Builder({ conf with labels = labels' }, callParent))

  member x.BatchesOf(size) =
    !(callParent <| Builder({ conf with maxBatchSize = size }, callParent))

  // c'tor, always include this one in your code
  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
