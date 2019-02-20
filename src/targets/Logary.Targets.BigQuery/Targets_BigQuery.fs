module Logary.Targets.BigQuery

open System.Runtime.CompilerServices
open Google.Api.Gax
open Google.Cloud.BigQuery.V2
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals
open Logary.Configuration.Target

[<assembly:InternalsVisibleTo("Logary.Targets.BigQuery.Tests")>]
do()


type BigQueryConf =
  { projectId: string option
    dataset: string
    table: string
  }
  
  static member create (?projectId: string, ?dataset: string, ?table: string) =
    { dataset = defaultArg dataset "logs"
      table = defaultArg table "all"
      projectId = projectId
    }

let empty = BigQueryConf.create()

module internal Impl =
  open System
  open System.IO
  open Google.Apis.Bigquery.v2.Data
  open Logary.Formatting
  open Logary.Internals.Chiron
  
  let tableSchema =
    let builder = TableSchemaBuilder()
    let field name typ mode = new TableFieldSchema(Name=name, Type=typ, Mode=mode)
    let req name typ = field name typ "REQUIRED"
    builder.Add(req "level" "INTEGER")
    builder.Add(req "message" "STRING")
    builder.Add(req "value" "STRING")
    builder.Add(req "logger" "STRING")
    builder.Add(req "timestamp" "TIMESTAMP")
    /// JSON string
    builder.Add(req "fields" "STRING")
    builder.Build()
    
  type Message with
    member x.toRow() =
      let row = new BigQueryInsertRow() 
      use sw = new StringWriter()
      MessageWriter.verbatim.write sw x
      row.["level"] <- x.level.toInt()
      row.["message"] <-sw.ToString()
      row.["value"] <- x.value
      row.["logger"] <- PointName.format x.name
      /// https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#timestamp-type
      row.["timestamp"] <- Instant.ofEpoch(x.timestamp).ToString()
      row.["fields"] <-
        getAllFields x
          |> Seq.fold (fun s (k, v) -> s |> JsonObject.add k (Json.encode v)) JsonObject.empty
          |> Object
          |> Json.format
      row
  
  type State =
    { table: BigQueryTable
      client: BigQueryClient }
    
    interface IDisposable with
      member x.Dispose() =
        x.client.Dispose()

  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf: BigQueryConf) (api: TargetAPI) =
    let logger = api.runtime.logger
    
    let rec initialise () =
      job {
        let! platform = Platform.InstanceAsync()
        let projectId = conf.projectId |> Option.defaultValue platform.ProjectId
        let client = BigQueryClient.Create(projectId)
        let dataset = client.GetOrCreateDataset(conf.dataset)
        let table = dataset.GetOrCreateTable(conf.table, tableSchema)
        return! running { table=table; client=client }
      }
      
    and running (state: State): Job<_> =
      Alt.choose [
        RingBuffer.takeBatch 512us api.requests ^=> fun messages ->
          let entries, acks, flushes =
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                message.toRow() :: entries,
                ack *<= () :: acks,
                flushes
              | Flush (ackCh, nack) ->
                entries,
                acks,
                ackCh *<= () :: flushes)
              ([], [], [])
              
          job {
            do logger.verbose (eventX "Writing {count} messages." >> setField "count" entries.Length)
            do! Job.fromUnitTask (fun () ->state.table.InsertRowsAsync(entries))
            do logger.verbose (eventX "Acking messages.")
            do! Job.conIgnore acks
            do! Job.conIgnore flushes
            return! running state
          }

        api.shutdownCh ^=> fun ack ->
          Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          >>=. ack *<= ()
      ] :> Job<_>

    initialise ()

/// Create a new BigQuery target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New(s => s.Target<BigQuery>())
type Builder(conf, callParent: ParentCallback<Builder>) =
  let update (conf' : BigQueryConf): Builder =
    Builder(conf', callParent)
  member x.ProjectId(projectId: string) =
    update { conf with projectId = Some projectId }
  member x.Table(table: string) =
    update { conf with table = table }
  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
