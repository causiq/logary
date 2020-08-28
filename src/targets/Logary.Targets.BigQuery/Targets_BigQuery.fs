module Logary.Targets.BigQuery

open System.Threading.Tasks
open System.Runtime.CompilerServices
open Google.Api.Gax
open Google.Cloud.BigQuery.V2
open Hopac
open Hopac.Infixes
open FSharp.Control.Tasks
open NodaTime
open Logary
open Logary.Model
open Logary.Internals
open Logary.Configuration.Target
open Logary.Targets

[<assembly:InternalsVisibleTo("Logary.Targets.BigQuery.Tests")>]
do()

// NOTE: this target is in ALPHA state (and it's fairly simple)
type BigQueryConf =
  { projectId: string option
    dataset: string
    table: string
    writer: MessageWriter
  }

  static member create (?projectId: string, ?dataset: string, ?table: string) =
    { projectId = projectId
      dataset = defaultArg dataset "logs"
      table = defaultArg table "all"
      writer = SimpleMessageWriter() :> MessageWriter
    }

let empty = BigQueryConf.create()

module internal Impl =
  open System
  open System.IO
  open Google.Apis.Bigquery.v2.Data
  open Logary.Internals.Chiron
  module E = Json.Encode

  let tableSchema =
    let builder = TableSchemaBuilder()
    let field name typ mode = new TableFieldSchema(Name=name, Type=typ, Mode=mode)
    let req name typ = field name typ "REQUIRED"
    let opt name typ = field name typ "NULLABLE"
    builder.Add(req "level" "INT64")
    builder.Add(opt "event" "STRING")
    builder.Add(opt "monetaryValue.amount" "NUMERIC")
    builder.Add(opt "monetaryValue.currency" "STRING")
    builder.Add(req "name" "STRING")
    builder.Add(req "timestamp" "TIMESTAMP")
    /// JSON string
    builder.Add(opt "json" "STRING")
    builder.Build()

  let maybeUpdate (projectId: string) (dataset: BigQueryDataset) (subject: BigQueryTable) (tableSchema: TableSchema) =
    let mutable shouldUpdate = false
    // we need recursive structural equality on the schema, and the C# lib doesn't seem to have that out of the box
    Job.unit ()

  type LogaryMessage with
    member x.toRow() =
      let row = new BigQueryInsertRow()
      row.["level"] <- x.level.asInt
      row.["name"] <- PointName.format x.name
      /// https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#timestamp-type
      row.["timestamp"] <- Instant.ofEpoch(x.timestamp).ToString()
      row.["json"] <- Json.serializeObjectWith E.logaryMessage JsonFormattingOptions.Compact x
      match x.kind with
      | MessageKind.Event ->
        let me = x.getAsOrThrow<EventMessage>()
        row.["event"] <- me.event
        if me.monetaryValue.IsSome then
          row.["monetaryValue.amount"] <- me.monetaryValue.Value.value.asFloat
          row.["monetaryValue.currency"] <- me.monetaryValue.Value.unit.name
        else
          row.["monetaryValue.amount"] <- 0.
          row.["monetaryValue.currency"] <- "EUR"
      | _ -> ()
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
        do! maybeUpdate projectId dataset table tableSchema
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
            do logger.verbose ("Writing {count} messages.", fun m -> m.setField("count", entries.Length))
            let! res = Job.fromTask (fun () ->state.table.InsertRowsAsync(entries))
            do ignore res
            do logger.verbose "Acking messages."
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
