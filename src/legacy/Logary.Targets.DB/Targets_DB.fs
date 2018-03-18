module Logary.Targets.DB

open System
open System.Data
open System.Net
open Hopac
open Hopac.Infixes
open Logary.Internals
open Logary.Configuration
open Logary.Configuration.Target
open Logary.Message
open Logary
open FsSql.Logging.LogLine

let jsonFormat (data: obj): string = 
  Logary.Formatting.Json.format data

type DBConf =
  { connectionFactory : unit -> IDbConnection
    schema            : string option }

  static member create openConn =
    { connectionFactory = openConn
      schema            = None }

module internal Impl =

  type DBInternalState =
    { connection : IDbConnection
      connMgr    : Sql.ConnectionManager }

    interface IDisposable with
      member x.Dispose () =
        x.connection.Dispose()

  let P = Sql.Parameter.make
  let txn = Tx.transactional

  let printSchema = function
    | Some (s: string) ->
      Printf.sprintf "%s." s

    | None ->
      String.Empty

  let sharedParameters (m: Message) =
    [ P("@host", Dns.GetHostName())
      P("@name", m.name)
      P("@fields", m |> Message.getAllFields |> jsonFormat)
      P("@context", m.context  |> jsonFormat)
      P("@epoch", m.timestamp)
      P("@level", m.level.toInt ()) ]

  let insertGauge schema (m: Message) (v: float) connMgr =
    Sql.execNonQuery connMgr
      (Printf.sprintf "INSERT INTO %sGauges (Value, Host, Name, Fields, Context, EpochNanos, Level)
       VALUES (@value, @host, @name, @fields, @context, @epoch, @level)" (printSchema schema))
      [ yield P("@value", v)
        yield! sharedParameters m ]

  let insertEvent schema (m: Message) (template: string) connMgr =
    Sql.execNonQuery connMgr
      (Printf.sprintf "INSERT INTO %sEvents (Host, Message, Data, Path, EpochTicks, Level, Exception, Tags)
       VALUES (@host, @message, @data, @path, @epoch, @level, @exception, @tags)" (printSchema schema))
      [ yield P("@template", template)
        yield! sharedParameters m ]

  let insertMessage schema message connMgr =
    let template = message.value
    let tplCount = insertEvent schema message template connMgr 
    let gaugesCount = 
      message 
      |> Message.getAllGauges
      |> Seq.sumBy (fun (gaugeType, Gauge(value,units)) -> insertGauge schema message value connMgr)
    tplCount + gaugesCount

  let ensureOpen (c: IDbConnection) =
    match c.State with
    | ConnectionState.Broken
    | ConnectionState.Closed ->
      c.Open()
      c

    | _ ->
      c

  let execInsert (logger: Logger) (insert: Sql.ConnectionManager -> _) connMgr =
    match insert connMgr with
    | Tx.Commit _ -> ()
    | Tx.Rollback _ -> logger.error (eventX "Insert was rolled back")
    | Tx.Failed ex -> logger.error (eventX "Insert failed" >> addExn ex)

  let loop (conf: DBConf)
           (svc: RuntimeInfo, api : TargetAPI)  =

    let logger =
      let pn = PointName [| "Logary"; "DB"; "loop" |]
      svc.logger |> Logger.apply (setName pn)

    let rec init () =
      logger.debugWithBP (eventX "DB target is opening connection.")
      |> Job.bind (fun _ ->
        let c = ensureOpen (conf.connectionFactory ())
        running { connection = c
                  connMgr    = Sql.withConnection c })

    and running state : Job<_> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          job {
            do! Job.Scheduler.isolate <| fun _ ->
              try
                (state :> IDisposable).Dispose ()
              with e ->
                Message.eventError "DB target disposing connection"
                |> Message.addExn e
                |> Logger.logSimple svc.logger
            do! ack *<= ()
          }

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            let insert =
              insertMessage conf.schema message |> txn
            Job.Scheduler.isolate (fun _ -> execInsert logger insert state.connMgr)
            >>=. running state

          | Flush (ackCh, nack) ->
            job {
              do! IVar.fill ackCh () 
              return! running state
            }

      ] :> Job<_>

    init ()

/// Create a new SqlServer target configuration.
let create conf = TargetConf.createSimple (Impl.loop conf)

/// C# interop: Create a new SqlServer target configuration.
[<CompiledName("Create")>]
let createSimple (conf, name) = create conf name

/// In this step you have finished configuring all required things for the
/// DB and can call Done() to complete the configuration.
type ThirdStep =
  inherit ConfigReader<DBConf>
  abstract Done : DBConf -> TargetConfBuild<Builder>

/// In this step you need to decide whether you want to use a schema for your
/// metrics and/or log lines tables.
and SecondStep =
  /// Don't use an explicit schema. This is the default for SQLite. For SQL
  /// Server the default is 'dbo'.
  abstract DefaultSchema : unit -> ThirdStep

  /// Use an explicit schema. You may not pass a null or empty string.
  abstract Schema : string -> ThirdStep

/// Use with LogaryFactory.New( s => s.Target<DB.Builder>() )
and Builder(conf, callParent : ParentCallback<Builder>) =

  new(callParent: ParentCallback<_>) =
    let conf = DBConf.create (fun () -> failwith "inner build error")
    Builder(conf, callParent)

  /// configure how to create connections to the database.
  member x.ConnectionFactory(conn: Func<IDbConnection>) =
    Builder({ conf with connectionFactory = fun () -> conn.Invoke() }, callParent)
    :> SecondStep

  interface SecondStep with
    member x.DefaultSchema () =
      Builder({ conf with schema = None }, callParent)
      :> ThirdStep

    member x.Schema schema =
      if String.IsNullOrWhiteSpace schema then
        invalidArg "schema" "You must specify a schema, or call DefaultSchema()"
      Builder({ conf with schema = Some schema }, callParent)
      :> ThirdStep

  interface ThirdStep with
    member x.Done (newConf: DBConf) =
      ! (callParent x)

  // this builder is extended with Migrations to optionally ensure the schemas
  // are their latest versions at start
  interface ConfigReader<DBConf> with
    member x.ReadConf () = conf

  interface SpecificTargetConf with
    member x.Build name = create conf name
