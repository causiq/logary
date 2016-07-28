module Logary.Targets.DB

open System
open System.Data
open System.Net
open Hopac
open Hopac.Infixes
open FsSql
open Logary.Internals
open Logary.Target
open Logary.Utils.Chiron
open Logary

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
    | Some (s : string) ->
      sprintf "%s." s

    | None ->
      String.Empty

  let remapFields fields =
    fields
    |> Seq.map (function KeyValue(key, value) -> PointName.format key, value)
    |> Map.ofSeq

  let sharedParameters (m : Message) =
    [ P("@host", Dns.GetHostName())
      P("@name", m.name)
      P("@fields", remapFields m.fields |> Json.serialize |> Json.format)
      P("@context", m.context |> Json.serialize |> Json.format)
      P("@epoch", m.timestamp)
      P("@level", m.level.toInt ()) ]

  let insertGauge schema (m : Message) (v : Value) connMgr =
    Sql.execNonQuery connMgr
      (sprintf "INSERT INTO %sGauges (Value, Host, Name, Fields, Context, EpochNanos, Level)
       VALUES (@value, @host, @name, @fields, @context, @epoch, @level)" (printSchema schema))
      [ yield P("@value", Value.toDouble v)
        yield! sharedParameters m ]

  let insertGaugeTx schema m v =
    insertGauge schema m v |> txn

  let insertEvent schema (m : Message) (template : string) connMgr =
    Sql.execNonQuery connMgr
      (sprintf "INSERT INTO %sEvents (Host, Message, Data, Path, EpochTicks, Level, Exception, Tags)
       VALUES (@host, @message, @data, @path, @epoch, @level, @exception, @tags)" (printSchema schema))
      [ yield P("@template", template)
        yield! sharedParameters m ]

  let insertEventTx schema message template =
    insertEvent schema message template |> txn

  let ensureOpen (c : IDbConnection) =
    match c.State with
    | ConnectionState.Broken
    | ConnectionState.Closed ->
      c.Open()
      c

    | _ ->
      c

  let execInsert log (insert : Sql.ConnectionManager -> _) connMgr =
    match insert connMgr with
    | Tx.Commit _ ->
      ()

    | Tx.Rollback _ ->
      Message.event Error "Insert was rolled back"
      |> log
      |> start

    | Tx.Failed ex ->
      Message.event Error "Insert failed"
      |> log
      |> start

  let loop (conf : DBConf)
           (svc: RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =

    let log =
      Message.setName (PointName [| "Logary"; "DB"; "loop" |])
      >> Logger.log svc.logger

    let rec init () =
      log (Message.event Debug "DB target is opening connection")
      |> Job.bind (fun _ ->
        let c = ensureOpen (conf.connectionFactory ())
        running { connection = c
                  connMgr    = Sql.withConnection c })

    and running state : Job<_> =
      Alt.choose [
        shutdown ^=> fun ack ->
          job {
            do! Job.Scheduler.isolate <| fun _ ->
              Try.safe "DB target disposing connection"
                       svc.logger
                       (state :> IDisposable).Dispose
                       ()
            do! ack *<= ()
          }

        RingBuffer.take requests ^=> function
          | Log (message, ack) ->
            let insert =
              match message.value with
              | Event template ->
                insertEventTx conf.schema message template

              | Gauge (value, units) ->
                insertGaugeTx conf.schema message value

              | Derived (value, units) ->
                insertGaugeTx conf.schema message value

            Job.Scheduler.isolate (fun _ -> execInsert log insert state.connMgr)
            >>=. running state

          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! running state
            }

      ] :> Job<_>

    init ()

/// Create a new SqlServer target configuration.
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# interop: Create a new SqlServer target configuration.
[<CompiledName("Create")>]
let createSimple (conf, name) = create conf name

/// In this step you have finished configuring all required things for the
/// DB and can call Done() to complete the configuration.
type ThirdStep =
  inherit FactoryApi.ConfigReader<DBConf>
  abstract Done : DBConf -> FactoryApi.TargetConfBuild<Builder>

/// In this step you need to decide whether you want to use a schema for your
/// metrics and/or log lines tables.
and SecondStep =
  /// Don't use an explicit schema. This is the default for SQLite. For SQL
  /// Server the default is 'dbo'.
  abstract DefaultSchema : unit -> ThirdStep

  /// Use an explicit schema. You may not pass a null or empty string.
  abstract Schema : string -> ThirdStep

/// Use with LogaryFactory.New( s => s.Target<Logary.Target.DB.Builder>() )
and Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  new(callParent : FactoryApi.ParentCallback<_>) =
    let conf = DBConf.create (fun () -> failwith "inner build error")
    Builder(conf, callParent)

  /// configure how to create connections to the database.
  member x.ConnectionFactory(conn : Func<IDbConnection>) =
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
    member x.Done (newConf : DBConf) =
      ! (callParent x)

  // this builder is extended with Migrations to optionally ensure the schemas
  // are their latest versions at start
  interface FactoryApi.ConfigReader<DBConf> with
    member x.ReadConf () = conf

  interface FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
