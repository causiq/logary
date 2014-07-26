module Logary.Targets.DB

open System
open System.Data
open System.Net

open FSharp.Actor

open FsSql

open NodaTime


open Logary
open Logary.Internals
open Logary.Target

type DBConf =
  { connFac : unit -> IDbConnection
    schema  : string option }
  static member Create openConn =
    { connFac = openConn
      schema  = None }

module internal Impl =

  type DBInternalState =
    { conn    : IDbConnection
      connMgr : Sql.ConnectionManager }

  let P = Sql.Parameter.make
  let txn = Tx.transactionalWithIsolation IsolationLevel.ReadCommitted

  let printSchema = function
    | Some (s : string) -> sprintf "%s." s
    | None              -> String.Empty

  let insertMeasure schema (m : Measure) connMgr =
    Sql.execNonQuery connMgr
      (sprintf "INSERT INTO %sMetrics (Host, Path, EpochTicks, Level, Type, Value)
       VALUES (@host, @path, @epoch, @level, @type, @value)" (printSchema schema))
      [ P("@host", Dns.GetHostName())
        P("@path", m.m_path)
        P("@epoch", m.m_timestamp.Ticks)
        P("@level", m.m_level.ToInt())
        P("@value", m.m_value) ]

  let insertMeasure' schema m = insertMeasure schema m |> txn

  let insertLogLine schema (l : LogLine) connMgr =
    Sql.execNonQuery connMgr
      (sprintf "INSERT INTO %sLogLines (Host, Message, Data, Path, EpochTicks, Level, Exception, Tags)
       VALUES (@host, @message, @data, @path, @epoch, @level, @exception, @tags)" (printSchema schema))
      [ P("@host", Dns.GetHostName())
        P("@message", l.message)
        P("@data", l.data)
        P("@path", l.path)
        P("@epoch", l.timestamp.Ticks)
        P("@level", l.level.ToInt())
        P("@exception", l.``exception`` |> Option.map (sprintf "%O") |> Option.getOrDefault)
        P("@tags", String.Join(",", l.tags)) ]

  let insertLogLine' schema l = insertLogLine schema l |> txn

  let loop (conf : DBConf) (svc : RuntimeInfo) =
    (fun (inbox : IActor<_>) ->
      let log = LogLine.setPath "Logary.DB.loop" >> Logger.log svc.logger
      let rec init () = async {
        LogLine.debug "target is opening connection to DB" |> log
        let c = conf.connFac ()
        match c.State with
        | ConnectionState.Broken
        | ConnectionState.Closed -> c.Open()
        | _ -> ()
        return! running
          { conn    = c
            connMgr = Sql.withConnection c } }

      and running state = async {
        let! msg, mopt = inbox.Receive()
        match msg with
        | Log l ->
          let _ = insertLogLine' conf.schema l state.connMgr
          return! running state

        | Measure msr ->
          let _ = insertMeasure' conf.schema msr state.connMgr
          return! running state

        | Flush chan ->
          chan.Reply Ack
          return! running state

        | Shutdown ackChan ->
          try state.conn.Dispose() with _ -> ()
          ackChan.Reply Ack
          return () }

      init ())

/// Create a new SqlServer target configuration.
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# interop: Create a new SqlServer target configuration.
[<CompiledName("Create")>]
let create' (conf, name) = create conf name

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
    let conf = DBConf.Create (fun () -> failwith "inner build error")
    Builder(conf, callParent)

  /// configure how to create connections to the database.
  member x.ConnectionFactory(conn : Func<IDbConnection>) =
    Builder({ conf with connFac = fun () -> conn.Invoke() }, callParent)
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