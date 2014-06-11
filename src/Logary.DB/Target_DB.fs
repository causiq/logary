module Logary.Target.DB

open System
open System.Data
open System.Net

open FSharp.Actor

open Logary
open Logary.Targets
open Logary.Internals

open FsSql

open NodaTime

/// Convert the MetricType to an int
let typeAsInt16 = function
  | MetricType.Counter -> 0s
  | MetricType.Timer _ -> 1s
  | MetricType.Gauge -> 2s

type DBConf =
  { connFac : unit -> IDbConnection
    schema  : string }
  static member Create openConn =
    { connFac = openConn
      schema  = "Logary" }

type internal DBInternalState =
  { conn    : IDbConnection
    connMgr : Sql.ConnectionManager }

let private P = Sql.Parameter.make
let private txn = Tx.transactionalWithIsolation IsolationLevel.ReadCommitted

let private insertMetric schema (m : Measure) connMgr =
  Sql.execNonQuery connMgr
    "INSERT INTO Metrics (Host, Path, EpochTicks, Level, Type, Value)
     VALUES (@host, @path, @epoch, @level, @type, @value)"
    [ P("@host", Dns.GetHostName())
      P("@path", m.path)
      P("@epoch", m.timestamp.Ticks)
      P("@level", m.level.ToInt())
      P("@type", typeAsInt16 m.mtype)
      P("@value", m.value) ]

let private insertMetric' schema m = insertMetric schema m |> txn

let private insertLogLine schema (l : LogLine) connMgr =
  Sql.execNonQuery connMgr
    "INSERT INTO LogLines (Host, Message, Data, Path, EpochTicks, Level, Exception, Tags)
     VALUES (@host, @message, @data, @path, @epoch, @level, @exception, @tags)"
    [ P("@host", Dns.GetHostName())
      P("@message", l.message)
      P("@data", l.data)
      P("@path", l.path)
      P("@epoch", l.timestamp.Ticks)
      P("@level", l.level.ToInt())
      P("@exception", l.``exception`` |> Option.map (sprintf "%O") |> Option.getOrDefault)
      P("@tags", String.Join(",", l.tags)) ]

let private insertLogLine' schema l = insertLogLine schema l |> txn

let private requestTraceLoop (conf : DBConf) (svc : ServiceMetadata) =
  (fun (inbox : IActor<_>) ->
    let rec init () = async {
      InternalLogger.debug "target is opening connection to DB"
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

      | Metric ms ->
        let _ = insertMetric' conf.schema ms state.connMgr
        return! running state

      | Flush chan ->
        chan.Reply Ack
        return! running state

      | ShutdownTarget ackChan ->
        try state.conn.Dispose() with _ -> ()
        ackChan.Reply Ack
        return () }

    init ())

/// Create a new SqlServer target configuration.
let create conf = TargetUtils.stdNamedTarget (requestTraceLoop conf)

/// Create a new SqlServer target configuration.
[<CompiledName("Create")>]
let CreateC(conf, name)  = create conf name

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  member x.ConnectionFactory(conn : unit -> IDbConnection) =
    ! (callParent <| Builder({ conf with connFac = conn }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    let conf = DBConf.Create (fun () -> failwith "inner build error")
    Builder(conf, callParent)

  interface Logary.Targets.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name