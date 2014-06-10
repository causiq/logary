module Logary.Target.DB

open System
open System.Data

open FSharp.Actor

open Logary
open Logary.Targets

open FsSql

open NodaTime

type DBConf =
  { connMgr : Sql.ConnectionManager }
  static member Create openConn =
    { connMgr = Sql.withNewConnection openConn }

type internal DBInternalState =
  { a : string }

let private P = Sql.Parameter.make
let private txn = Tx.transactionalWithIsolation IsolationLevel.ReadCommitted

let private insertMetric (m : Measure) connMgr =
  Sql.execNonQuery connMgr
    "INSERT INTO Metrics (Path, EpochTicks, Level, Type, Value)
     VALUES (@path, @epoch, @level, @type, @value)"
    [ P("@path", m.path)
      P("@epoch", m.timestamp.Ticks)
      P("@level", m.level.ToInt())
      P("@type", m.mtype.ToString())
      P("@value", m.value) ]

let private insertMetric' m = insertMetric m |> txn

let private insertLogLine (l : LogLine) connMgr =
  Sql.execNonQuery connMgr
    "INSERT INTO LogLine (Message, Data, Path, EpochTicks, Level, Exception, Tags)
     VALUES (@message, @data, @path, @epoch, @level, @exception, @tags)"
    [ P("@message", l.message)
      P("@data", l.data)
      P("@path", l.path)
      P("@epoch", l.timestamp.Ticks)
      P("@level", l.level.ToInt())
      P("@exception", l.``exception``)
      P("@tags", String.Join(",", l.tags)) ]

let private insertLogLine' l = insertLogLine l |> txn

let private requestTraceLoop (conf : DBConf) (svc : ServiceMetadata) =
  (fun (inbox : IActor<_>) ->
    let rec init () = async {
      return! running () }

    and running state = async {
      let! msg, mopt = inbox.Receive()
      match msg with
      | Log l ->
        let _ = insertLogLine' l conf.connMgr
        return! running state

      | Metric ms ->
        let _ = insertMetric' ms conf.connMgr
        return! running state

      | Flush chan ->
        chan.Reply Ack
        return! running state

      | ShutdownTarget ackChan ->
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
    ! (callParent <| Builder({ conf with connMgr = Sql.withNewConnection conn }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    let conf = DBConf.Create (fun () -> failwith "inner build error")
    Builder(conf, callParent)

  interface Logary.Targets.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name