module Logary.Tests.DB

open Expecto
open System
open System.Data
open System.Data.SQLite
open System.Net
open NodaTime
open Hopac
open Logary
open Logary.Configuration
open Logary.Internals
open Logary.Targets

let runtime =
  RuntimeInfo.create "tests" "localhost"

/// this one is a per-process db
[<Literal>]
let private inMemConnStrShared = "FullUri=file::memory:?cache=shared"

/// this one cleans out the database when you close the connection
[<Literal>]
let private inMemConnStrEmpheral = "FullUri=file::memory:"

/// some methods for working with SQLite databases in memory with connection
/// management nicely handled
module SQLiteDB =

  let private consoleAndDebugger =
    let ilogger = InternalLogger.create runtime |> run
    [ Console.create Console.empty "console"
      Debugger.create Debugger.empty "debugger" ]
    |> List.map (fun conf -> InternalLogger.add conf ilogger)
    |> Job.conIgnore
    |> run
    ilogger :> Logger

  open System
  open Logary.DB.Migrations

  open FluentMigrator
  open FluentMigrator.Runner
  open FluentMigrator.Runner.Processors
  open FluentMigrator.Runner.Processors.SQLite
  open FluentMigrator.Runner.Generators.SQLite

  /// wrap the connection in a non-closing delegator for connections that are
  /// managed externally
  let wrapConnNoClose (inner: IDbConnection) =
    { new IDbConnection with
        member x.BeginTransaction() =
          inner.BeginTransaction()
        member x.BeginTransaction isolationLevel =
          inner.BeginTransaction isolationLevel
        member x.ChangeDatabase newDb =
          inner.ChangeDatabase newDb
        member x.ConnectionString
          with get() = inner.ConnectionString
          and set v = inner.ConnectionString <- v
        member x.CreateCommand () =
          inner.CreateCommand ()
        member x.ConnectionTimeout = inner.ConnectionTimeout
        member x.Close() = ()
        member x.Dispose() = ()
        member x.Database
          with get() = inner.Database
        member x.Open () =
          inner.Open()
        member x.State
          with get() = inner.State }

  /// used to avoid closing the SQLite connection in between migrations
  type NonClosingSQLiteProcessorFactory(conn: IDbConnection) =
    inherit MigrationProcessorFactory()
    override x.Create (connStr: string, accouncer : IAnnouncer, opts : IMigrationProcessorOptions) =
      new SQLiteProcessor(wrapConnNoClose conn,
                          new SQLiteGenerator(), accouncer, opts,
                          new SQLiteDbFactory())
      :> IMigrationProcessor

  let private openConnInner connStr () =
    let conn = new SQLiteConnection(connStr: string) :> IDbConnection
    conn.Open()
    conn

  /// open and migrate with the given connection string
  let openConn connStr =
    Message.event Info "Open Connection" |> Logger.logSimple consoleAndDebugger
    let conn = openConnInner connStr ()
    Runner(NonClosingSQLiteProcessorFactory(conn), connStr, logger = consoleAndDebugger).MigrateUp()
    conn

  let connMgrShared = Sql.withNewConnection (fun () -> openConn inMemConnStrShared)

let raised_exn msg =
  let e = ref None : exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

let read (m: Map<string, obj>) k =
  try m.[k] :?> 'a
  with :? InvalidCastException as e ->
    Tests.failtestf "converting key %s to %s failed, was: %s."
      k typeof<'a>.Name (m.[k].GetType().Name)

[<Tests>]
let targetTests =
  let flush = Target.flush >> Job.Ignore >> run

  let stop = Target.shutdown >> Job.Ignore >> run

  let start connFac =
    let conf = DB.DBConf.create connFac
    Target.create runtime (DB.create conf "db-target")
    |> run

  testList "db target" [
    testCase "smoke" <| fun _ ->
      use c = new SQLiteConnection(inMemConnStrEmpheral) :> IDbConnection
      c.Open()
      c.Close()

    testCase "initialise" <| fun _ ->
      stop (start (fun () -> SQLiteDB.openConn inMemConnStrEmpheral))

    testCase "initialise and log" <| fun _ ->
      let target = start (fun () -> SQLiteDB.openConn inMemConnStrEmpheral)
      try
        Message.event Info "hello world" |> Target.log target |> Job.Ignore |> run
        flush target
      finally stop target

    testCase "log and read back returns result" <| fun _ ->
      use db = SQLiteDB.openConn inMemConnStrShared
      let mgr = Sql.withConnection db

      let countSql = "SELECT COUNT(*) FROM Events"

      // pre-conditions
      let count = Sql.execScalar mgr countSql [] |> Option.get
      Expect.equal count 0L "count should be zero"

      // given
      let target = start (fun () -> db)
      Message.event Info "hello world"
      |> Message.setName (PointName.parse "a.b.c")
      |> Target.log target
      |> Job.Ignore
      |> run
      Message.event Info "goodbye world"
      |> Message.setName (PointName.parse "a.b.c")
      |> Message.addExn (raised_exn "hoho")
      |> Target.log target
      |> Job.Ignore
      |> run

      flush target

      // then
      try
        let count = Sql.execScalar mgr countSql [] |> Option.get
        Expect.equal count 2L "Count should be two log lines"

        let records = Sql.execReader mgr "SELECT * FROM Events" [] |> Sql.map Sql.asMap

        for r in records do
          let read k : 'a = read r k
          Expect.equal (read "Host") (Dns.GetHostName()) "should have host name from computer DNS"
          Expect.equal (read "Path") "a.b.c" "Should have path"
          Expect.equal (read "Level") (int64 (Info.toInt ()))  "Should have Info level"

          if read "Message" = "goodbye world" then
            Expect.equal (read "Tags") "tests,things"
                         "should have comma-separated tags"
            Expect.stringContains (read "Exception") "ApplicationException"
                                  "should have exception substring"
            Expect.stringContains (read "Exception") "hoho"
                                  "should have exception substring"
      finally
        stop target

    testCase "initialise and metric" <| fun _ ->
      let target = start (fun () -> SQLiteDB.openConn inMemConnStrEmpheral)
      try Message.gauge "app.signin" 3. |> Target.log target |> Job.Ignore |> run
      finally stop target

    testCase "metric and read back returns result" <| fun _ ->
      use db = SQLiteDB.openConn inMemConnStrShared
      let mgr = Sql.withConnection db

      let countSql = "SELECT COUNT(*) FROM Metrics"

      // pre-conditions
      let count = Sql.execScalar mgr countSql [] |> Option.get
      Expect.equal count 0L "count should be zero"

      // given
      let target = start (fun () -> db)
      Message.gauge "web01.app.signin" 3. |> Target.log target |> Job.Ignore |> run
      Message.gauge "web02.app.signin" 6. |> Target.log target |> Job.Ignore |> run
      flush target

      // then
      try
        let count = Sql.execScalar mgr countSql [] |> Option.get
        Expect.equal count 2L "Count should be two metrics"

        let records = Sql.execReader mgr "SELECT * FROM Metrics" [] |> Sql.map Sql.asMap

        for r in records do
          let read k : 'a = read r k
          Expect.equal (read "Host") (Dns.GetHostName()) "should have host name from computer DNS"
          Expect.stringContains (read "Path") ".app.signin" "should have path from from metric"
          Expect.equal (read "Level") (int64 (Info.toInt())) "should have info level"
//          Expect.equal("should have counter type", DB.typeAsInt16 MetricType.Counter |> int64, read "Type")
          Expect.equal (read "Value" = 3.M || read "Value" = 6.M) true "value is 3 or 6"
      finally
        stop target
    ]

open Logary.DB.Migrations
open FluentMigrator.Runner.Processors

[<Tests>]
let migrationTests =
  let fac = SQLite.SQLiteProcessorFactory()
  let forgetful = "FullUri=file::memory:"

  testList "migrating sqlite db up and down" [
    testCase "migrating up" <| fun _ ->
      Runner(fac, forgetful).MigrateUp()

    testCase "migrating up with reading index" <| fun _ ->
      Runner(fac, forgetful).MigrateUp(Runner.IndexForReading)

    testCase "migating down" <| fun _ ->
      Runner(fac, forgetful).MigrateDown()

    testCase "migating down with reading index" <| fun _ ->
      Runner(fac, forgetful).MigrateDown(Runner.IndexForReading)
    ]

[<EntryPoint>]
let main args =
  runTestsInAssembly defaultConfig args