module Logary.Tests.DB

open Fuchu

open System
open System.Data
open System.Data.SQLite
open System.Net

open Logary
open Logary.Configuration
open Logary.Metric
open Logary.Internals
open Logary.Measure
open Logary.Targets

let emptyRuntime = { serviceName = "tests"; logger = NullLogger() }

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
    { new Logger with
        member x.Log line =
          let fm = Formatting.StringFormatter.VerbatimNewline
          let str = fm.format line
          if not <| String.IsNullOrWhiteSpace str then
            System.Console.Write(str)
            System.Diagnostics.Debugger.Log(6, "tests", str)
        member x.Measure _ = ()
        member x.Level = LogLevel.Info
      interface Named with
        member x.Name = "DB test logger"
        member x.CompareTo other = "DB test logger".CompareTo other
        member x.Equals other = "DB test logger".Equals other }

  open System
  open Logary.DB.Migrations

  open FluentMigrator
  open FluentMigrator.Runner
  open FluentMigrator.Runner.Processors
  open FluentMigrator.Runner.Processors.Sqlite
  open FluentMigrator.Runner.Generators.SQLite

  /// wrap the connection in a non-closing delegator for connections that are
  /// managed externally
  let wrapConnNoClose (inner : IDbConnection) =
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
  type NonClosingSqliteProcessorFactory(conn : IDbConnection) =
    inherit MigrationProcessorFactory()
    override x.Create (connStr : string, accouncer : IAnnouncer, opts : IMigrationProcessorOptions) =
      new SqliteProcessor(wrapConnNoClose conn,
                          new SqliteGenerator(), accouncer, opts,
                          new SqliteDbFactory())
      :> IMigrationProcessor

  let private openConnInner connStr () =
    let conn = new SQLiteConnection(connStr : string) :> IDbConnection
    conn.Open()
    conn

  /// open and migrate with the given connection string
  let openConn connStr =
    Logger.info consoleAndDebugger "openConn"
    let conn = openConnInner connStr ()
    Runner(NonClosingSqliteProcessorFactory(conn), connStr, logger = consoleAndDebugger).MigrateUp()
    conn

  let connMgrShared = Sql.withNewConnection (fun () -> openConn inMemConnStrShared)

let raised_exn msg =
  let e = ref None : exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

let read (m : Map<string, obj>) k =
  try m.[k] :?> 'a
  with :? InvalidCastException as e ->
    Tests.failtestf "converting key %s to %s failed, was: %s."
      k typeof<'a>.Name (m.[k].GetType().Name)

[<Tests>]
let targetTests =
  let flush = Target.flush >> Async.Ignore >> Async.RunSynchronously

  let stop = Target.shutdown >> Async.Ignore >> Async.RunSynchronously

  let start f_conn =
    let conf = DB.DBConf.Create f_conn
    Target.init emptyRuntime (DB.create conf "db-target")

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
        (LogLine.info "hello world") |> Target.sendLogLine target
        flush target
      finally stop target

    testCase "log and read back returns result" <| fun _ ->
      use db = SQLiteDB.openConn inMemConnStrShared
      let mgr = Sql.withConnection db

      let countSql = "SELECT COUNT(*) FROM LogLines"

      // pre-conditions
      let count = Sql.execScalar mgr countSql [] |> Option.get
      Assert.Equal("count should be zero", 0L, count)

      // given
      let target = start (fun () -> db)
      LogLine.create'' "a.b.c" "hello world"
        |> Target.sendLogLine target
      LogLine.create "goodbye world" Map.empty Info ["tests"; "things"] "a.b.c" (Some (raised_exn "hoho"))
        |> Target.sendLogLine target
      flush target

      // then
      try
        let count = Sql.execScalar mgr countSql [] |> Option.get
        Assert.Equal("count should be two log lines", 2L, count)

        let records = Sql.execReader mgr "SELECT * FROM LogLines" [] |> Sql.map Sql.asMap

        for r in records do
          let read k : 'a = read r k
          Assert.Equal("should have host name from computer DNS", Dns.GetHostName(), read "Host")
          Assert.Equal("should have path from", "a.b.c", read "Path")
          Assert.Equal("should have info level", int64 (Info.ToInt()), read "Level")

          if read "Message" = "goodbye world" then
            Assert.Equal("should have comma-separated tags", "tests,things", read "Tags")
            Assert.StringContains("should have exception substring",
                                  "ApplicationException", read "Exception")
            Assert.StringContains("should have exception substring",
                                  "hoho", read "Exception")
      finally
        stop target

    testCase "initialise and metric" <| fun _ ->
      let target = start (fun () -> SQLiteDB.openConn inMemConnStrEmpheral)
      try (Measure.create' "app.signin" 3.0) |> Target.sendMeasure target
      finally stop target

    testCase "metric and read back returns result" <| fun _ ->
      use db = SQLiteDB.openConn inMemConnStrShared
      let mgr = Sql.withConnection db

      let countSql = "SELECT COUNT(*) FROM Metrics"

      // pre-conditions
      let count = Sql.execScalar mgr countSql [] |> Option.get
      Assert.Equal("count should be zero", 0L, count)

      // given
      let target = start (fun () -> db)
      (Measure.create' "web01.app.signin" 3.0) |> Target.sendMeasure target
      (Measure.create' "web02.app.signin" 6.0) |> Target.sendMeasure target
      flush target

      // then
      try
        let count = Sql.execScalar mgr countSql [] |> Option.get
        Assert.Equal("count should be two metrics", 2L, count)

        let records = Sql.execReader mgr "SELECT * FROM Metrics" [] |> Sql.map Sql.asMap

        for r in records do
          let read k : 'a = read r k
          Assert.Equal("should have host name from computer DNS", Dns.GetHostName(), read "Host")
          Assert.StringContains("should have path from from metric", ".app.signin", read "Path")
          Assert.Equal("should have info level", int64 (Info.ToInt()), read "Level")
//          Assert.Equal("should have counter type", DB.typeAsInt16 MetricType.Counter |> int64, read "Type")
          Assert.Equal("value is 3 or 6", true, read "Value" = 3.M || read "Value" = 6.M)
      finally
        stop target
    ]

open Logary.DB.Migrations
open FluentMigrator.Runner.Processors

[<Tests>]
let migrationTests =
  let fac = Sqlite.SqliteProcessorFactory()
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
let main args = defaultMainThisAssembly args