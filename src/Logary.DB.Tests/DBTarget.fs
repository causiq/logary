module Logary.DB.Tests

open Fuchu

open Logary
open Logary.Configuration
open Logary.Target
open Logary.Targets
open Logary.Metric

open System.Data
open System.Data.SQLite

[<Literal>]
let private inMemConnStr = "FullUri=file::memory:?cache=shared"

module SQLiteDB =

  open System
  open Logary.DB.Migrations
  open FluentMigrator.Runner.Processors

  let private consoleAndDebugger =
    { new Logger with
        member x.Log line =
          let line = { line with message = (line.message.TrimEnd([| '\n'; '\r'|])) }
          let fm = Formatting.StringFormatter.VerbatimNewline
          let str = fm.format line
          System.Console.Write(str)
          System.Diagnostics.Debugger.Log(6, "tests", str)
        member x.Metric _ = ()
        member x.Name = "DB test logger"
        member x.Level = LogLevel.Verbose }

  let private openConnInner () =
    let conn = new SQLiteConnection(inMemConnStr) :> IDbConnection
    conn.Open()
    conn

  let openConn () =
    let fac = Sqlite.SqliteProcessorFactory()
    let c = openConnInner ()
    Runner(fac, inMemConnStr, logger = consoleAndDebugger).MigrateUp()
    c

[<Tests>]
let tests =
  let inited () =
    let conf = DB.DBConf.Create SQLiteDB.openConn
    initTarget { serviceName = "tests" } (DB.create conf "db-target")

  let stop = shutdownTarget >> Async.Ignore >> Async.RunSynchronously

  testList "db target" [
    testCase "smoke" <| fun _ ->
      use c = new SQLiteConnection(inMemConnStr) :> IDbConnection
      c.Open()

    testCase "initialise" <| fun _ ->
      stop (inited ())

    testCase "initialise and log" <| fun _ ->
      let target = inited ()
      (LogLine.Create "hello world") |> logTarget target
      stop target

    testCase "initialise and metric" <| fun _ ->
      let target = inited ()
      (Metric.Counter.counterValue "app.signin" 4.) |> metricTarget target
      stop target

    testCase "log and read back returns result" <| fun _ ->
      Tests.skiptest "log and read back not impl"
      
    testCase "log and read back: good field contents" <| fun _ ->
      Tests.skiptest "log and read back not impl"
      
    testCase "metric and read back returns result" <| fun _ ->
      Tests.skiptest "metric and read back not impl"

    testCase "metric and read back: good field contents" <| fun _ ->
      Tests.skiptest "metric and read back not impl"
    ]

open Logary.DB.Migrations
open FluentMigrator.Runner.Processors

[<Tests>]
let migrations =
  let fac = Sqlite.SqliteProcessorFactory()
  let forgetful = "FullUri=file::memory:"

  testList "migrating sqlite db up and down" [
    testCase "migrating up" <| fun _ ->
      Runner(fac, forgetful).MigrateUp()

    testCase "migating down" <| fun _ ->
      Runner(fac, forgetful).MigrateDown()
    ]

[<EntryPoint>]
let main args = defaultMainThisAssembly args