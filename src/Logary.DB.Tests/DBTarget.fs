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

[<Tests>]
let tests =
  let inited () =
    let conf = DB.DBConf.Create (fun () ->
      let conn = new SQLiteConnection(inMemConnStr) :> IDbConnection
      conn.Open()
      conn)
    initTarget { serviceName = "tests" } (DB.create conf "db-target")

  let stop = shutdownTarget >> Async.Ignore >> Async.RunSynchronously

  testList "db target" [
    testCase "smoke" <| fun _ ->
      use c = new SQLiteConnection(inMemConnStr) :> IDbConnection
      c.Open()
      ()

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
    ]

open Logary.DB.Migrations
open FluentMigrator.Runner.Processors

[<Tests>]
let tests' =
  let fac = Sqlite.SqliteProcessorFactory()
  testList "migrating sqlite" [
    testCase "migrating up" <| fun _ ->
      Runner(fac, inMemConnStr).MigrateUp()
    testCase "migating down" <| fun _ ->
      Runner(fac, inMemConnStr).MigrateDown()
    ]

[<EntryPoint>]
let main args = defaultMainThisAssembly args