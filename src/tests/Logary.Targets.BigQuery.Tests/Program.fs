module Logary.Targets.BigQuery.Tests.Program

open Expecto
open Hopac
open Logary
open Logary.Tests
open Logary.Internals
open Logary.Targets.BigQuery
open System
open System.IO

let bigQuery =
  lazy (
    let project = env "crucial-baton-203418" "BIGQUERY_PROJECT"
    let dataset = env "logs" "BIGQUERY_DATASET"
    let table = env "all" "BIGQUERY_TABLE"
    BigQueryConf.create(project, dataset, table)
  )

[<Tests>]
let target =
  testList "big query" [
    TargetBaseline.basicTests "Google BigQuery" (fun name ->
      create bigQuery.Value name) true

    testCaseJob "send" <| job {
      let ri = RuntimeInfo.ofServiceAndHost "bigquery-tests" "localhost"
      let targetConf = create bigQuery.Value "logary-bigquery"
      let! target = Target.create ri targetConf

      for _ in 0..20 do
        let! ack =
          let e = raisedExnWithInner "outer exception" (raisedExn "inner exception")
          let evt = Model.Event("thing happened at {blah}", None, level=Error)
          evt.setContext("zone", "foobar")
          evt.setField("application", "logary tests")
          evt.addExn e
          Target.tryLog target evt
        do! ack |> function Ok ack -> ack | Result.Error e -> failtestf "Failure placing in buffer %A" e

      do! finalise target
    }
  ]

[<EntryPoint>]
let main argv =
  let privKey = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "bigquery-service-account.json")
  Environment.SetEnvironmentVariable("GOOGLE_APPLICATION_CREDENTIALS", privKey)
  runTestsInAssembly defaultConfig argv