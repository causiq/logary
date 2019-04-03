module Logary.Targets.BigQuery.Tests.Program

open Expecto
open Hopac
open Logary
open Logary.Tests
open Logary.Internals
open Logary.Message
open Logary.Targets
open Logary.Targets.BigQuery
open System
open System.IO

let ri =
  RuntimeInfo.create "tests" "localhost"

let flush =
  Target.flush >> Job.Ignore

let env defaultValue k =
  match Environment.GetEnvironmentVariable k with
  | null when isNull defaultValue ->
    failwithf "Couldn't load key %s" k
  | null ->
    defaultValue
  | v ->
    v

let raisedExn msg =
  let e = ref None: exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value
let raisedExnWithInner  msg inner =
  let e = ref None: exn option ref
  try raise <| ApplicationException(msg,inner)
  with ex -> e := Some ex
  (!e).Value

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
      BigQuery.create bigQuery.Value name) true

    testCaseJob "send" <| job {
      let targetConf = BigQuery.create bigQuery.Value "logary-bigquery"
      let! target = Target.create ri targetConf

      for i in 0..20 do
        let! ack =
          Target.tryLog target (event LogLevel.Error "thing happened at {blah}" |> setField "application" "logary tests" |> setContext "zone" "foobar" |> addExn (raisedExnWithInner "outer exception" (raisedExn "inner exception")))
        do! ack |> function Ok ack -> ack | Result.Error e -> failtestf "Failure placing in buffer %A" e

      do! flush target
    }
  ]

[<EntryPoint>]
let main argv =
  let privKey = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "bigquery-service-account.json")
  Environment.SetEnvironmentVariable("GOOGLE_APPLICATION_CREDENTIALS", privKey)
  Tests.runTestsInAssembly defaultConfig argv