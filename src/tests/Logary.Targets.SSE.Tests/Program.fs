module Logary.Targets.SSE.Tests.Program

open Expecto
open Hopac
open Logary
open Logary.Tests
open Logary.Internals
open Logary.Message
open Logary.Targets
open Logary.Targets.SSE
open System

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

let sse = lazy (SSEConf.create("/logs", port=9095))

[<Tests>]
let target =
  testList "sse" [
    testSequenced <|
    TargetBaseline.basicTests "SSE" (fun name ->
      SSE.create sse.Value name) true

    testCaseJob "send" <| job {
      let targetConf = SSE.create sse.Value "logary-sse"
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
  Tests.runTestsInAssembly defaultConfig argv