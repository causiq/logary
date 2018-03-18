module Program

open Expecto
open Google.Cloud.Logging.V2
open Google.Cloud.Logging.Type
open Google.Protobuf.WellKnownTypes
open Google.Api.Gax.Grpc
open Grpc
open Grpc.Core
open Hopac
open Logary
open Logary.Tests
open Logary.Internals
open Logary.Message
open Logary.Target
open Logary.Targets
open Logary.Targets.Stackdriver
open System
open System.Collections.Generic

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

let stackdriver =
  lazy (
    let logName = env "logary-tests" "STACKDRIVER_LOG"
    let project = env "tradera-lab" "STACKDRIVER_PROJECT"
    let labels = Dictionary<_,_>()
    StackdriverConf.create(project, logName, ResourceType.createComputeInstance("us-central1-b", "ci-server"), labels, 1u)
  )

[<Tests>]
let target =
  testList "stackdriver" [
    TargetBaseline.basicTests "Google Stackdriver" (fun name ->
      Stackdriver.create stackdriver.Value name)

    testCase "serialise" <| fun () ->
      let e1 = raisedExn "darn"
      let e2 = raisedExn "actual exn"

      let subject =
        Message.eventWarn "Testing started"
        |> Message.setField "data-key" "data-value"
        |> Message.setField "tags" [ "integration" ]
        |> Message.setContext "service" "tests"
        |> Message.addExn e2
        |> Stackdriver.Impl.write

      Expect.equal subject.Severity LogSeverity.Warning "severity should be warning"
      Expect.equal (subject.Labels.["service"]) "tests" "should have correct context"

    testCaseJob "send" <| job {
      let targetConf = Stackdriver.create stackdriver.Value "logary-stackdriver"
      let! target = Target.create ri targetConf

      for i in 0..20 do
        let! ack = Target.log target (event LogLevel.Info "thing happened at {blah}" |> setField "blah" 12345 |> setContext "zone" "foobar" |> addExn (raisedExn "boohoo"))
        do! ack

      do! flush target
    }
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv