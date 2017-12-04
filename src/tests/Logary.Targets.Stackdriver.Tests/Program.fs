module Program

open Expecto
open Google.Cloud.Logging.V2
open Google.Cloud.Logging.Type
open Google.Protobuf.WellKnownTypes
open Google.Api.Gax.Grpc
open Grpc
open Grpc.Core
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Message
open Logary.Target
open Logary.Targets
open Logary.Targets.Stackdriver
open Logary.Serialisation.Chiron
open System
open System.Collections.Generic

let emptyRuntime = RuntimeInfo.create "tests" "localhost"


let flush = Target.flush >> Job.Ignore >> run

let env k =
  match Environment.GetEnvironmentVariable k with
  | null -> failwithf "couldn't load key %s" k
  | v -> v

let logName = env "STACKDRIVER_LOG"
let project = env "STACKDRIVER_PROJECT"
let targConf =
  let labels = Dictionary<_,_>()
  StackdriverConf.create(project, logName, ResourceType.createComputeInstance("us-central1-b", "abcdefg"), labels, 1u)

let raisedExn msg =
  let e = ref None : exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

[<Tests>]
let target =
  testList "stackdriver" [
    testCase "serialise" <| fun _ ->
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
      
    testCase "send" <| fun _ ->
      let conf = Stackdriver.create targConf "test"
      let target = 
            Target.create emptyRuntime conf
            |> run
      
      for i in 0..20 do
        Target.log target (event LogLevel.Info "thing happened at {blah}" |> setField "blah" 12345 |> setContext "zone" "foobar" |> addExn (raisedExn "boohoo")) |> run |> run
      
      target |> flush
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv