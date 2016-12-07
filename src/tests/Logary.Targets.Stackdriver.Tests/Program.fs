module Program

open System
open Expecto
open Hopac
open Hopac.Infixes
open Logary.Utils.Chiron
open Logary
open Logary.Target
open Logary.Targets
open Logary.Internals
open Google.Logging.V2
open Logary.Message
open Google.Logging.Type
open Google.Protobuf.WellKnownTypes

let emptyRuntime = RuntimeInfo.create "tests"

let flush = Target.flush >> Job.Ignore >> run

let env k =
  match Environment.GetEnvironmentVariable k with
  | null -> failwithf "couldn't load key %s" k
  | v -> v

let logName = env "STACKDRIVER_LOG"
let project = env "STACKDRIVER_PROJECT"
let targConf =
  let labels = System.Collections.Generic.Dictionary<_,_>()
  labels.["test"] <- "foo"
  Stackdriver.StackdriverConf.create(project, logName, Stackdriver.ResourceType.createComputeInstance("us-central1-b", "abcdefg"), labels, 1u)

let raisedExn msg =
  let e = ref None : exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

let now = Message.setUTCTicks System.DateTime.UtcNow.Ticks

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
        |> Stackdriver.write

      Expect.equal subject.Severity LogSeverity.Warning "severity should be warning"
      Expect.equal (subject.JsonPayload.Fields.["context"]) (Google.Protobuf.WellKnownTypes.Value.ForStruct(Struct.Parser.ParseJson("""{ "service" : "tests" }"""))) "should have correct context"
      
    testCase "send" <| fun _ ->
      let getCount () =
        let client = Google.Logging.V2.LoggingServiceV2Client.Create()
        let resp = client.ListLogEntries([|project|], "", "")
        resp |> Seq.length
      let initCount = getCount () 
      let conf = Stackdriver.create targConf "test"
      let target = run <| init emptyRuntime conf
      let logged = Target.log target (event LogLevel.Info "thing happened") |> run |> run

      let afterCount = getCount()
      Expect.isGreaterThan afterCount initCount "there should be more messages after a write"

      
  ]
    

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv