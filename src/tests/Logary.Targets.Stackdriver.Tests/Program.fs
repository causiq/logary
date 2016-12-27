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

let emptyRuntime = RuntimeInfo.create "tests"

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
      Expect.equal (subject.JsonPayload.Fields.["context"]) (Google.Protobuf.WellKnownTypes.Value.ForStruct(Struct.Parser.ParseJson("""{ "service" : "tests" }"""))) "should have correct context"
      
    testCase "send" <| fun _ ->
      
      let creds = Grpc.Auth.GoogleGrpcCredentials.GetApplicationDefaultAsync().Result
      let channel = Grpc.Core.Channel(LoggingServiceV2Client.DefaultEndpoint.Host, creds, [ChannelOption("grpc.initial_reconnect_backoff_ms", 100); ChannelOption(ChannelOptions.MaxConcurrentStreams, 20)])
      let client = LoggingServiceV2Client.Create(channel)
      
      let getCount (client : LoggingServiceV2Client) =
        let req = new ListLogEntriesRequest()
        req.Filter <- sprintf """logName = \"projects/%s/logs/%s\"""  project logName
        req.ProjectIds.Add(project)
        let resp = client.ListLogEntries(req)
        resp |> Seq.length
      let initCount = getCount client 
      let conf = Stackdriver.create targConf "test"
      let target = run <| init emptyRuntime conf
      let logged = Target.log target (event LogLevel.Info "thing happened") |> run |> run

      let afterCount = getCount client
      Expect.isGreaterThan afterCount initCount "there should be more messages after a write"
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv