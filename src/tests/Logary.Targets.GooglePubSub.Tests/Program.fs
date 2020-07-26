module Logary.Targets.GooglePubSub.Tests.Program

open Expecto
open Hopac
open Logary
open Logary.Model
open Logary.Tests
open Logary.Internals
open Logary.Targets
open Logary.Targets.GooglePubSub
open System
open System.IO

let lazyConf =
  lazy (
    let project = env "crucial-baton-203418" "GOOGLE_PUB_SUB_PROJECT"
    let topic = env "logs" "GOOGLE_PUB_SUB_TOPIC"
    GooglePubSubConf.create(project, TopicSelector.Constant topic)
  )

[<Tests>]
let target =
  testList "pubsub" [
    TargetBaseline.basicTests "Google PubSub" (fun name ->
      GooglePubSub.create lazyConf.Value name) true

    testCaseJob "send" <| job {
      let targetConf = GooglePubSub.create lazyConf.Value "logary-pubsub"
      let ri = RuntimeInfo.ofServiceAndHost "pubsub-tests" "localhost"
      let! target = Target.create ri targetConf

      for i in 0..20 do
        let e = raisedExnWithInner "outer exception" (raisedExn "inner exception")
        let evt = Model.Event("thing happened at {blah}", None, level=Error)
        evt.setContext("zone", "foobar")
        evt.setField("application", "logary tests")
        evt.addExn e
        let! ack = Target.tryLog target evt
        do! ack |> function Ok ack -> ack | Result.Error e -> failtestf "Failure placing in buffer %A" e

      do! Target.flush target
    }
  ]

[<EntryPoint>]
let main argv =
  let privKey = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "pubsub-service-account.json")
  Environment.SetEnvironmentVariable("GOOGLE_APPLICATION_CREDENTIALS", privKey)
  Tests.runTestsInAssembly defaultConfig argv