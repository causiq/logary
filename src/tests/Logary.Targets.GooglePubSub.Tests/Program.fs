module Logary.Targets.GooglePubSub.Tests.Program

open Expecto
open Hopac
open Logary
open Logary.Tests
open Logary.Internals
open Logary.Message
open Logary.Targets
open Logary.Targets.GooglePubSub
open System
open System.IO

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

    ptestCaseJob "send" <| job {
      let targetConf = GooglePubSub.create lazyConf.Value "logary-pubsub"
      let! target = Target.create (RuntimeInfo.create "tests" "localhost") targetConf

      for i in 0..20 do
        let! ack =
          Target.tryLog target (event LogLevel.Error "thing happened at {blah}" |> setField "application" "logary tests" |> setContext "zone" "foobar" |> addExn (raisedExnWithInner "outer exception" (raisedExn "inner exception")))
        do! ack |> function Ok ack -> ack | Result.Error e -> failtestf "Failure placing in buffer %A" e

      do! Target.flush target
    }
  ]

[<EntryPoint>]
let main argv =
  let privKey = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "pubsub-service-account.json")
  Environment.SetEnvironmentVariable("GOOGLE_APPLICATION_CREDENTIALS", privKey)
  Tests.runTestsInAssembly defaultConfig argv