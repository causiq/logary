module Program

open System
open Expecto
open Hopac
open Logary
open Logary.Tests
open Logary.Targets
open Logary.Internals
open Logary.Internals.Chiron

let targConf =
  Elasticsearch.ElasticsearchConf.create()

let start () =
  let emptyRuntime = RuntimeInfo.create "tests" "localhost"
  Target.create emptyRuntime (Elasticsearch.create targConf "elasticsearch")
  |> run

let shutdown t = Target.shutdown t |> run |> run

let raisedExn msg =
  let e = ref None: exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

let now = Message.setUTCTicks System.DateTime.UtcNow.Ticks

[<Tests>]
let target =
  testList "elasticsearch" [
    TargetBaseline.basicTests "Elasticsearch" (Elasticsearch.create Elasticsearch.empty) false

    testCase "serialise" <| fun _ ->
      let e1 = raisedExn "darn"
      let e2 = raisedExn "actual exn"

      let subject =
        Message.eventWarn "Testing started"
        |> Message.setName (PointName.ofArray [| "a"; "b"; "c" |])
        |> Message.setField "data-key" "data-value"
        |> Message.setField "tags" [ "integration" ]
        |> Message.setField "e" e1
        |> Message.setContext "service" "tests"
        |> Message.addExn e2
        |> now
        |> Elasticsearch.serialise
        |> Json.format

      Expect.equal subject "expected" "should serialise to proper message"
    ]

[<EntryPoint>]
let main argv =
  //Tests.runTestsInAssembly defaultConfig argv
  0
