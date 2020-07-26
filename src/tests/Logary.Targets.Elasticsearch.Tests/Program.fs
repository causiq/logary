module Program

open System
open Expecto
open Expecto.Flip
open Hopac
open Logary
open Logary.Model
open Logary.Tests
open Logary.Targets
open Logary.Internals
open Logary.Internals.Chiron

let raisedExn msg =
  let e = ref None: exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

let now =  System.DateTime.UtcNow.Ticks

[<Tests>]
let target =
  testList "elasticsearch" [
    TargetBaseline.basicTests "Elasticsearch" (Elasticsearch.create Elasticsearch.empty) false

    testCase "serialise" <| fun _ ->
      let e2 = raisedExn "actual exn"
      let ctx = Map [ "data-key", Value.Str "data-value"; "tags", Value.Str "integration" ]
      let fs = Map []
      let subject = Event("Testing started", None, now, Id.create(), PointName.parse "a.b.c", Info, ctx, fs)
      subject.addExn(e2)

      Elasticsearch.Impl.serialise subject
        |> Expect.isNotNull "Has a value"
    ]

[<EntryPoint>]
let main argv =
  //Tests.runTestsInAssembly defaultConfig argv
  0
