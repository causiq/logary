module Logary.Tests.Engine

open Logary
open Logary.Message
open Expecto
open Hopac
open Hopac.Infixes

let throttled stream publish =
  Alt.withNackFun <| fun nack ->
  let give state (elapsed : int64) =
    match state with
    | None -> Alt.always ()
    | Some message ->
      message
      |> Message.tag "throttled"
      |> Ch.give publish

  let rec inner state =
    Alt.choose [
      stream ^=> (function
        | NodeInput.Message message ->
          Some message |> inner
        | NodeInput.EventTime lwm ->
          inner state)
      upcast nack
    ]

  inner None

let tests =
  [
    testCase "empty flow" <| fun () ->
      Flow.singleton 43
      |> Flow.map (fun (i : int) -> i + 1)
      |> ignore

    testCase "play" <| fun () ->
      let processing =
        Events.stream
        |> Events.subscribers [
          Events.filterBy (service "riak")
          |> Events.sink "email" ["henrik@haf.se"]

          Events.filterBy (service "spns")
          |> Events.sink "elasticsearch"
        ]
        |> Events.toProcessing

      Engine.create processing |> ignore


    testList "lifetime" [
      testCaseAsync "create and shutdown" (job {
        let! engine = Engine.create (Processing throttled)
        do! Engine.shutdown engine
      } |> Job.toAsync)

      testCaseAsync "pause and resume" (job {
        let! engine = Engine.create (Processing throttled)
        do! Engine.pause engine
        do! Engine.resume engine
        do! Engine.shutdown engine
      } |> Job.toAsync)
    ]

    testList "processing" [
      testCaseAsync "no ticks, no processing" (job {
        // context
        let! engine = Engine.create (Processing throttled)
        // given
        let mref = IVar ()
        let sink message = IVar.fill mref message
        do! Engine.subscribe "end" sink
        // when
        let! isDone = Engine.logWithAck engine Verbose (eventX "Hello World")
        do! isDone
        // then
        Expect.isTrue mref.Full "Should have value"
        let! res = IVar.read mref
        Expect.equal res.value (Event "Hello World") "Should have event"
        Expect.isFalse (Message.hasTag "throttled" res) "Should not be throttled event"
        // finally
        do! Engine.shutdown engine
      } |> Job.toAsync)
    ]

    testCase "identity routes to sink" <| fun () ->
      Tests.skiptest "TODO"

    testCase "metric-created messages to sink" <| fun () ->
      Tests.skiptest "TODO"

    testCase "metric-created messages to metrics" <| fun () ->
      Tests.skiptest "TODO"

    testCase "metric-derived messages to sink (m->m->s)" <| fun () ->
      Tests.skiptest "TODO"
  ]