module Logary.Tests.Engine

open System.Text
open System.IO
open Logary
open Logary.Message
open Expecto
open Hopac
open Hopac.Infixes

// let throttled stream publish =
//   Alt.withNackFun <| fun nack ->
//   let give state (elapsed : int64) =
//     match state with
//     | None -> Alt.always ()
//     | Some message ->
//       message
//       |> Message.tag "throttled"
//       |> Ch.give publish

//   let rec inner state =
//     Alt.choose [
//       stream ^=> (function
//         | NodeInput.Message message ->
//           Some message |> inner
//         | NodeInput.EventTime lwm ->
//           give state lwm ^=>. inner state)
//       upcast nack
//     ]

//   inner None


let tests =
  [
    testCase "empty flow" <| fun () ->
      Flow.singleton 43
      |> Flow.map (fun (i : int) -> i + 1)
      |> ignore

    testCaseAsync "play" (job {
      let pipes =
        Events.stream
        |> Events.subscribers [
          Pipe.start
          |> Events.service "svc1"
          |> Events.sink "console"

          Pipe.start
          |> Events.service "svc2"
          |> Events.sink "test"
        ]
        |> Events.toPipes

      let! engine = Engine.create pipes

      // given
      let mref = IVar ()
      let sink message = IVar.fill mref message
      do! Engine.subscribe engine "test" sink

      let eventSvc1 = ( eventX "Hello World" ) >> Message.setContext KnownLiterals.ServiceContextName "svc1"
      let eventSvc2 = ( eventX "another hello world" ) >> Message.setContext KnownLiterals.ServiceContextName "svc2"

      let! isDone = Engine.logWithAck engine Verbose eventSvc1
      do! isDone

      Expect.isFalse mref.Full "Should not have value"

      let! isDone = Engine.logWithAck engine Verbose eventSvc2
      do! isDone

      Expect.isTrue mref.Full "Should have value"
      let! res = IVar.read mref
      Expect.equal res.value (Event "another hello world") "Should have event"

      // finally
      do! Engine.shutdown engine
    } |> Job.toAsync)

    testList "lifetime" [
      testCaseAsync "create and shutdown" (job {
        let! engine = Engine.create (List.empty)
        do! Engine.shutdown engine
      } |> Job.toAsync)

      testCaseAsync "pause and resume" (job {
        let! engine = Engine.create (List.empty)
        do! Engine.pause engine
        do! Engine.resume engine
        do! Engine.shutdown engine
      } |> Job.toAsync)
    ]

    // testList "processing" [
    //   testCaseAsync "no ticks, no processing" (job {
    //     // context
    //     let! engine = Engine.create (Processing throttled)
    //     // given
    //     let mref = IVar ()
    //     let sink message = IVar.fill mref message
    //     do! Engine.subscribe engine "end" sink
    //     // when
    //     let! isDone = Engine.logWithAck engine Verbose (eventX "Hello World")
    //     do! isDone
    //     // then
    //     Expect.isTrue mref.Full "Should have value"
    //     let! res = IVar.read mref
    //     Expect.equal res.value (Event "Hello World") "Should have event"
    //     Expect.isFalse (Message.hasTag "throttled" res) "Should not be throttled event"
    //     // finally
    //     do! Engine.shutdown engine
    //   } |> Job.toAsync)
    // ]

    testCase "identity routes to sink" <| fun () ->
      Tests.skiptest "TODO"

  ]