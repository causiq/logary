module Logary.Tests.NullLogger

open Expecto
open Expecto.Flip
open Hopac
open Logary
open Logary.Internals

let tests = [
  testCase "should be named 'Logary.NullLogger'" <| fun () ->
    NullLogger.instance.name
      |> Expect.equal "Is called Logary.NullLogger" (PointName.parse "Logary.NullLogger")

  testCaseJob "logWithAck success" (job {
    let sut = NullLogger.instance
    let! p = sut.logWithAck(true, Model.Event("hi", level=Fatal))
    match p with
    | Ok ack ->
      do! ack
    | Result.Error e ->
      failtestf "%A" e
  })

  testCaseJob "logAck success" (job {
    let! ok = NullLogger.instance.fatalAck "Hi"
    ok |> Expect.isTrue "Success"
  })

  testCaseJob "logBP success" (job {
    let sut = NullLogger.instance
    let! res = sut.logBP (Model.Event "testing logBP")
    res |> Expect.isTrue "Should return true as a stubbed value"
  })
]
