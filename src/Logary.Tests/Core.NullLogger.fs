module Logary.Tests.NullLogger

open Expecto
open Hopac
open Logary
open Logary.Message
open Logary.Internals

let tests = [
  testCase "should be named 'Logary.NullLogger'" <| fun () ->
    let sut = NullLogger.instance
    Expect.equal sut.name (PointName.parse "Logary.NullLogger")
                 "Is called Logary.NullLogger"

  testCaseJob "logWithAck success" (job {
    let sut = NullLogger.instance
    let! p = sut.logWithAck (true, Fatal) (eventX "hi")
    match p with
    | Ok ack ->
      do! ack
    | Result.Error e ->
      failtestf "%A" e
  })

  testCaseJob "logAck success" (job {
    do! NullLogger.instance.logAck Fatal (eventX "Hi")
  })

  testCaseJob "log success" (job {
    let sut = NullLogger.instance
    let! res = sut.log Fatal (eventX "hi")
    Expect.isTrue res "Should return true as a stubbed value"
  })
]
