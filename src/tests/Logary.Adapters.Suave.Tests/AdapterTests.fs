module Logary.Adapters.Suave.Tests.AdapterTests

open Expecto
open global.Suave
open global.Suave.Logging
open System.Threading
open Logary
open Hopac

let testLoggers (minLevel : LogLevel) (lineLevel : Suave.Logging.LogLevel) (message : Message ref) =
  let stub = { new Logger with
                  member x.logVerboseWithAck msgFactory =
                    x.logWithAck (msgFactory Verbose)

                  member x.logDebugWithAck msgFactory =
                    x.logWithAck (msgFactory LogLevel.Debug)

                  member x.logWithAck msg =
                    message := msg
                    Alt.always (Promise (()))

                  member x.logSimple msg =
                    message := msg

                  member x.level =
                    minLevel

                  member x.name =
                    PointName.ofSingle "test stub" }

  let subject = SuaveAdapter(stub) :> Suave.Logging.Logger

  subject.Log lineLevel <| fun () ->
    { message       = "test"
      ``exception`` = None
      level         = lineLevel
      path          = "test"
      tsUTCTicks    = 0L
      trace         = Logging.TraceHeader.empty }

type Message with
  member x.message =
    match x.value with
    | Event templ ->
      templ

    | x ->
      failwithf "Unexpected %A" x

[<Tests>]
let tests =
  testList "with levels" [
    testCase "logs nothing on Debug level" <| fun _ ->
      let msg : Message ref = ref (Message.event Info "empty" |> Message.setName (PointName.parse "a.b.c"))
      testLoggers Info Logging.LogLevel.Debug msg
      Expect.equal (!msg).message "empty" "should have 'empty' message"

    testCase "logs same on Info level" <| fun _ ->
      let msg : Message ref = ref (Message.event Info "empty" |> Message.setName (PointName.parse "a.b.c"))
      testLoggers LogLevel.Debug Logging.LogLevel.Info msg
      while (!msg).message = "empty" do Thread.Sleep 500
      Expect.equal (!msg).message "test" "should have 'test' message"

    testCase "logs same on Error level" <| fun _ ->
      let msg : Message ref = ref (Message.event Info "empty" |> Message.setName (PointName.parse "a.b.c"))
      testLoggers Info Logging.LogLevel.Error msg
      while (!msg).message = "empty" do Thread.Sleep 500
      Expect.equal (!msg).message "test" "should have 'test' message"
    ]
