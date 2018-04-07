module Logary.Tests.TargetBaseline

open Logary
open Logary.Tests.Utils
open NodaTime
open Hopac
open Expecto
open Expecto.Logging
open Expecto.Logging.Message

let private logger = Log.create "Logary.Tests.TargetBaseline"

/// Run basic tests against the target;
///
///  - can create instance thereof
///  - can start and stop
///  - can receive a few different sorts of messages
///
let basicTests targetName confFac =
  testList (sprintf "basic tests for target '%s'" targetName) [
    testCaseJob "creating instance" <| job {
      let! ri, _ = emptyRuntime
      do! logger.infoWithBP (eventX "Creating instance: calling configFactory")
      let conf = confFac targetName
      do! logger.infoWithBP (eventX "Creating instance: creating target")
      let! targetApi = Target.create ri conf
      do! logger.infoWithBP (eventX "Creating instance: asserting")
      Expect.equal targetApi.Name targetName "Should be named"
    }

    testCaseJob "start, log and stop" <| job {
      let conf = confFac targetName
      let! ri, _ = emptyRuntime
      let! targetApi = Target.create ri conf
      do! logger.infoWithBP (eventX "Start, log and stop: log and wait")
      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
        let now = SystemClock.Instance.GetCurrentInstant()
        Message.eventInfo (sprintf "User signed up! @ %O" now) |> logAndWait)
      do! logger.infoWithBP (eventX "Start, log and stop: done!")
    }

    testCaseJob "log exception message" <| job {
      let! ri, _ = emptyRuntime
      let conf = confFac targetName
      let! targetApi = Target.create ri conf
      let exnMsg =
        let now = SystemClock.Instance.GetCurrentInstant()
        { exnMsg with value = sprintf "%s @ %O" exnMsg.value now }
      do! logMsgWaitAndShutdown targetApi (fun logAndWait -> logAndWait exnMsg)
    }
  ]