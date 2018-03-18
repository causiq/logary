module Logary.Tests.TargetBaseline

open Logary
open Logary.Tests.Utils
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
let basicTests targetName confFac =
  testList (sprintf "basic tests for target '%s'" targetName) [
    testCaseAsync "creating instance" <| (job {
      do! logger.infoWithBP (eventX "Creating instance: calling configFactory")
      let conf = confFac targetName
      do! logger.infoWithBP (eventX "Creating instance: creating target")
      let! targetApi = Target.create emptyRuntime conf
      do! logger.infoWithBP (eventX "Creating instance: asserting")
      Expect.equal targetApi.Name targetName "Should be named"
    } |> Job.toAsync)

    testCaseAsync "start, log and stop" <| (job {
      let conf = confFac targetName
      let! targetApi = Target.create emptyRuntime conf
      do! logger.infoWithBP (eventX "Start, log and stop: log and wait")
      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          Message.eventInfo "Hello World!" |> logAndWait)
      do! logger.infoWithBP (eventX "Start, log and stop: done!")
    } |> Job.toAsync)

    testCaseAsync "log exception message" <| (job {
      let conf = confFac targetName
      let! targetApi = Target.create emptyRuntime conf
      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait exnMsg)
    } |> Job.toAsync)
  ]