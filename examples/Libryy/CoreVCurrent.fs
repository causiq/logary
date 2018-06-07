module Libryy.CoreVCurrent

// Note: this library has no reference to Logary proper
open Libryy.Logging
open Hopac

let coreLogger = Log.create "Libryy.Core"

let getSampleException messagePrefix =
  let m4 () = failwithf "%s (a sample exception)" messagePrefix
  let m3 () = m4 ()
  let m2 () = m3 ()
  let m1 () = m2 ()
  try m1 () with e -> e

let work (logger: Logger) =
  let res =
    logger.logWithAck (false, Warn) (
      Message.eventX "Hey {user}!"
      >> Message.setField "user" "haf"
      >> Message.setSingleName "Libryy.Core.work-work-work"
      >> Message.addExn (getSampleException "Warnings can have exceptions too!")
      >> Message.setTimestamp 1470047883029045000L)
    |> run

  match res with
  | Ok ack -> run ack
  | Result.Error err -> failwithf "%A" err

  42

let workBackpressure (logger: Logger) =
  let ok =
    logger.log Warn (
      Message.eventX "Hey {user}!"
      >> Message.setField "user" "haf"
      >> Message.setSingleName "Libryy.Core.work-bp"
      >> Message.setTimestamp 1470047883029045000L)
    |> run
  if not ok then failwith "Returned false"
  45

let errorWithBP (logger: Logger) =
  logger.errorWithBP (Message.eventX "Too simplistic")
  |> run

  43

let generateAndLogExn (logger: Logger) =
  let ex = getSampleException "Uhoh!"

  logger.error (
    Message.eventX "An error with an attached exception"
    >> Message.addExn ex
    >> Message.addExn (exn "another"))

  99

let staticWork () =
  job {
    do! coreLogger.debugWithBP (Message.eventX "A debug log")
    return 49
  }