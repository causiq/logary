module Libryy.Core

// Note: this library has no reference to Logary proper
open Libryy.Logging

let coreLogger = Log.create "Libryy.Core"

let getSampleException messagePrefix =
  let m4 () = failwithf "%s (a sample exception)" messagePrefix
  let m3 () = m4 ()
  let m2 () = m3 ()
  let m1 () = m2 ()
  try
    m1 ()
  with
  | ex -> ex

let work (logger: Logger) =
  logger.logWithAck Warn (
    Message.eventX "Hey {user}!"
    >> Message.setFieldValue "user" "haf"
    >> Message.setSingleName "Libryy.Core.work"
    >> Message.addExn (getSampleException "Warnings can have exceptions too!")
    >> Message.setTimestamp 1470047883029045000L)
  |> Async.RunSynchronously

  42

let workBackpressure (logger: Logger) =
  logger.log Warn (
    Message.eventX "Hey {user}!"
    >> Message.setFieldValue "user" "haf"
    >> Message.setSingleName "Libryy.Core.work"
    >> Message.setTimestamp 1470047883029045000L)
  |> Async.RunSynchronously

  45

let errorWithBP (logger: Logger) =
  logger.errorWithBP (Message.eventX "Too simplistic") |> Async.RunSynchronously
  43

let generateAndLogExn (logger: Logger) =
  let ex = getSampleException "Uhoh!"
  logger.logSimple (Message.event Error "An error with an attached exception"
                    |> Message.addExn ex
                    |> Message.addExn (exn "another"))
  99

let staticWork () =
  async {
    do! coreLogger.debugWithBP (Message.eventX "A debug log")
    return 49
  }