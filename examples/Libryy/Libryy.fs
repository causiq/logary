module Libryy.Core

// Note: this library has no reference to Logary proper
open Libryy.Logging

let coreLogger = Log.create "Libryy.Core"

let work (logger : Logger) =
  logger.logWithAck Warn (
    Message.eventX "Hey {user}!"
    >> Message.setFieldValue "user" "haf"
    >> Message.setSingleName "Libryy.Core.work"
    >> Message.setTimestamp 1470047883029045000L)
  |> Async.RunSynchronously

  42

let workNonAsync (logger : Logger) =
  logger.log Warn (
    Message.eventX "Hey {user}!"
    >> Message.setFieldValue "user" "haf"
    >> Message.setSingleName "Libryy.Core.work"
    >> Message.setTimestamp 1470047883029045000L)

  45

let simpleWork (logger : Logger) =
  logger.logSimple (Message.event Error "Too simplistic")
  43

let getSampleException () =
  let m4 () = failwith "a sample exception"
  let m3 () = m4 ()
  let m2 () = m3 ()
  let m1 () = m2 ()
  try
    m1 ()
  with
  | ex -> ex

let simpleWorkThatCatchesAndLogsAnErrorAndException (logger : Logger) =
  let ex = getSampleException()
  logger.logSimple (Message.event Error "An error with an attached exception" |> Message.addExn ex)
  99

let staticWork () =
  coreLogger.logSimple (Message.event Debug "A debug log")
  49