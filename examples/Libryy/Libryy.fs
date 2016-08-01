module Libryy.Core

// Note: this library has no reference to Logary proper
open Libryy.Logging

let work (logger : Logger) =
  fun () ->
      Message.event Warn "Hey {user}!"
      |> Message.setFieldValue "user" "haf"
      |> Message.setSingleName "Libryy.Core.work"
      |> Message.setTimestamp 1470047883029045000L
  |> logger.log Warn
  |> Async.RunSynchronously

  42

let simpleWork (logger : Logger) =
  logger.logSimple (Message.event Error "Too simplistic")
  43