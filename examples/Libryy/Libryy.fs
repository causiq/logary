module Libryy.Core

open Libryy.Logging

// Note: this library has no reference to Logary proper

let work (logger : Logger) =
  fun () ->
      Message.event Warn "Hey {ho}"
      |> Message.setTimestamp 1234L
      |> Message.setFieldValue "myField" 223
      |> Message.setSingleName "Libryy.Core.work"
  |> logger.log Warn
  |> Async.RunSynchronously

  42

let simpleWork (logger : Logger) =
  logger.logSimple (Message.event Error "Too simplistic")
  43