module Logary.Internals.Try

open Hopac
open Hopac.Infixes
open Logary

/// Safely try to execute asynchronous function f, catching any thrown
/// exception and logging exception internally. Returns async<unit>
/// irregardless of the codomain of f.
let safe label logger (runnable: Job<_>) =
  Job.startIgnore (Job.catch runnable >>= (function
  | Choice1Of2 () ->
    Job.result ()

  | Choice2Of2 e ->
    Message.eventError label |> Message.addExn e |> Logger.log logger :> Job<_>))  