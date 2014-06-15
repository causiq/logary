module Logary.Tests.Fac

open System.IO
open System.Text.RegularExpressions
open System.Text

open FSharp.Actor

open Logary
open Logary.Target.TextWriter
open Logary.Configuration.Config

open Rules
open Targets

open TestDSL

let emptyTarget =
  { name  = "empty target"
  ; actor = Actor.spawn Actor.Options.Default (fun _ -> async { return () }) }

let emptyRule =
  { hiera  = Regex(".*")
  ; target = "empty target"
  ; accept = fun line -> true
  ; level  = Verbose }

let textWriter () =
  let sb = new StringBuilder()
  new StringWriter(sb)

let withLogary f =
  let out, err = textWriter (), textWriter ()

  let target = confTarget "cons" (create <| TextWriterConf.Default(out, err))

  let rule =
    { accept = fun ll -> true
    ; hiera = Regex(".*")
    ; level = LogLevel.Verbose
    ; target = target.name }

  let logary =
    confLogary "tests"
    |> withRules   [ rule ]
    |> withTargets [ target ]
    |> validateLogary
    |> runLogary

  f logary out err

let finaliseLogary = shutdownLogary >> fun a ->
  let state = Async.RunSynchronously a
  (because "finalise should always work" <| fun () ->
    if state.Successful then () else failwithf "finaliseLogary failed %A" state)
  |> thatsIt

let finaliseTarget = shutdownTarget >> fun a ->
  let acks = Async.RunSynchronously(a, 1000)
  match acks with
  | FSharp.Actor.RPC.ExperiencedTimeout actor -> failwith "finalising target timeout"
  | FSharp.Actor.RPC.SuccessWith(acks, actor) ->
    match acks with
    | Nack desc -> failwith "would not shut down: %A"
    | Ack -> ()