module Logary.Tests.Fac

open System.IO
open System.Text.RegularExpressions
open System.Text

open FSharp.Actor

open Logary
open Logary.Internals
open Logary.Target.TextWriter
open Logary.Configuration.Config
open Logary.Rule
open Logary.Targets

open TestDSL

let emptyTarget =
  { name  = "empty target"
    actor = Actor.spawn Actor.Options.Default (fun _ -> async { return () }) }

let emptyRule = Rule.forAny "empty target"

let textWriter () =
  let sb = new StringBuilder()
  new StringWriter(sb)

let withLogary f =
  let out, err = textWriter (), textWriter ()

  let target = confTarget "cons" (create <| TextWriterConf.Default(out, err))

  let rule = Rule.forAny "cons"

  let logary =
    confLogary "tests"
    |> withRule rule
    |> withTarget target
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