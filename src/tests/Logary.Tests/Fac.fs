module Logary.Tests.Fac

open System.IO
open System.Text.RegularExpressions
open System.Text

open FSharp.Actor

open Logary
open Logary.Internals
open Logary.Targets.TextWriter
open Logary.Configuration
open Logary.Target

open TestDSL
open Fuchu

let emptyTarget =
  { name  = "empty target"
    actor = Actor.spawn Actor.Options.Default (fun _ -> async.Return ()) }

let emptyRule = Rule.createForTarget "empty target"

/// { serviceName = "tests"; logger = NullLogger() }
let emptyRuntime = { serviceName = "tests"; logger = NullLogger() }

let textWriter () =
  let sb = new StringBuilder()
  new StringWriter(sb)

let withLogary f =
  let out, err = textWriter (), textWriter ()

  let target = confTarget "cons" (create <| TextWriterConf.Create(out, err))

  let rule = Rule.createForTarget "cons"

  let logary =
    confLogary "tests"
    |> withRule rule
    |> withTarget target
    |> Config.validate
    |> runLogary
    |> Async.RunSynchronously

  f logary out err

let finaliseLogary = Config.shutdown >> fun a ->
  let state = Async.RunSynchronously a
  (because "finalise should always work" <| fun () ->
    if state.Successful then () else Tests.failtestf "finaliseLogary failed %A" state)
  |> thatsIt

let finaliseTarget = Target.shutdown >> fun a ->
  let acks = Async.RunSynchronously(a, 1000)
  match acks with
  | FSharp.Actor.RPC.ExperiencedTimeout actor -> Tests.failtest "finalising target timeout"
  | FSharp.Actor.RPC.SuccessWith(acks, actor) ->
    match acks with
    | Nack desc -> Tests.failtestf "would not shut down: %s" desc
    | Ack -> ()