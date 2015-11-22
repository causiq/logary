module Logary.Tests.Fac

open System.IO
open System.Text.RegularExpressions
open System.Text

open Hopac

open Logary
open Logary.Internals
open Logary.Targets.TextWriter
open Logary.Configuration
open Logary.Target

open TestDSL
open Fuchu


let emptyTarget =
  { name  = "empty target"
    reqCh = Ch.Now.create () }

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

  f logary out err

let finaliseLogary = Config.shutdown >> fun a ->
  let state = Job.Global.run a
  (because "finalise should always work" <| fun () ->
    if state.Successful then () else Tests.failtestf "finaliseLogary failed %A" state)
  |> thatsIt

let finaliseTarget = Target.shutdown >> fun a ->
  let acks = Job.withTimeout (HopacTimeout (System.TimeSpan.FromSeconds(1.0))) a
             |> Job.Global.run
  match acks with
  | HopacTimedOut -> Tests.failtest "finalising target timeout"
  | HopacSuccess acks ->
    match acks with
    | Nack desc -> Tests.failtestf "would not shut down: %s" desc
    | Ack -> ()