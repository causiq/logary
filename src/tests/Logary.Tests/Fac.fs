module Logary.Tests.Fac

open System.IO
open System.Text.RegularExpressions
open System.Text
open System.Net
open NodaTime
open Expecto
open Logary
open Logary.Internals
open Logary.Targets
open Logary.Targets.TextWriter
open Logary.Configuration
open Logary.Target
open TestDSL
open Hopac
open Hopac.Infixes

let emptyTarget = Noop.create {isYes = true} "empty target"
let emptyRule = Rule.createForTarget "empty target"
let emptyRuntime = RuntimeInfo.create "tests"

let literal () =
  LiterateConsole.create LiterateConsole.empty "literate"
  |> Target.init emptyRuntime
  >>= (fun ti -> Job.start (ti.server (fun _ -> Job.result ()) None) >>-. ti)
  >>- (Seq.singleton >> InternalLogger.create LogLevel.Debug)
  |> run

let textWriter () =
  let sb = new StringBuilder()
  new StringWriter(sb)

let finaliseLogary = Config.shutdownSimple >> fun a ->
  let state = run a
  (because "finalise should always work" <| fun () ->
    if state.successful then () else Tests.failtestf "finaliseLogary failed %A" state)
  |> thatsIt

let withLogary f =
  let out, err = textWriter (), textWriter ()

  let target = confTarget "cons" (create (TextWriterConf.create(out, err)))

  let rule = Rule.createForTarget "cons"

  let logary =
    confLogary "tests"
    |> withRule rule
    |> withTarget target
    |> Config.validate
    |> runLogary
    |> run

  f logary out err