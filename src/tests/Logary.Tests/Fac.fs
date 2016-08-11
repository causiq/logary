module Logary.Tests.Fac

open System.IO
open System.Text.RegularExpressions
open System.Text
open NodaTime
open Logary
open Logary.Internals
open Logary.Targets
open Logary.Targets.TextWriter
open Logary.Configuration
open Logary.Target
open TestDSL
open Fuchu
open Hopac
open Hopac.Infixes

let emptyTarget = Noop.create {isYes = true} "empty target"
let emptyRule = Rule.createForTarget "empty target"
let emptyRuntime =
  { serviceName = "tests"
    clock       = SystemClock.Instance
    logger      = NullLogger() }

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

  //try 
  f logary out err
  //finally finaliseLogary logary

let logTarget target =
  Target.log target >> run >> run

let finaliseTarget t = Target.shutdown t |> fun a ->
  let acks = a ^-> TimeoutResult.Success <|> timeOutMillis 1000 ^->. TimedOut
             |> run
  match acks with
  | TimedOut -> Tests.failtestf "finalising target timed out: %A" t
  | TimeoutResult.Success _ -> ()