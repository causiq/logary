module Logary.Tests.Fac

open System.IO
open System.Text.RegularExpressions
open System.Text
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

let emptyTarget = Noop.create {isYes = true} (PointName.ofSingle "empty target")
let emptyRule = Rule.createForTarget (PointName.ofSingle "empty target")
let emptyRuntime = { serviceName = "tests"; logger = NullLogger() }

let textWriter () =
  let sb = new StringBuilder()
  new StringWriter(sb)

let withLogary f =
  let out, err = textWriter (), textWriter ()

  let target = confTarget (PointName.ofSingle "cons") (create (TextWriterConf.create(out, err)))

  let rule = Rule.createForTarget (PointName.ofSingle "cons")

  let logary =
    confLogary "tests"
    |> withRule rule
    |> withTarget target
    |> Config.validate
    |> runLogary
    |> run

  f logary out err

let logTarget target =
  Target.log target >> run >> run

let finaliseLogary = Config.shutdownSimple >> fun a ->
  let state = Job.Global.run a
  (because "finalise should always work" <| fun () ->
    if state.successful then () else Tests.failtestf "finaliseLogary failed %A" state)
  |> thatsIt

let finaliseTarget t = Target.shutdown t |> fun a ->
  let acks = a ^-> TimeoutResult.Success <|> timeOutMillis 1000 ^->. TimedOut
             |> Job.Global.run
  match acks with
  | TimedOut -> Tests.failtestf "finalising target timed out: %A" t
  | TimeoutResult.Success _ -> ()