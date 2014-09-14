module Program

open System
open System.IO
open System.Text

open log4net
open log4net.Layout
open log4net.Repository.Hierarchy

open log4net.Appender

open NodaTime

open Fuchu

open Logary
open Logary.Internals
open Logary.Targets.TextWriter
open Logary.Configuration

let textWriter () =
  let sb = new StringBuilder()
  new StringWriter(sb)

let withLogary f =
  let out, err = textWriter (), textWriter ()

  let target = Target.confTarget "cons" (create <| TextWriterConf.Create(out, err))

  let rule = Rule.createForTarget "cons"

  let logary =
    confLogary "tests"
    |> withRule rule
    |> withTarget target
    |> Config.validate
    |> runLogary

  f logary out err

let finaliseLogary = Config.shutdown >> fun a ->
  let state = Async.RunSynchronously a
  Assert.Equal(sprintf "should succeed: %O" state, true, state.Successful)

let finaliseTarget = Target.shutdown >> fun a ->
  let acks = Async.RunSynchronously(a, 1000)
  match acks with
  | FSharp.Actor.RPC.ExperiencedTimeout actor -> Tests.failtest "finalising target timeout"
  | FSharp.Actor.RPC.SuccessWith(acks, actor) ->
    match acks with
    | Nack desc -> Tests.failtestf "would not shut down: %s" desc
    | Ack -> ()

let newHierarchy fHierarchy =
  let tracer = new TraceAppender()
  let hierarchy = log4net.LogManager.GetRepository() :?> Hierarchy
  hierarchy.Root.AddAppender tracer
  let patternLayout = new PatternLayout(ConversionPattern = "%m%n")
  tracer.Layout <- patternLayout
  fHierarchy hierarchy
  hierarchy.Configured <- true

[<Tests>]
let tests =
  testList "integration tests (memory only)" [
    testCase "starting log4net" <| fun _ ->
      newHierarchy (fun _ -> ())
    testCase "logging to log4net logs to logary" <| fun _ ->
      let logaryAppender = LogaryAppender()
      let patternLayout = new PatternLayout(ConversionPattern = "%m")
      logaryAppender.Layout <- patternLayout

      let hiera = newHierarchy (fun hiera -> hiera.Root.AddAppender logaryAppender)
      let log4 = log4net.LogManager.GetLogger("logary.tests")

      let out, err =
        withLogary <| fun logary out err ->
          log4.Fatal "oh noes"
          finaliseLogary logary
          out.ToString(), err.ToString()

      Assert.Equal("should be empty", "", out.ToString())
      Assert.StringContains("should have 'oh noes' in it", "oh noes", err.ToString())
    ]

////////

[<EntryPoint>]
let main argv =
  defaultMainThisAssembly argv
