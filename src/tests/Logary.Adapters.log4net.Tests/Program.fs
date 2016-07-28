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
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals
open Logary.Targets.TextWriter
open Logary.Configuration

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

let finaliseLogary = Config.shutdownSimple >> fun a ->
  let state = run a
  Assert.Equal("finalise should always work", true, state.successful)

let finaliseTarget t = Target.shutdown t |> fun a ->
  let acks = a ^-> TimeoutResult.Success <|> timeOutMillis 1000 ^->. TimedOut
             |> Job.Global.run
  match acks with
  | TimedOut -> Tests.failtestf "finalising target timed out: %A" t
  | TimeoutResult.Success _ -> ()

let newHierarchy fHierarchy =
  let tracer = new TraceAppender()
  let hierarchy = log4net.LogManager.GetRepository() :?> Hierarchy
  hierarchy.Root.AddAppender tracer
  let patternLayout = new PatternLayout(ConversionPattern = "%m%n")
  tracer.Layout <- patternLayout
  fHierarchy hierarchy
  hierarchy.Configured <- true

[<Tests>]
let integration =
  testList "integration tests (memory only)" [
    testCase "starting log4net" <| fun _ ->
      newHierarchy (fun _ -> ())

    testCase "logging to log4net logs to logary" <| fun _ ->

      let logaryAppender = LogaryAppender()
      let patternLayout = new PatternLayout(ConversionPattern = "%m")
      logaryAppender.Layout <- patternLayout

      let hiera = newHierarchy (fun hiera -> hiera.Root.AddAppender logaryAppender)
      let log4 = log4net.LogManager.GetLogger("Logary.Tests")

      let out, err =
        withLogary <| fun logary out err ->
          log4.Fatal "oh noes"
          finaliseLogary logary
          out.ToString(), err.ToString()

      Assert.Equal("should be empty", "", out.ToString())
      Assert.StringContains("should have 'oh noes' in it", "oh noes", err.ToString())
    ]

[<Tests>]
let mappings =
  testList "mapping properties' dictionary" [
    testCase "can map empty" <| fun _ ->
      let res = Map.empty |> LogaryHelpers.addProperties (new Util.PropertiesDictionary())
      Assert.Equal("should be empty map", Map.empty, res)

    testCase "can map primitives" <| fun _ ->
      let subject = new Util.PropertiesDictionary()
      subject.["first"] <- 1
      subject.["second"] <- 1us
      subject.["third"] <- 1L
      subject.["fourth"] <- Nullable<_>(123)

      let res = Map.empty |> LogaryHelpers.addProperties subject
      Assert.Equal("has first", 1, unbox res.["first"])
      Assert.Equal("has second", 1us, unbox res.["second"])
      Assert.Equal("has third", 1L, unbox res.["third"])
      Assert.Equal("has fourth", Nullable<_>(123), unbox res.["fourth"])
    ]

////////

[<EntryPoint>]
let main argv =
  defaultMainThisAssembly argv
