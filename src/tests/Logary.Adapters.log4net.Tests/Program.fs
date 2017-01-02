module Program

open System
open System.IO
open System.Text
open log4net
open log4net.Layout
open log4net.Repository.Hierarchy
open log4net.Appender
open NodaTime
open Expecto
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

let finaliseLogary = Config.shutdownSimple >> fun a ->
  let state = run a
  Expect.equal state.successful true "Finalise should always work" 

let finaliseTarget t = Target.shutdown t |> fun a ->
  let acks = a ^-> TimeoutResult.Success <|> timeOutMillis 1000 ^->. TimedOut
             |> run
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

      let logaryAppender = LogaryLog4NetAppender()
      let patternLayout = new PatternLayout(ConversionPattern = "%m")
      logaryAppender.Layout <- patternLayout

      let hiera = newHierarchy (fun hiera -> hiera.Root.AddAppender logaryAppender)
      let log4 = log4net.LogManager.GetLogger("Logary.Tests")

      let out, err =
        withLogary <| fun logary out err ->
          log4.Fatal "oh noes"
          finaliseLogary logary
          out.ToString(), err.ToString()

      Expect.equal (out.ToString()) "" "should be empty"
      Expect.stringContains (err.ToString()) "oh noes" "should have 'oh noes' in it"
    ]

[<Tests>]
let mappings =
  testList "mapping properties' dictionary" [
    testCase "can map empty" <| fun _ ->
      let res = Map.empty |> LogaryHelpers.addProperties (new Util.PropertiesDictionary())
      Expect.equal res Map.empty "Should be empty map"

    testCase "can map primitives" <| fun _ ->
      let subject = new Util.PropertiesDictionary()
      subject.["first"] <- 1
      subject.["second"] <- 1us
      subject.["third"] <- 1L
      subject.["fourth"] <- Nullable<_>(123)

      let res = Map.empty |> LogaryHelpers.addProperties subject
      Expect.equal (unbox res.["first"]) 1 "has first"
      Expect.equal (unbox res.["second"]) 1us "has second"

      Expect.equal (unbox res.["third"]) 1L "has third"
      Expect.equal (unbox res.["fourth"]) (Nullable<_>(123)) "has fourth"
    ]

////////

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv
