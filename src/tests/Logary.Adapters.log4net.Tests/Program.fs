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
open Logary
open Logary.Targets
open Logary.Configuration

let buildTextWriteTarget name =
  let (out, error) = (new StringWriter (), new StringWriter ())
  let twconf = TextWriter.TextWriterConf.create (out, error)
  let twTargetConf = TextWriter.create twconf name
  (out, error, twTargetConf)

let buildLogManager () = job {
  let svc = "svc"
  let host = "localhost"
  let tname = "4test"
  let (out, error, twTargetConf) = buildTextWriteTarget tname
  // let iloggerConf = ILogger.Targets [ twTargetConf ]
  let processing =
    Events.stream
    |> Events.subscribers [
      Events.events |> Events.sink [tname]
    ]
    |> Events.toProcessing


  let! registry =
    Config.create svc host
    // |> Config.ilogger iloggerConf
    // |> Config.ilogger (ILogger.Console Verbose)
    |> Config.target twTargetConf
    |> Config.processing processing
    // |> Config.disableGlobals
    |> Config.build
  let logm = Registry.toLogManager registry
  return (registry, logm, out, error)
}

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

    testCaseAsync "logging to log4net logs to logary" <| (job {
      let logaryAppender = LogaryLog4NetAppender()
      let patternLayout = new PatternLayout(ConversionPattern = "%m")
      logaryAppender.Layout <- patternLayout

      let hiera = newHierarchy (fun hiera -> hiera.Root.AddAppender logaryAppender)
      let log4 = log4net.LogManager.GetLogger("Logary.Tests")


      let! (r, logm, out, error)  = buildLogManager ()
      log4.Fatal "oh noes" 
      // since log4net adapter use logSimple (fire and forget style), so we first wait for logary shutdown then check the output
      do! Registry.shutdown r

      Expect.equal (out.ToString()) "" "should be empty"
      Expect.stringContains (error.ToString()) "oh noes" "should have 'oh noes' in it"

      } |> Job.toAsync)
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
