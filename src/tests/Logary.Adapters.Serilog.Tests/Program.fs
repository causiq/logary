module Program

open System
open System.IO
open System.Text
open NodaTime
open Fuchu
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals
open Logary.Targets.TextWriter
open Logary.Configuration
open Logary.Adapters.Serilog
open Serilog

let textWriter () =
  let sb = new StringBuilder()
  new StringWriter(sb)

let withLogary f =
  let out, err = textWriter (), textWriter ()
  let jsonTextTarget = confTarget (PointName.ofSingle "textWriter") (create (TextWriterConf.create(out, err)))
  let textWriterRule = Rule.createForTarget (PointName.ofSingle "textWriter")
  
  let logary =
    confLogary "tests"
    |> withRule textWriterRule
    |> withTarget jsonTextTarget
    |> Config.validate
    |> runLogary
    |> run

  f logary out err

let finaliseLogary = Config.shutdownSimple >> fun a ->
  let state = run a
  Assert.Equal("finalise should always work", true, state.successful)

let fatalSerilogLogary template ([<ParamArray>] args: obj[]) =
    withLogary <| fun logary out err ->
        let consLogger = Logary.Logging.getLoggerByName "textWriter"
        let serilogLogger = LoggerConfiguration()
                                .WriteTo.Sink(LogarySerilogSink consLogger)
                                .CreateLogger()
        serilogLogger.Fatal (template, args)
        finaliseLogary logary
        out.ToString(), err.ToString()

type AClassWithAProperty (s:string) =
    member this.TheProperty = s

[<Tests>]
let integration =
  testList "integration tests (memory only)" [

    testCase "logging a fatal scalar to Serilog logs to logary" <| fun _ ->
        let out, err = fatalSerilogLogary "Hello {user}" [| "adam" |]
        Assert.Equal("should be empty", "", out.ToString())
        
        let logaryJsonOutput = err.ToString()
        Assert.StringContains("should be 'fatal' level",
            "\"level\":\"fatal\"", logaryJsonOutput)
        Assert.StringContains("should have the message template in it",
            "\"event\":\"Hello {user}\"", logaryJsonOutput)
        Assert.StringContains("should have the 'user' field in it",
            "{\"user\":{\"value\":\"adam\"}}", logaryJsonOutput)

    testCase "logging a fatal destructured object to Serilog logs to logary" <| fun _ ->
        let out, err = fatalSerilogLogary "Started MyApp v{@version}" [| AClassWithAProperty("1.2.3.4") |]
        Assert.Equal("should be empty", "", out.ToString())
        
        let logaryJsonOutput = err.ToString()
        Assert.StringContains("should be 'fatal' level",
            "\"level\":\"fatal\"", logaryJsonOutput)
        Assert.StringContains("should have the message template in it",
            "\"event\":\"Started MyApp v{@version}\"", logaryJsonOutput)
        Assert.StringContains("should have a structure with the right fields in it",
            "{\"version\":{\"value\":{\"TheProperty\":\"1.2.3.4\"}}}",
            logaryJsonOutput)

    testCase "logging a fatal template with destructured sequence to Serilog logs to logary" <| fun _ ->
        let out, err = fatalSerilogLogary "Found numbers {@numbers}" [| seq { 1..4 } |]
        Assert.Equal("should be empty", "", out.ToString())
        
        let logaryJsonOutput = err.ToString()
        Assert.StringContains("should be 'fatal' level", "\"level\":\"fatal\"", logaryJsonOutput)

        Assert.StringContains("should have the message template in it",
            "\"event\":\"Found numbers {@numbers}\"", logaryJsonOutput)
        Assert.StringContains("should have a structure with the right fields in it",
            "{\"numbers\":{\"value\":[{\"Int64\":1},{\"Int64\":2},{\"Int64\":3},{\"Int64\":4}]}}",
            logaryJsonOutput)

    testCase "logging a fatal template with destructured dictionary to Serilog logs to logary" <| fun _ ->
        let out, err = fatalSerilogLogary "Found numbers {@numbers}" [| seq { 1..4 } |]
        Assert.Equal("should be empty", "", out.ToString())
        
        let logaryJsonOutput = err.ToString()
        Assert.StringContains("should be 'fatal' level", "\"level\":\"fatal\"", logaryJsonOutput)

        Assert.StringContains("should have the message template in it",
            "\"event\":\"Found numbers {@numbers}\"", logaryJsonOutput)
        Assert.StringContains("should have a structure with the right fields in it",
            "{\"numbers\":{\"value\":[{\"Int64\":1},{\"Int64\":2},{\"Int64\":3},{\"Int64\":4}]}}",
            logaryJsonOutput)
    ]

////////

[<EntryPoint>]
let main argv =
  defaultMainThisAssembly argv
