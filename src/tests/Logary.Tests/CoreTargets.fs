module Logary.Tests.CoreTargets

open Fuchu
open Logary
open Logary.Targets
open Logary.Tests.Targets
open Hopac
open TestDSL
open Fac

let textWriterConf =
  TextWriter.TextWriterConf.create(System.Console.Out, System.Console.Error)

module LiterateTesting =
  open System
  open LiterateConsole
  module TestTheme =
      let textColours =     { foreground=ConsoleColor.White; background=None }
      let subtextColours =  { foreground=ConsoleColor.Gray; background=None }
      let punctuationColours = { foreground=ConsoleColor.DarkGray; background=None }
      let levelVerboseColours = { foreground=ConsoleColor.Gray; background=None }
      let levelDebugColours = { foreground=ConsoleColor.Gray; background=None }
      let levelInfoColours = { foreground=ConsoleColor.White; background=None }
      let levelWarningColours = { foreground=ConsoleColor.Yellow; background=None }
      let levelErrorColours = { foreground=ConsoleColor.White; background=Some ConsoleColor.Red }
      let levelFatalColours = { foreground=ConsoleColor.White; background=Some ConsoleColor.Red }
      let keywordSymbolColours = { foreground=ConsoleColor.Blue; background=None }
      let numericSymbolColours = { foreground=ConsoleColor.Magenta; background=None }
      let stringSymbolColours = { foreground=ConsoleColor.Cyan; background=None }
      let otherSymbolColours = { foreground=ConsoleColor.Green; background=None }
      let nameSymbolColours = { foreground=ConsoleColor.Gray; background=None }
      let missingTemplateFieldColours = { foreground=ConsoleColor.Red; background=None }
      let theme = function
        | Tokens.Text -> textColours | Tokens.Subtext -> subtextColours | Tokens.Punctuation -> punctuationColours
        | Tokens.LevelVerbose -> levelVerboseColours | Tokens.LevelDebug -> levelDebugColours
        | Tokens.LevelInfo -> levelInfoColours | Tokens.LevelWarning -> levelWarningColours
        | Tokens.LevelError -> levelErrorColours | Tokens.LevelFatal -> levelFatalColours
        | Tokens.KeywordSymbol -> keywordSymbolColours | Tokens.NumericSymbol -> numericSymbolColours
        | Tokens.StringSymbol -> stringSymbolColours | Tokens.OtherSymbol -> otherSymbolColours
        | Tokens.NameSymbol -> nameSymbolColours | Tokens.MissingTemplateField -> missingTemplateFieldColours

  let levelD, levelE, levelF, levelI, levelV, levelW = "D", "E", "F", "I", "V", "W"
  let singleLetterLogLevelText = function
    | Debug -> levelD | Error -> levelE | Fatal -> levelF | Info -> levelI | Verbose -> levelV | Warn -> levelW

  let frenchFormatProvider = System.Globalization.CultureInfo("fr-FR") :> IFormatProvider

  /// creates a LiterateConsoleConf for testing purposes, and also returns a function that,
  /// when invoked, returns the coloured text parts written to the target.
  let createInspectableWrittenPartsConf () =
    let writtenParts = ResizeArray<LiterateConsole.ColouredText>()
    // replace everything except for tokenize
    { LiterateConsole.empty with
        formatProvider = frenchFormatProvider
        getLogLevelText = singleLetterLogLevelText
        theme = TestTheme.theme
        colourWriter = (fun sem parts -> writtenParts.AddRange(parts)) },
    fun () -> writtenParts |> List.ofSeq

[<Tests>]
let tests =
  testList "CoreTargets" [
    Targets.basicTests "text writer" (TextWriter.create textWriterConf)
    Targets.integrationTests "text writer" (TextWriter.create textWriterConf)

    Targets.basicTests "literate console" (LiterateConsole.create LiterateConsole.empty)
    Targets.integrationTests "literate console" (LiterateConsole.create LiterateConsole.empty)

    testList "literate console" [
      testCase "printing Hello World" <| fun _ ->
        let conf, getWrittenParts = LiterateTesting.createInspectableWrittenPartsConf()
        let target = LiterateConsole.create conf "testLC"
        let instance = target |> Target.init emptyRuntime |> run
        instance.server (fun _ -> Job.result ()) None
        |> start
        
        (because "logging with info level and then finalising the target" <| fun () ->
          Message.eventInfo "Hello World!" |> Message.setTicksEpoch 0L |> Target.logAndWait instance
          Target.finalise instance
          getWrittenParts()
        )
        |> should equal [ {text = "[";                    colours = LiterateTesting.TestTheme.punctuationColours }
                          {text = "10:00:00";             colours = LiterateTesting.TestTheme.subtextColours }
                          {text = " ";                    colours = LiterateTesting.TestTheme.subtextColours }
                          {text = LiterateTesting.levelI; colours = LiterateTesting.TestTheme.levelInfoColours }
                          {text = "] ";                   colours = LiterateTesting.TestTheme.punctuationColours }
                          {text = "Hello World!";         colours = LiterateTesting.TestTheme.textColours } ]
        |> thatsIt
    ]

    testList "text writer" [
      testCase "printing Hello World" <| fun _ ->
        let stdout = Fac.textWriter ()
        let target = TextWriter.create (TextWriter.TextWriterConf.create(stdout, stdout)) "writing console target"
        let instance = target |> Target.init emptyRuntime |> run
        instance.server (fun _ -> Job.result ()) None
        |> start

        (because "logging with info level and then finalising the target" <| fun () ->
          Message.eventInfo "Hello World!" |> Target.logAndWait instance
          Target.finalise instance
          stdout.ToString())
        |> should contain "Hello World!"
        |> thatsIt

      testCase "initialising TextWriter target" <| fun _ ->
        let stdout = Fac.textWriter ()
        let target = TextWriter.create (TextWriter.TextWriterConf.create(stdout, stdout)) "writing console target"
        let instance = target |> Target.init emptyRuntime |> run
        instance.server (fun _ -> Job.result ()) None
        |> start

        let x = dict ["foo", "bar"]
        (because "logging with fields then finalising the target" <| fun () ->
          Message.event Info "Hello World!" |> Message.setFieldFromObject "the Name" x |> Target.logAndWait instance
          Target.finalise instance
          stdout.ToString())
        |> should contain "Hello World!"
        |> should contain "foo"
        |> should contain "bar"
        |> should contain "the Name"
        |> thatsIt

      testCase "``error levels should be to error text writer``" <| fun _ ->
        let out, err = Fac.textWriter (), Fac.textWriter ()
        let target = TextWriter.create (TextWriter.TextWriterConf.create(out, err)) "error writing"
        let subject = target |> Target.init emptyRuntime |> run
        subject.server (fun _ -> Job.result ()) None
        |> start

        (because "logging 'Error line' and 'Fatal line' to the target" <| fun () ->
          Message.eventError "Error line" |> Target.logAndWait subject
          Message.eventFatal "Fatal line" |> Target.logAndWait subject
          Target.finalise subject
          err.ToString())
        |> should contain "Error line"
        |> should contain "Fatal line"
        |> thatsIt
      ]
    ]
