module Logary.Tests.LiterateConsole

open System
open Expecto
open Expecto.Flip
open Hopac
open Hopac.Infixes
open Logary
open Logary.Tests
open Logary.Targets
open Logary.Targets.LiterateConsole
open Logary.Targets.LiterateConsole.Tokenisers
open Logary.Formatting

let tokenisation =
  let failingFn (inner) =
    raise (exn ("Top level exn", inner))

  let throwAnotherExn inner =
    try failingFn inner
    with e -> e

  let throwAggrExn inner1 inner2 =
    try raise (AggregateException("Outer aggregate exception", inner1, inner2))
    with e -> e

  testList "tokenisation" [
    testCase "tokenise hierarchal exn" <| fun _ ->
      let exnInner = withException id
      let exnOuter = throwAnotherExn exnInner
      let expected =
        [ "-", MessageTemplates.Formatting.Literate.Subtext // newline
          "System.Exception: Top level exn", MessageTemplates.Formatting.Literate.Text
          "Logary.Tests.LiterateConsole.throwAnotherExn@19(Exception inner)", MessageTemplates.Formatting.Literate.Subtext
          "--- End of exception stack trace ---", MessageTemplates.Formatting.Literate.Punctuation
          "System.Exception: Bad things going on", MessageTemplates.Formatting.Literate.Text
          "Logary.Tests.Utils.innermost[a](Boolean throwCLRExn)", MessageTemplates.Formatting.Literate.Subtext
        ]
      Message.event Warn "General error" |> Message.addExn exnOuter
        |> Literate.tokeniseExceptions Culture.invariant "-"
        |> Expect.sequenceContainsOrder "Has correct output" expected

    testCase "tokenise Aggregate exn" <| fun _ ->
      let e1, e2 = withException id, withException id
      let exnOuter = throwAggrExn e1 e2
      let expected =
        [ "-", MessageTemplates.Formatting.Literate.Subtext // newline
          "System.AggregateException: Outer aggregate exception (Bad things going on) (Bad things going on)", MessageTemplates.Formatting.Literate.Text
          "Logary.Tests.LiterateConsole.throwAggrExn@23(Exception inner1, Exception inner2)", MessageTemplates.Formatting.Literate.Subtext
          "--- End of exception stack trace ---", MessageTemplates.Formatting.Literate.Punctuation
          "System.Exception: Bad things going on", MessageTemplates.Formatting.Literate.Text
          "Logary.Tests.Utils.innermost[a](Boolean throwCLRExn)", MessageTemplates.Formatting.Literate.Subtext
          "--- End of exception stack trace ---", MessageTemplates.Formatting.Literate.Punctuation
          "System.Exception: Bad things going on", MessageTemplates.Formatting.Literate.Text
          "Logary.Tests.Utils.innermost[a](Boolean throwCLRExn)", MessageTemplates.Formatting.Literate.Subtext
        ]
      Message.event Warn "My error" |> Message.addExn exnOuter
        |> Literate.tokeniseExceptions Culture.invariant "-"
        |> Expect.sequenceContainsOrder "Has correct output" expected
  ]

module LiterateTesting =

  module Theme =
    let textColours = { foreground=ConsoleColor.White; background=None }
    let subtextColours = { foreground=ConsoleColor.Gray; background=None }
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
      | Tokens.Text -> textColours
      | Tokens.Subtext -> subtextColours
      | Tokens.Punctuation -> punctuationColours
      | Tokens.LevelVerbose -> levelVerboseColours
      | Tokens.LevelDebug -> levelDebugColours
      | Tokens.LevelInfo -> levelInfoColours
      | Tokens.LevelWarning -> levelWarningColours
      | Tokens.LevelError -> levelErrorColours
      | Tokens.LevelFatal -> levelFatalColours
      | Tokens.KeywordSymbol -> keywordSymbolColours
      | Tokens.NumericSymbol -> numericSymbolColours
      | Tokens.StringSymbol -> stringSymbolColours
      | Tokens.OtherSymbol -> otherSymbolColours
      | Tokens.NameSymbol -> nameSymbolColours
      | Tokens.MissingTemplateField -> missingTemplateFieldColours

  let levelD, levelE, levelF, levelI, levelV, levelW = "D", "E", "F", "I", "V", "W"

  let singleLetterLogLevelText = function
    | Debug -> levelD
    | Error -> levelE
    | Fatal -> levelF
    | Info -> levelI
    | Verbose -> levelV
    | Warn -> levelW

  let frenchFormatProvider = System.Globalization.CultureInfo("fr-FR") :> IFormatProvider

  let formatLocalTime provider (ens: EpochNanoSeconds) =
    let dto = DateTimeOffset.ofEpoch ens
    dto.LocalDateTime.ToString("HH:mm:ss", provider),
    Tokens.Subtext

  /// creates a LiterateConsoleConf for testing purposes, and also returns a function that,
  /// when invoked, returns the coloured text parts written to the target.
  let createInspectableWrittenPartsConf (tokeniserSubject) =
    let writtenParts = ResizeArray<LiterateConsole.ColouredText>()
    // replace everything except for tokenize
    { LiterateConsole.empty with
        tokenise        = tokeniserSubject
        formatProvider  = frenchFormatProvider
        getLogLevelText = singleLetterLogLevelText
        formatLocalTime = formatLocalTime
        theme           = Theme.theme
        colourWriter    = fun sem parts -> writtenParts.AddRange(parts) },
    fun () -> writtenParts |> List.ofSeq

/// 5 values
let levels expectedTimeText: LiterateConsole.ColouredText list =
  [ { text = "[";                    colours = LiterateTesting.Theme.punctuationColours }
    { text = expectedTimeText;       colours = LiterateTesting.Theme.subtextColours }
    { text = " ";                    colours = LiterateTesting.Theme.subtextColours }
    { text = LiterateTesting.levelI; colours = LiterateTesting.Theme.levelInfoColours }
    { text = "] ";                   colours = LiterateTesting.Theme.punctuationColours } ]

let parts =
  testList "parts" [
    let testLiterateCase testMsg messageFactory tokeniser cb =
      let conf, getWrittenParts = LiterateTesting.createInspectableWrittenPartsConf tokeniser
      let message = messageFactory Info

      testCaseJob testMsg (job {
        // in context
        let! ri = emptyRuntime
        let! targetApi = LiterateConsole.create conf "testLC" |> Target.create ri

        // because
        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait message >>- fun _ ->
          let written, expectedTimeText =
            getWrittenParts (),
            fst (LiterateTesting.formatLocalTime conf.formatProvider message.timestamp)
          cb expectedTimeText written)
      })

    yield testLiterateCase "printing 'Hello World!'" helloWorld defaultTokeniser <| fun timeText parts ->
      let expected =
        [ yield! levels timeText
          yield { text = "Hello World!"; colours = LiterateTesting.Theme.textColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "<"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = ""; colours = LiterateTesting.Theme.subtextColours }
          yield { text = ">"; colours = LiterateTesting.Theme.punctuationColours }
        ]
      parts
        |> Expect.formattedEqual
            "Should print the right output text"
            (sprintf "[%s I] Hello World! <>" timeText)
      parts
        |> Expect.sequenceEqual "Should log with info level" expected

    let gaugeFirstLine =
      [ yield { text = "Gauges: "; colours = LiterateTesting.Theme.textColours }
        yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
        yield { text = "Check"; colours = LiterateTesting.Theme.nameSymbolColours }
        yield { text = " took "; colours = LiterateTesting.Theme.subtextColours }
        yield { text = "60,03"; colours = LiterateTesting.Theme.numericSymbolColours }
        yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
        yield { text = "ms"; colours = LiterateTesting.Theme.textColours }
        yield { text = " to execute"; colours = LiterateTesting.Theme.subtextColours }
        yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }
        yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
        yield { text = "<"; colours = LiterateTesting.Theme.punctuationColours }
        yield { text = "A.B.C"; colours = LiterateTesting.Theme.subtextColours }
        yield { text = ">"; colours = LiterateTesting.Theme.punctuationColours }
      ]

    let g = timeMessage 60029379L

    yield testLiterateCase "Time in ms" g defaultTokeniser <| fun timeText parts ->
      let expected =
        [ yield! levels timeText
          yield! gaugeFirstLine ]
      parts
        |> Expect.formattedEqual
            "Should print the right output text"
            (sprintf "[%s I] Gauges: [Check took 60,03 ms to execute] <A.B.C>" timeText)
      parts
        |> Expect.sequenceEqual "Should print [06:15:02 INF] A.B.C.Check took 60,02 ms" expected

    yield testLiterateCase "Time in ms (ext)" g extendedTokeniser <| fun timeText parts ->
      let expected =
        [ yield! levels timeText
          yield! gaugeFirstLine
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  gauges:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "Check"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"60.029379 ms\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  others:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "_logary.tags"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"gauge\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }
        ]
      parts
        |> Expect.sequenceEqual "Should print [06:15:02 INF] A.B.C.Check took 60,02 ms" expected

    yield testLiterateCase "Time in μs" (timeMessage 133379L) defaultTokeniser <| fun expectedTimeText parts ->
      let expected =
        [ yield! levels expectedTimeText
          yield { text = "Gauges: "; colours = LiterateTesting.Theme.textColours }
          yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "Check"; colours = LiterateTesting.Theme.nameSymbolColours }
          yield { text = " took "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "133,38"; colours = LiterateTesting.Theme.numericSymbolColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "µs"; colours = LiterateTesting.Theme.textColours }
          yield { text = " to execute"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "<"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "A.B.C"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = ">"; colours = LiterateTesting.Theme.punctuationColours }
        ]
      parts
        |> Expect.sequenceEqual "Should print [06:15:02 INF] A.B.C.Perform took 133,38 μs" expected

    yield testLiterateCase "Time in ns" (timeMessage 139L) defaultTokeniser <| fun expectedTimeText parts ->
      let expected =
        [ yield! levels expectedTimeText
          yield { text = "Gauges: "; colours = LiterateTesting.Theme.textColours }
          yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "Check"; colours = LiterateTesting.Theme.nameSymbolColours }
          yield { text = " took "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "139"; colours = LiterateTesting.Theme.numericSymbolColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "ns"; colours = LiterateTesting.Theme.textColours }
          yield { text = " to execute"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "<"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "A.B.C"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = ">"; colours = LiterateTesting.Theme.punctuationColours }
        ]
      parts
        |> Expect.sequenceEqual "Should print [06:15:02 INF] Check took 139 μs, because nanoseconds is as accurate as it gets" expected

    let g = gaugeMessage 1.4562

    let singleM =
      [ yield { text = "Gauges: "; colours = LiterateTesting.Theme.textColours }
        yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
        yield { text = "spin"; colours = LiterateTesting.Theme.nameSymbolColours }
        yield { text = ":"; colours = LiterateTesting.Theme.punctuationColours }
        yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
        yield { text = "1,4562"; colours = LiterateTesting.Theme.numericSymbolColours }
        yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
        yield { text = "s/revolution"; colours = LiterateTesting.Theme.textColours }
        yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }
        yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
        yield { text = "<"; colours = LiterateTesting.Theme.punctuationColours }
        yield { text = "Revolver"; colours = LiterateTesting.Theme.subtextColours }
        yield { text = ">"; colours = LiterateTesting.Theme.punctuationColours }
      ]

    yield testLiterateCase "single measurement per second gauge" g defaultTokeniser <| fun timeText parts ->
      let expected =
        [ yield! levels timeText
          yield! singleM ]
      parts
        |> Expect.formattedEqual
            "Should print the right output text"
            (sprintf "[%s I] Gauges: [spin: 1,4562 s/revolution] <Revolver>" timeText)
      parts
        |> Expect.sequenceEqual "Prints the tokens in the right order" expected


    yield testLiterateCase "single measurement per second gauge (ext)" g extendedTokeniser <| fun timeText parts ->
      let expected =
        [ yield! levels timeText
          yield! singleM
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  gauges:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "spin"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"1.4562 s/revolution\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  others:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "_logary.tags"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"gauge\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }
        ]
      parts
        |> Expect.sequenceEqual "Should print '[06:15:02 INF] Gauges: [spin: 1,4562 s/revolution] <Revolver>'" expected

    yield testLiterateCase "Multiple measurements per second gauge" multiGaugeMessage extendedTokeniser <| fun timeText parts ->
      //LiterateConsole.empty.colourWriter (Global.getConsoleSemaphore ()) (upcast parts)
      //printfn "Parts: %s" (parts |> List.map (fun ct -> ct.text) |> String.Concat)
      let expected =
        [ yield! levels timeText
          yield { text = "Processor.% Idle"; colours = LiterateTesting.Theme.textColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "Gauges: "; colours = LiterateTesting.Theme.textColours }
          yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "Core 1"; colours = LiterateTesting.Theme.nameSymbolColours }
          yield { text = ":"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "0,1"; colours = LiterateTesting.Theme.numericSymbolColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "%"; colours = LiterateTesting.Theme.textColours }
          yield { text = ", "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "Core 2"; colours = LiterateTesting.Theme.nameSymbolColours }
          yield { text = ":"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "99"; colours = LiterateTesting.Theme.numericSymbolColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "%"; colours = LiterateTesting.Theme.textColours }
          yield { text = ", "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "Core 3"; colours = LiterateTesting.Theme.nameSymbolColours }
          yield { text = ":"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "47,3223755"; colours = LiterateTesting.Theme.numericSymbolColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "%"; colours = LiterateTesting.Theme.textColours }
          yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }

          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "<"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = ""; colours = LiterateTesting.Theme.subtextColours }
          yield { text = ">"; colours = LiterateTesting.Theme.punctuationColours }


          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  gauges:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "Core 1"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"0.1 %\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "Core 2"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"99 %\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "Core 3"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"47.3223755 %\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  others:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "_logary.tags"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "["; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"gauge\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = "]"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "host"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"db-001\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "service"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"api-web\""; colours = LiterateTesting.Theme.stringSymbolColours }
        ]
      parts
        |> Expect.sequenceEqual
            "Should print [06:15:02 INF] Processor.% Idle Gauges: [Core 1: 0,10 %, Core 2: 99,00 %, Core 3: 47,32 %"
            expected
  ]

