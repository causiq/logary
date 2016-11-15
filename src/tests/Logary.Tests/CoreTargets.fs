module Logary.Tests.CoreTargets

open System
open System.Text.RegularExpressions
open System.Globalization
open System.IO
open System.Text
open System.Threading
open Expecto
open Logary
open Logary.Internals
open Logary.Targets
open Logary.Message
open Logary.Tests.Targets
open Hopac
open NodaTime
open TestDSL
open Fac

let textWriterConf () =
  let stdout, stderr = Fac.textWriter (), Fac.textWriter ()
  TextWriter.TextWriterConf.create(stdout, stderr)

module LiterateTesting =
  open System
  open LiterateConsole
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

  let formatLocalTime provider (ens : EpochNanoSeconds) =
    let dto = DateTimeOffset.ofEpoch ens
    dto.LocalDateTime.ToString("HH:mm:ss", provider),
    Tokens.Subtext

  /// creates a LiterateConsoleConf for testing purposes, and also returns a function that,
  /// when invoked, returns the coloured text parts written to the target.
  let createInspectableWrittenPartsConf () =
    let writtenParts = ResizeArray<LiterateConsole.ColouredText>()
    // replace everything except for tokenize
    { LiterateConsole.empty with
        formatProvider  = frenchFormatProvider
        getLogLevelText = singleLetterLogLevelText
        formatLocalTime = formatLocalTime
        theme           = Theme.theme
        colourWriter    = fun sem parts -> writtenParts.AddRange(parts) },
    fun () -> writtenParts |> List.ofSeq

let timeMessage (nanos : int64) level =
  let value, units = Int64 nanos, Scaled (Seconds, float Constants.NanosPerSecond)
  snd (Message.time (PointName [| "A"; "B"; "C"; "Check" |]) (fun () -> 32) ())
  |> Message.setGauge (value, units)
  |> Message.setLevel level

let nanos xs =
  Duration.FromTicks (xs / Constants.NanosPerTick)

let helloWorldMsg =
  Message.eventX "Hello World!"
  >> Message.setTicksEpoch (0L : EpochNanoSeconds)

let levels expectedTimeText : LiterateConsole.ColouredText list =
  [ { text = "[";                    colours = LiterateTesting.Theme.punctuationColours }
    { text = expectedTimeText;       colours = LiterateTesting.Theme.subtextColours }
    { text = " ";                    colours = LiterateTesting.Theme.subtextColours }
    { text = LiterateTesting.levelI; colours = LiterateTesting.Theme.levelInfoColours }
    { text = "] ";                   colours = LiterateTesting.Theme.punctuationColours } ]

[<Tests>]
let tests =
  testList "CoreTargets" [
    Targets.basicTests "text writer" (fun name -> TextWriter.create (textWriterConf ()) name)
    Targets.integrationTests "text writer" (fun name -> TextWriter.create (textWriterConf ()) name)

    // TODO: don't want to actually print these
    //Targets.basicTests "literate console" (LiterateConsole.create LiterateConsole.empty)
    //Targets.integrationTests "literate console" (LiterateConsole.create LiterateConsole.empty)

    testCase "convert DateTimeOffset with timespan delta" <| fun _ ->
      let a = DateTimeOffset(2016, 07, 02, 12, 33, 56, TimeSpan.FromHours(2.0))
      let ts = a.timestamp
      let b = ts |> DateTimeOffset.ofEpoch
      Expect.equal a.Ticks b.Ticks "Should be convertible to timestamp and back, including accounting for TimeSpans"

    testCase "formatLocalTime" <| fun _ ->
      let actual, _ =
        let fp : IFormatProvider = upcast CultureInfo "sv-SE"
        LiterateConsole.empty.formatLocalTime
          fp (DateTimeOffset(2016, 10, 25, 11, 17, 41, TimeSpan(2,0,0)).timestamp)
      let expected = "13:17:41"
      Expect.isTrue (Regex.IsMatch(actual, @"\d{2}:\d{2}:\d{2}"))
        (sprintf "Should print the time without time zone, was: %s" actual)

    testList "literate console" [
      let testLiterateCase testMsg messageFactory cb =
        let conf, getWrittenParts = LiterateTesting.createInspectableWrittenPartsConf ()
        let message = messageFactory Info

        testCase testMsg <| fun () ->
          // in context
          let instance =
            LiterateConsole.create conf "testLC"
            |> Target.init emptyRuntime
            |> run

          start (instance.server (fun _ -> Job.result ()) None)

          // because
          message |> Target.logAndWait instance

          let written, expectedTimeText =
            getWrittenParts (),
            fst (LiterateTesting.formatLocalTime conf.formatProvider message.timestamp)

          // then
          try cb expectedTimeText written
          finally Target.finalise instance

      yield testLiterateCase "printing 'Hello World!'" helloWorldMsg <| fun expectedTimeText parts ->
        Expect.equal parts
                     [ yield! levels expectedTimeText
                       yield {text = "Hello World!"; colours = LiterateTesting.Theme.textColours } ]
                     "logging with info level and then finalising the target"

      yield testLiterateCase "Time in ms" (timeMessage 60029379L) <| fun expectedTimeText parts ->
        Expect.sequenceEqual
          parts
          [ yield! levels expectedTimeText
            yield { text = "A.B.C.Check"; colours = LiterateTesting.Theme.nameSymbolColours }
            yield { text = " took "; colours = LiterateTesting.Theme.subtextColours }
            yield { text = "60,03"; colours = LiterateTesting.Theme.numericSymbolColours }
            yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
            yield { text = "ms"; colours = LiterateTesting.Theme.textColours }
            yield { text = " to execute."; colours = LiterateTesting.Theme.subtextColours } ]
          "Should print [06:15:02 DBG] A.B.C.Check took 60,02 ms"

      yield testLiterateCase "Time in μs" (timeMessage 133379L) <| fun expectedTimeText parts ->
        Expect.sequenceEqual
          parts
          [ yield! levels expectedTimeText
            yield { text = "A.B.C.Check"; colours = LiterateTesting.Theme.nameSymbolColours }
            yield { text = " took "; colours = LiterateTesting.Theme.subtextColours }
            yield { text = "133,38"; colours = LiterateTesting.Theme.numericSymbolColours }
            yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
            yield { text = "µs"; colours = LiterateTesting.Theme.textColours }
            yield { text = " to execute."; colours = LiterateTesting.Theme.subtextColours } ]
          "Should print [06:15:02 DBG] A.B.C.Perform took 133,38 μs"

      yield testLiterateCase "Time in ns" (timeMessage 139L) <| fun expectedTimeText parts ->
        Expect.sequenceEqual
          parts
          [ yield! levels expectedTimeText
            yield { text = "A.B.C.Check"; colours = LiterateTesting.Theme.nameSymbolColours }
            yield { text = " took "; colours = LiterateTesting.Theme.subtextColours }
            yield { text = "139"; colours = LiterateTesting.Theme.numericSymbolColours }
            yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
            yield { text = "ns"; colours = LiterateTesting.Theme.textColours }
            yield { text = " to execute."; colours = LiterateTesting.Theme.subtextColours } ]
          "Should print [06:15:02 DBG] A.B.C.Perform took 139 μs, because nanoseconds is as accurate as it gets"
    ]

    testList "text writer prints" [
      testCase "message" <| fun _ ->
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

      testCase "fields" <| fun _ ->
        let stdout = Fac.textWriter ()
        let target = TextWriter.create (TextWriter.TextWriterConf.create(stdout, stdout)) "writing console target"
        let instance = target |> Target.init emptyRuntime |> run
        instance.server (fun _ -> Job.result ()) None
        |> start

        let x = dict ["foo", "bar"]
        (because "logging with fields then finalising the target" <| fun () ->
          Message.event Info "textwriter-test-init" |> Message.setFieldFromObject "the Name" x |> Target.logAndWait instance
          Target.finalise instance
          stdout.ToString())
        |> should contain "textwriter-test-init"
        |> should contain "foo"
        |> should contain "bar"
        |> should contain "the Name"
        |> thatsIt

      testCase "to correct stream" <| fun _ ->
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

open FileSystem
open File
open Expecto.Logging

let logger = Log.create "Logary.Targets.Tests.CoreTargets"

[<Tests>]
let files =
  let env = Environment.GetEnvironmentVariable
  let rnd = new ThreadLocal<Random>(fun () -> Random ())

  let sha1 =
    UTF8.bytes
    >> Bytes.hash Security.Cryptography.SHA1.Create
    >> Bytes.toHex

  let rndName () =
    let buf = Array.zeroCreate<byte> 16
    rnd.Value.NextBytes buf
    Bytes.toHex buf

  let rndFolderFile testName =
    let tempPath =
      // TMPDIR is on OS X and Linux
      [ "TMP"; "TEMP"; "TMPDIR" ]
      |> List.map env
      |> List.filter (isNull >> not)
      |> List.head
    let subFolder = sha1 testName
    let folderPath = Path.Combine(tempPath, subFolder)
    let fileName = rndName ()
    folderPath, fileName

  let ensureFolder folderPath =
    if not (Directory.Exists folderPath) then
      Directory.CreateDirectory(folderPath) |> ignore

  let aFewTimes throw fn =
    let rec inner count acc =
      try
        fn ()
      with e ->
        Thread.Sleep 150
        if count <= 10 then
          inner (count + 1) (e :: acc)
        else
          if throw then failwithf "Function %O failed with %A" fn (e :: acc)
          else ()
    inner 1 []

  let createInRandom testName =
    let folder, file = rndFolderFile (sprintf "file - %s" testName)
    ensureFolder folder
    let conf = FileConf.create folder (Naming (file, "log"))
    File.create conf testName

  let testCaseF testName (fn : FolderPath -> FileName -> Job<_>) =
    let folderPath, fileName = rndFolderFile testName
    testCase testName <| fun _ ->
      ensureFolder folderPath
      try
        fn folderPath fileName |> run
      finally
        aFewTimes false <| fun _ -> Directory.Delete(folderPath, true)

  let runtime now =
    RuntimeInfo.create (
      "my service",
      { new IClock with
          member x.Now = now },
      "myHost")

  let runtime2016_10_11T13_14_15 =
    runtime (Instant.FromUtc (2016, 10, 11, 13, 14, 15))

  testList "files" [
    testList "rotate policies" [
      testCaseF "file size" <| fun _ _ ->
        Tests.skiptest "\
          The rotation policies decide when a given file is due to be rotated,\n\
          i.e. closed, renamed to a 'historical name' and then re-opened."

      testCaseF "file age" <| fun _ _ ->
        Tests.skiptest "\
          This rotation should rotate the file when the current file has\n\
          reached a certain age."

      testCaseF "custom callback policy unit -> Instant" <| fun _ _ ->
        Tests.skiptest "\
          This retention policy should be continuously called by the Logary\n\
          scheduler which is then told when the rotation should happen. This also\n\
          covers the case when developers want to rotate on starting the system."
    ]

    testList "deletion policies" [
      testCaseF "file count in folder" <| fun _ _ ->
        Tests.skiptest "\
          The deletion policies should work by counting the number of files\n\
          that match the given pattern, and delete older files that count above\n\
          a specified threshold, excluding the current file."

      testCaseF "folder total size" <| fun _ _ ->
        Tests.skiptest "\
          This deletion policy should count the total size of the folder and\n\
          if it's above a certain threshold, start deleting files until that threshold\n\
          is reached, excluding the current file."

      testCaseF "file age" <| fun _ _ ->
        Tests.skiptest "\
          This deletion policy should kick in to delete files that are above a\n\
          specified age, excluding the current file."
    ]

    testList "naming policies" [
      testCase "service – from RuntimeInfo" <| fun _ ->
        let subject = Naming ("{service}", "log")
        let actual = subject.formatS runtime2016_10_11T13_14_15
        Expect.equal actual "my service.log" "Should format service"
        let regex = subject.regex runtime2016_10_11T13_14_15
        Expect.isTrue (regex.IsMatch actual) "Should match its own output"

      testCase "year-month-day - date" <| fun _ ->
        let subject = Naming ("{date}", "log")
        let actual = subject.formatS runtime2016_10_11T13_14_15
        Expect.equal actual "2016-10-11.log" "Should format date"
        let regex = subject.regex runtime2016_10_11T13_14_15
        Expect.isTrue (regex.IsMatch actual) "Should match its own output"

      testCase "year-month-dayThour-minutes-seconds – datetime" <| fun _ ->
        let subject = Naming ("{datetime}", "log")
        let actual = subject.formatS runtime2016_10_11T13_14_15
        Expect.equal actual "2016-10-11T13-14-15Z.log" "Should format datetime"
        let regex = subject.regex runtime2016_10_11T13_14_15
        Expect.isTrue (regex.IsMatch actual) "Should match its own output"

      testCase "host" <| fun _ ->
        let subject = Naming ("{host}", "log")
        let actual = subject.formatS runtime2016_10_11T13_14_15
        Expect.equal actual "myHost.log" "Should format host name"
        let regex = subject.regex runtime2016_10_11T13_14_15
        Expect.isTrue (regex.IsMatch actual) "Should match its own output"

      testCase "combo host and service and datetime (no sep)" <| fun _ ->
        let subject = Naming ("{host}{service}{datetime}", "log")
        let actual = subject.formatS runtime2016_10_11T13_14_15
        Expect.equal actual "myHostmy service2016-10-11T13-14-15Z.log"
                     "Should format composite file name"
        let regex = subject.regex runtime2016_10_11T13_14_15
        Expect.isTrue (regex.IsMatch actual) "Should match its own output"

      testCase "combo host and service and datetime" <| fun _ ->
        // ^[a-zA-Z0-9\s]+-[a-zA-Z0-9\s]+-\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}Z\.log$
        let subject = Naming ("{host}-{service}-{datetime}", "log")
        let actual = subject.formatS runtime2016_10_11T13_14_15
        Expect.equal actual "myHost-my service-2016-10-11T13-14-15Z.log"
                     "Should format composite file name"
        let regex = subject.regex runtime2016_10_11T13_14_15
        Expect.isTrue (regex.IsMatch actual)
                      (sprintf "Should match its own output '%s' with regex /%O/"
                               actual
                               regex)

      testList "invalids" [
        for invalid in [ "{host}-}{service}-{datetime}"
                         "apa:"
                         ""
                         "\\"
                         "/"
                         ";"] do
          yield testCase "combo host and service and datetime" <| fun _ ->
            let naming = Naming (invalid, "log")
            // comment in to see error messages in output
            //naming.format runtime2016_10_11T13_14_15 |> ignore
            Expect.throws (fun () ->
              naming.format runtime2016_10_11T13_14_15 |> ignore)
              "Should throw due to bad input"
      ]
    ]

    Targets.basicTests "file" createInRandom
    Targets.integrationTests "file" createInRandom

    testCaseF "log ten thousand messages" <| fun folder file ->
      //Tests.skiptest "Locks up, see https://github.com/haf/expecto/issues/2"
      let fileConf = FileConf.create folder (Naming ("10K", "log"))
      let targetConf = Target.confTarget "basic2" (File.create fileConf)

      Message.event Debug "Creating test case job." |> logger.logSimple
      job {
        let! instance = targetConf |> Target.init { Fac.emptyRuntime with logger = Fac.literal.Value }
        Message.event Debug "Starting target server." |> logger.logSimple
        do! Job.start (instance.server (fun _ -> Job.result ()) None)

        let acks = ResizeArray<_>(10000)
        Message.event Debug (sprintf "Starting writing messages into Logary at %s." folder) |> logger.logSimple
        for i in 1 .. 10000 do
          let! ack =
            event Logary.LogLevel.Info "Event {number}"
            |> setField "number" i
            |> Target.log instance
          acks.Add ack
        Message.event Debug "Waiting for ACKs" |> logger.logSimple
        do! Job.conIgnore acks
        Message.event Debug "All messages ACK-ed" |> logger.logSimple

        let! res = Target.finaliseJob instance
        match res with
        | Internals.TimedOut ->
          Tests.failtest "Stopping file target timed out"
        | _ ->
          ()
      }
  ]
