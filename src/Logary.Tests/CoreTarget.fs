module Logary.Tests.CoreTargets

open System
open System.Text.RegularExpressions
open System.Globalization
open System.IO
open System.Threading
open Expecto
open Expecto
open Logary
open Logary.Internals
open Logary.Targets
open Hopac
open Hopac.Infixes
open NodaTime

let logger = Expecto.Logging.Log.create "CoreTargets"

module LiterateTesting =
  open System
  open LiterateConsole
  open Logary.Formatting
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

  let formatLocalTime provider (ens: EpochNanoSeconds) =
    let dto = DateTimeOffset.ofEpoch ens
    dto.LocalDateTime.ToString("HH:mm:ss", provider),
    Tokens.Subtext

  /// creates a LiterateConsoleConf for testing purposes, and also returns a function that,
  /// when invoked, returns the coloured text parts written to the target.
  let createInspectableWrittenPartsConf () =
    let writtenParts = ResizeArray<LiterateConsole.ColouredText>()
    // replace everything except for tokenize
    { LiterateConsole.empty with
        tokenise        = Tokenisers.extendedTokeniser
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

open Logary.Tests.Utils

module Files =

  open FileSystem
  open File

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
      |> List.tryHead

    let tempPath =
      if tempPath.IsNone then Environment.CurrentDirectory
      else tempPath.Value

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

  let testCaseF testName (fn: FolderPath -> FileName -> Job<_>) =
    let folderPath, fileName = rndFolderFile testName
    testCaseJob testName (job {
      ensureFolder folderPath
      try
        do! fn folderPath fileName
      finally
        aFewTimes false <| fun _ -> Directory.Delete(folderPath, true)
    })

  let runtime (now: Instant) =
    RuntimeInfo.create "my service" "myHost"
    |> RuntimeInfo.setGetTimestamp (fun _ -> now.ToUnixTimeTicks() * Constants.NanosPerTick)

  let runtime2016_10_11T13_14_15 =
    runtime (Instant.FromUtc (2016, 10, 11, 13, 14, 15))

  let tests = [
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
          yield testCase ("combo host and service and datetime: "+ invalid) <| fun _ ->
            let naming = Naming (invalid, "log")
            // comment in to see error messages in output
            //naming.format runtime2016_10_11T13_14_15 |> ignore
            Expect.throws (fun () ->
              naming.format runtime2016_10_11T13_14_15 |> ignore)
              "Should throw due to bad input"
      ]
    ]

    // Commented out while file target is in dev
    // basicTests "file" createInRandom

    testCaseF "log ten thousand messages" <| fun folder file ->
      job {
        Tests.skiptest "Locks up, see https://github.com/haf/expecto/issues/2 but probably due to the Janitor loop"

        let! ri, _ = emptyRuntime
        let fileConf = FileConf.create folder (Naming ("10K", "log"))
        let! targetApi = Target.create ri  (File.create fileConf "basic2")
        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          let acks = ResizeArray<_>(10000)
          for i in 1 .. 10000 do
            let msg = Message.eventFormat (Info, "Event {number}", [| i |])
            let ack = logAndWait msg
            acks.Add ack
          Job.conIgnore acks)
      }
  ]

let tests = [
  TargetBaseline.basicTests "text writer" (fun name ->
    let _, _, twTargetConf = Utils.buildTextWriteTarget name
    twTargetConf) false

  TargetBaseline.basicTests "System.Diagnostics.Trace" (DiagnosticsTrace.create DiagnosticsTrace.empty) false

  testCase "convert DateTimeOffset with timespan delta" <| fun _ ->
    let a = DateTimeOffset(2016, 07, 02, 12, 33, 56, TimeSpan.FromHours(2.0))
    let ts = a.timestamp
    let b = ts |> DateTimeOffset.ofEpoch
    Expect.equal a.Ticks b.Ticks "Should be convertible to timestamp and back, including accounting for TimeSpans"

  testCase "formatLocalTime" <| fun _ ->
    let actual, _ =
      let fp: IFormatProvider = upcast CultureInfo "sv-SE"
      LiterateConsole.empty.formatLocalTime
        fp (DateTimeOffset(2016, 10, 25, 11, 17, 41, TimeSpan(2,0,0)).timestamp)
    let expected = "13:17:41"
    Expect.isTrue (Regex.IsMatch(actual, @"\d{2}:\d{2}:\d{2}"))
      (sprintf "Should print the time without time zone, was: %s" actual)

  testList "literate console" [
    let testLiterateCase testMsg messageFactory cb =
      let conf, getWrittenParts = LiterateTesting.createInspectableWrittenPartsConf ()
      let message = messageFactory Info

      testCaseJob testMsg (job {
        // in context
        let! ri, _ = emptyRuntime
        let! targetApi = LiterateConsole.create conf "testLC" |> Target.create ri

        // because
        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait message >>- fun _ ->
          let written, expectedTimeText =
            getWrittenParts (),
            fst (LiterateTesting.formatLocalTime conf.formatProvider message.timestamp)
          cb expectedTimeText written)
      })

    yield testLiterateCase "printing 'Hello World!'" helloWorld <| fun expectedTimeText parts ->
      Expect.sequenceEqual
        parts
        [ yield! levels expectedTimeText
          yield { text = "Hello World!"; colours = LiterateTesting.Theme.textColours }
          yield { text = " "; colours = LiterateTesting.Theme.subtextColours }
          yield { text = "<"; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = ""; colours = LiterateTesting.Theme.subtextColours }
          yield { text = ">"; colours = LiterateTesting.Theme.punctuationColours }
        ]
        "Should log with info level"

    yield testLiterateCase "Time in ms" (timeMessage 60029379L) <| fun expectedTimeText parts ->
      Expect.sequenceEqual
        parts
        [ yield! levels expectedTimeText
          yield { text = "Gauges: "; colours = LiterateTesting.Theme.textColours }
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
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  gauges:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "Check"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"60.029379 ms\""; colours = LiterateTesting.Theme.stringSymbolColours }
        ]
        "Should print [06:15:02 INF] A.B.C.Check took 60,02 ms"

    yield testLiterateCase "Time in μs" (timeMessage 133379L) <| fun expectedTimeText parts ->
      Expect.sequenceEqual
        parts
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
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  gauges:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "Check"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"133.379 µs\""; colours = LiterateTesting.Theme.stringSymbolColours }
                 ]
        "Should print [06:15:02 INF] A.B.C.Perform took 133,38 μs"

    yield testLiterateCase "Time in ns" (timeMessage 139L) <| fun expectedTimeText parts ->
      Expect.sequenceEqual
        parts
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
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  gauges:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "Check"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"139 ns\""; colours = LiterateTesting.Theme.stringSymbolColours }
          ]
        "Should print [06:15:02 INF] Check took 139 μs, because nanoseconds is as accurate as it gets"

    yield testLiterateCase "single measurement per second gauge" (gaugeMessage 1.4562) <| fun expectedTimeText parts ->
      Expect.sequenceEqual
        parts
        [ yield! levels expectedTimeText
          yield { text = "Gauges: "; colours = LiterateTesting.Theme.textColours }
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
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "  gauges:"; colours = LiterateTesting.Theme.textColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "spin"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"1.4562 s/revolution\""; colours = LiterateTesting.Theme.stringSymbolColours }
                   ]
        "Should print '[06:15:02 INF] Gauges: [spin: 1,4562 s/revolution] <Revolver>'"

    yield testLiterateCase "Multiple measurements per second gauge" multiGaugeMessage <| fun expectedTimeText parts ->
      //LiterateConsole.empty.colourWriter (Global.getConsoleSemaphore ()) (upcast parts)
      //printfn "Parts: %s" (parts |> List.map (fun ct -> ct.text) |> String.Concat)
      Expect.sequenceEqual
        parts
        [ yield! levels expectedTimeText
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
          yield { text = "host"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"db-001\""; colours = LiterateTesting.Theme.stringSymbolColours }
          yield { text = Environment.NewLine; colours = LiterateTesting.Theme.textColours }
          yield { text = "    "; colours = LiterateTesting.Theme.textColours }
          yield { text = "service"; colours = LiterateTesting.Theme.subtextColours }
          yield { text = " => "; colours = LiterateTesting.Theme.punctuationColours }
          yield { text = "\"api-web\""; colours = LiterateTesting.Theme.stringSymbolColours }
        ]
        // "Should print [06:15:02 INF] Processor.% Idle (M): Core 1=0,10 % | Core 2=99,00 % | Core 3=47,32 %"
        "Should print [06:15:02 INF] Processor.% Idle Gauges: [Core 1: 0,10 %, Core 2: 99,00 %, Core 3: 47,32 %"
  ]

  testList "text writer prints" [
    testCaseJob "message" <| (job {
      let out, error, conf = Utils.buildTextWriteTarget "writing console target"
      let! ri, _ = emptyRuntime
      let! targetApi = Target.create ri conf

      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
        logAndWait (Message.eventInfo "Hello World!" ) >>- fun _ ->
        Expect.stringContains (string out) "Hello World!" "logging with info level and then finalising the target")
    })

    testCaseJob "fields" <| (job {
      let out, error, conf = Utils.buildTextWriteTarget "writing console target"
      let! ri, _ = emptyRuntime
      let! targetApi = Target.create ri conf

      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
        let x = dict ["foo", "bar"]
        let message =
          Message.event Info "textwriter-test-init"
          |> Message.setContext "the Name" x
        logAndWait message >>- fun _ ->
        Expect.stringContains (string out) "textwriter-test-init" "logging with fields then finalising the target"
        Expect.stringContains (string out) "foo" "logging with fields then finalising the target"
        Expect.stringContains (string out) "bar" "logging with fields then finalising the target"
        Expect.stringContains (string out) "the Name" "logging with fields then finalising the target")
    })

    testCaseJob "to correct stream" <| (job {
      let out, error, conf = Utils.buildTextWriteTarget "writing console target"
      let! ri, _ = emptyRuntime
      let! targetApi = Target.create ri conf

      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
        logAndWait (Message.eventError "Error line") >>= fun _ ->
        logAndWait (Message.eventFatal "Fatal line") >>- fun _ ->
        let errorStr = string error
        Expect.stringContains errorStr "Error line" "logging 'Error line' and 'Fatal line' to the target"
        Expect.stringContains errorStr "Fatal line" "logging 'Error line' and 'Fatal line' to the target")
    })
  ]

  testList "files" Files.tests
]

