module Logary.Tests.CoreTargets

open System
open System.Text.RegularExpressions
open System.Globalization
open System.IO
open System.Text
open System.Threading
open Expecto
open Logary
open Logary.Targets
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

module FileSystem =

  /// A folder path is a fully qualified path to a directory/folder; it's
  /// should not be interpreted as a file and it should not just contain
  /// a non-rooted path, i.e. it should be a rooted path.
  type FolderPath = string

  /// A file name does not contain a path/folder structure as part of its
  /// contents. It's purely the name of the file and its extension (which is)
  /// part of the file name.
  type FileName = string

  /// A file path is a fully qualified name of a file, in that it contains the
  /// parent folders that the file is housed in.
  type FilePath = string

  type FileSystem =
    abstract getFile : FilePath -> FileInfo
    abstract getFolder : FolderPath -> DirectoryInfo

  /// This file system implementation contains the necessary code to use the
  /// .Net System.IO abstractions.
  [<Sealed>]
  type DotNetFileSystem() =
    interface FileSystem with
      member x.getFile path = FileInfo path
      member x.getFolder path = DirectoryInfo path

/// This is the File target of Logary. It can be both a rolling file and a
/// "normal", non-rolling file target, depending on the deletion and rotation
/// policies supplied.
module File =

  /// Bytes
  let B (n : uint64) = n
  /// Kilobytes
  let KiB n = n * 1024UL * 1024UL
  /// Megabytes
  let MiB n = n * 1024UL * 1024UL * 1024UL
  /// Gigabytes
  let GiB n = n * 1024UL * 1024UL * 1024UL * 1024UL

  /// These are the available rotation policies
  type RotationPolicy =
    /// The scheduled rotation policy is called initially at start and then at
    /// every Instant, after the rotation has been made. In the callback, the
    /// programmer can choose to read the file attributes. Reading the files'
    /// attributes (such as last-modified) may have side-effects on Windows,
    /// such as flushing the page cache to disk and thereby fsyncing, so if you
    /// end up scheduling flushes often (less than every second or so), then
    /// consider to let your callback remember that last lookup of the file's
    /// attribute(s).
    | Scheduled of scheduler:(FileInfo -> Instant)
    /// Rotate every given file size.
    | FileSize of maxSize:uint64
    | FileAge of age:Duration

  /// This module contains some rotation policies you may like to use by
  /// default.
  module RotationPolicies =
    /// This module contains rotation policies that rotate the files after
    /// specific periods.
    module ByAge =
      let everyMinute = FileAge (Duration.FromMinutes 1L)
      let every10minutes = FileAge (Duration.FromMinutes 10L)
      let hourly = FileAge (Duration.FromHours 1L)
      let daily = FileAge (Duration.FromHours 24L)
      let weekly = FileAge (Duration.FromHours (7L * 24L))

    /// This module contains rotation policies that specify that files should
    /// be rotated every nth byte (e.g. every 100 MiB).
    module BySize =
      let everyMegabyte = FileSize (MiB 1UL)
      let everyNthMegabyte n = FileSize (MiB n)
      let everyHundredMegabytes = everyNthMegabyte 100UL
      let everyGigabyte = FileSize (GiB 1UL)

    /// Implements a custom rotation policy signified by the passed callback
    /// function.
    let custom scheduler =
      Scheduled scheduler

  /// This is the result output of the deletion policy. Use these cases
  /// to decide the fate of the log file.
  type DeletionPolicyResult =
    /// The file should be deleted right away.
    | DeleteFile
    /// The file should be kept for now.
    | KeepFile

  /// A deletion policy gets the directory info that the logs are being stored
  /// in, the file info and should return a decision of whether to delete or
  /// keep the file.
  type DeletionPolicy = DirectoryInfo * FileInfo -> DeletionPolicyResult

  /// This module contains some pre-made deletion policies that serve to remove
  /// files from the logging folder, as to keep the disk-space of your disks
  /// free(-ish!).
  module DeletionPolicies =
    /// This deletion policy specifies that there are to be max n number of
    /// files present in the folder.
    let maxNoOfFiles (n : uint16) : DeletionPolicy =
      fun (dirInfo, fileInfo) ->
        // TODO: implement
        KeepFile

    /// Deletes all log files (but not the current non-rotated one) older than
    /// the given duration.
    let olderThan (dur : Duration) : DeletionPolicy =
      fun (dirInfo, fileInfo) ->
        // TODO: implement
        KeepFile

    /// Deletes files in oldest-first order when the folder size has passed a
    /// given bytes-large threshold. Be aware that you should not use this
    /// policy alone when there are other services with the same policy (alone)
    /// logging to the same folder.
    let folderSize (bytesSize : uint64) : DeletionPolicy =
      fun (dirInfo, fileInfo) ->
        // TODO: implement
        KeepFile

  /// The discriminated union that specifies whether to do file rotation in the
  /// File target.
  [<RequireQualifiedAccess>]
  type Rotation =
    /// Don't rotate the (single) log file, instead, keep appending to it.
    | SingleFile
    /// The rotation policy decides when it's time to rotate the file log. It's a
    /// list so that the user can supply multiple policies that all act on a
    /// first-come-first-served basis.
    /// The deletion policy decides when a given file should be deleted.
    /// You have to supply a non-empty rotation policy list, but you can supply
    /// an empty deletion policy.
    | Rotate of rotation:RotationPolicy list * deletion:DeletionPolicy list

  /// A module of oh-so-sweet file rolling and retention/deletion policies. Use
  /// these and sleep well at night.
  module Policies =
    open DeletionPolicies

    /// Rotates your log files when each file has reached 200 MiB.
    /// Lets your (for your service; single-purpose) logging folder grow to
    /// 2 GiB or letting your log files remain in the folder for two weeks
    /// tops.
    let rotate200MiB_deleteTwoGigsOrTwoWeeks =
      let rotate, delete =
        [ FileSize (MiB 200UL) ],
        [ folderSize (GiB 2UL)
          olderThan (Duration.FromHours (14L * 24L)) ]
      Rotation.Rotate (rotate, delete)

    /// Rotates your log files when each file has reached 200 MiB.
    /// Lets your (for your service; single-purpose) logging folder grow to
    /// 2 GiB.
    let rotate200MiB_deleteTwoGigs =
      let rotate, delete =
        [ FileSize (MiB 200UL) ],
        [ folderSize (GiB 2UL) ]
      Rotation.Rotate (rotate, delete)

    /// Rotates your log files when each file has reached 200 MiB.
    /// Never deletes your log files. Remember to put other sorts of monitoring
    /// in place, e.g. by using Logary's health checks.
    let rotate200MiB_neverDelete =
      let rotate, delete =
        [ FileSize (MiB 200UL) ],
        []
      Rotation.Rotate (rotate, delete)

  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Target
  open Logary.Internals

  /// The file target configuration record. This is used to customise the behaviour
  /// of the file target.
  type FileConf =
      /// Whether to buffer the string writer in this process' memory. Defaults to false,
      /// so that the textwriter that writes to the underlying file stream is continuously
      /// flushed.
    { buffer : bool
      /// Whether to force the operating system's page cache to flush to persistent
      /// storage for each log message. By default this is false, but sending a
      /// Flush message to the target forces the page cache to be flushed to disk
      /// (and of course will also force-flush the in-process buffer if needed).
      flushToDisk : bool
      /// Use the `Rotation` discriminated union to specify the policy for
      /// when to rotate the file. If you rotate the files, you can optionally
      /// choose to let this target delete files, too, by supplying a list of
      /// deletion policies.
      policies : Rotation
      /// The file system abstraction to write to.
      fileSystem : FileSystem.FileSystem }

  /// The empty/default configuration for the Logary file target.
  let empty =
    { buffer      = true
      flushToDisk = false
      policies    = Policies.rotate200MiB_deleteTwoGigsOrTwoWeeks
      fileSystem  = FileSystem.DotNetFileSystem() }

  module internal Impl =
    type State = { state : bool }

    let loop (conf : FileConf)
             (ri : RuntimeInfo)
             (requests : RingBuffer<_>)
             (shutdown : Ch<_>)
             (saveWill : obj -> Job<unit>)
             (lastWill : obj option) =

      let rec loop (state : State) : Job<unit> =
        Alt.choose [
          shutdown ^=> fun ack ->
            // do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
            ack *<= () :> Job<_>

          RingBuffer.take requests ^=> function
            | Log (message, ack) ->
              job {
                do! ack *<= ()
                return! loop { state = not state.state }
              }
            | Flush (ackCh, nack) ->
              job {
                do! Ch.give ackCh () <|> nack
                return! loop { state = not state.state }
              }
        ] :> Job<_>

      // start the inner loop by the exit of the outer loop function
      loop { state = false }

  /// Create a new File target through this.
  [<CompiledName "Create">]
  let create conf name =
    TargetUtils.willAwareNamedTarget (Impl.loop conf) name

open FileSystem

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

  let testCase testName (fn : FolderPath -> FileName -> Job<_>) =
    let tempPath =
      // TMPDIR is on OS X
      [ "TMP"; "TEMP"; "TMPDIR" ]
      |> List.map env
      |> List.filter (isNull >> not)
      |> List.head
    let subFolder = sha1 testName
    let folderPath = Path.Combine(tempPath, subFolder)
    let fileName = rndName ()
    let filePath = Path.Combine(folderPath, fileName)

    testCase testName <| fun _ ->
      if not (Directory.Exists folderPath) then
        Directory.CreateDirectory(folderPath) |> ignore

      try
        fn folderPath fileName |> run
      finally
        Directory.Delete(folderPath, true)

  testList "files" [
    testList "rotate policies" [
      testCase "file size" <| fun _ _ ->
        Tests.skiptest "\
          The rotation policies decide when a given file is due to be rotated,\n\
          i.e. closed, renamed to a 'historical name' and then re-opened."

      testCase "file age" <| fun _ _ ->
        Tests.skiptest "\
          This rotation should rotate the file when the current file has\n\
          reached a certain age."

      testCase "custom callback policy unit -> Instant" <| fun _ _ ->
        Tests.skiptest "\
          This retention policy should be continuously called by the Logary\n\
          scheduler which is then told when the rotation should happen. This also\n\
          covers the case when developers want to rotate on starting the system."
    ]

    testList "deletion policies" [
      testCase "file count in folder" <| fun _ _ ->
        Tests.skiptest "\
          The deletion policies should work by counting the number of files\n\
          that match the given pattern, and delete older files that count above\n\
          a specified threshold, excluding the current file."

      testCase "folder total size" <| fun _ _ ->
        Tests.skiptest "\
          This deletion policy should count the total size of the folder and\n\
          if it's above a certain threshold, start deleting files until that threshold\n\
          is reached, excluding the current file."

      testCase "file age" <| fun _ _ ->
        Tests.skiptest "\
          This deletion policy should kick in to delete files that are above a\n\
          specified age, excluding the current file."
    ]

    testList "file target" [
      Targets.basicTests "file" (fun name -> File.create File.empty name)
      Targets.integrationTests "file" (fun name -> File.create File.empty name)

      testCase "can create" <| fun folderPath fileName ->
        //File.create (FileConf.create folderPath fileName)
        ()
    ]

    testList "rolling file target" [
    ]
  ]
