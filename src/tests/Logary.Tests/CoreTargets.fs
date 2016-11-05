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

  /// A file-system abstraction
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

  module Path =
    let combine (segments : string seq) =
      IO.Path.Combine(segments |> Array.ofSeq)


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
    let ``rotate >200 MiB, delete if folder >2 GiB or file age >2 weeks`` =
      let rotate, delete =
        [ FileSize (MiB 200UL) ],
        [ folderSize (GiB 2UL)
          olderThan (Duration.FromHours (14L * 24L)) ]
      Rotation.Rotate (rotate, delete)

    /// Rotates your log files when each file has reached 200 MiB.
    /// Lets your (for your service; single-purpose) logging folder grow to
    /// 2 GiB.
    let ``rotate >200 MiB, delete if folder >3 GiB`` =
      let rotate, delete =
        [ FileSize (MiB 200UL) ],
        [ folderSize (GiB 3UL) ]
      Rotation.Rotate (rotate, delete)

    /// Rotates your log files when each file has reached 200 MiB.
    /// Never deletes your log files. Remember to put other sorts of monitoring
    /// in place, e.g. by using Logary's health checks.
    let ``rotate >200 MiB, never delete`` =
      let rotate, delete =
        [ FileSize (MiB 200UL) ],
        []
      Rotation.Rotate (rotate, delete)

  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Target
  open Logary.Internals

  /// The naming specification gives the File target instructions on how to
  /// name files when they are created and rotated.
  type Naming =
    Naming of specification:string

    with
      member x.format (ri : RuntimeInfo) =
        let (Naming spec) = x
        let now = ri.clock.Now.ToDateTimeOffset()
        let known =
          Map [ "service", ri.serviceName
                "date", now.ToString("yyyy-MM-dd")
                "datetime", now.ToString("yyyy-MM-ddThh:mm:ssZ") ]
        "TODO"



  /// The file target configuration record. This is used to customise the behaviour
  /// of the file target.
  type FileConf =
      /// Whether to buffer the string writer in this process' memory. Defaults to false,
      /// so that the textwriter that writes to the underlying file stream is continuously
      /// flushed.
    { inProcBuffer : bool
      /// Whether to force the operating system's page cache to flush to persistent
      /// storage for each log message. By default this is false, but sending a
      /// Flush message to the target forces the page cache to be flushed to disk
      /// (and of course will also force-flush the in-process buffer if needed).
      /// If you specify the `writeThrough` flag (which is true by default)
      /// then you can have this flag as false.
      ///
      /// This flag corresponds to `Flush(true)`.
      flushToDisk : bool
      /// Whether the `FileStream` is opened with the flags FILE_FLAG_WRITE_THROUGH
      /// and FILE_FLAG_NO_BUFFERING. Defaults to true, to let this target model a
      /// transaction log. See https://support.microsoft.com/en-us/kb/99794 for more
      /// details. Turning this flag off gives a 21x throughput boost on SSD and
      /// a 140x throughput boost on spinning disk on Windows -
      /// ref https://ayende.com/blog/174785/fast-transaction-log-windows. Linux
      /// gets a 10x performance boost on SSD without this flag.
      writeThrough : bool
      /// Use the `Rotation` discriminated union to specify the policy for
      /// when to rotate the file. If you rotate the files, you can optionally
      /// choose to let this target delete files, too, by supplying a list of
      /// deletion policies.
      policies : Rotation

      /// Where to save the logs.
      logFolder : FileSystem.FolderPath

      /// What encoding to use for writing the text to the file.
      encoding  : Encoding

      /// The naming specification gives the File target instructions on how to
      /// name files when they are created and rotated.
      naming : Naming

      /// The file system abstraction to write to.
      fileSystem : FileSystem.FileSystem

      /// The formatter is responsible for writing to the textwriter in an async manner,
      /// however, the returned Alts do not have mean that any flush has been done.
      /// The returned value is hot, i.e. it's begun executing. Must never throw
      /// exceptions; instead place the exceptions in the returned promise.
      formatter : Message -> TextWriter -> Promise<unit>

      /// How many log messages to write in one go.
      batchSize : uint16

      /// How many times to try to recover a failed batch messages.
      attempts  : uint16 }

  /// The empty/default configuration for the Logary file target.
  let empty =
    { inProcBuffer = false
      flushToDisk  = false
      writeThrough = true
      policies     = Policies.``rotate >200 MiB, delete if folder >3 GiB``
      logFolder    = Environment.CurrentDirectory
      encoding     = Encoding.UTF8
      naming       = Naming "{service}-{date}.log"
      fileSystem   = FileSystem.DotNetFileSystem()
      formatter    = fun m tw ->
        let str = Formatting.StringFormatter.levelDatetimeMessagePathNl.format m
        tw.WriteLine str
        Promise (())
      batchSize    = 100us
      attempts     = 3us }

  module internal Impl =
    open System.IO
    open System.Text
    open FileSystem

    [<Sealed>]
    type CountingStream(inner : Stream, written : int64 ref) =
      inherit Stream()

      /// Updates the passed bytes refrence
      member x.updateBytesRef () =
        written := inner.Position

      override x.Write(buffer, offset, length) =
        written := !written + int64 length
        inner.Write(buffer, offset, length)

      override x.WriteAsync(buffer, offset, length, cancellationToken) =
        written := !written + int64 length
        inner.WriteAsync(buffer, offset, length, cancellationToken)

      override x.BeginWrite(buffer, offset, count, callback, state) =
        written := !written + int64 count
        inner.BeginWrite(buffer, offset, count, callback, state)

      override x.EndWrite(state) =
        inner.EndWrite(state)

      // others:
      override x.CanRead = inner.CanRead
      override x.CanWrite = inner.CanWrite
      override x.CanSeek = inner.CanSeek
      override x.Length = inner.Length
      override x.Position
        with get() = inner.Position
        and set value = inner.Position <- value
      override x.Flush() = inner.Flush()
      override x.Seek (offset, origin) = inner.Seek (offset, origin)
      override x.SetLength value = inner.SetLength value
      override x.Read(buffer, offset, count) = inner.Read(buffer, offset, count)

    type State =
      { underlying : FileStream
        writer     : TextWriter
        written    : int64 ref }

      static member create (underlying, writer, written) =
        { underlying = underlying
          writer = writer
          written = written }

      interface IDisposable with
        member x.Dispose() =
          x.writer.Dispose()
          // underlying is closed when writer is disposed
          x.written := 0L

    let applyRotation counter (fs : Stream) (policies : RotationPolicy list) : Stream =
      let rec iter state = function
        | [] -> state
        | Scheduled _ :: rest -> iter state rest
        | FileSize size :: rest ->
          let stream = new CountingStream(state, counter)
          stream.updateBytesRef()
          stream :> Stream
        | FileAge age :: rest -> iter state rest
      iter fs policies

    /// Takes the counter reference cell, and if there exists a rotation policy
    /// dependent on the 
    let applyStreamPolicies counter fs : Rotation -> Stream = function
      | Rotation.SingleFile ->
        fs
      | Rotation.Rotate ([] as rotation, _) ->
        fs
      | Rotation.Rotate (rotations, _) ->
        applyRotation counter fs rotations

    let openFile ri conf =
      let folder = conf.logFolder
      let fileName = conf.naming.format ri
      let path = Path.combine [folder; fileName]

      let fs =
        new FileStream(
          path,
          FileMode.Append,
          FileAccess.Write,
          FileShare.Read,
          0x1000,
          FileOptions.Asynchronous // IO overlapped
          ||| (if conf.writeThrough then FileOptions.WriteThrough else enum<FileOptions>(0)))

      let counter = ref 0L

      let writer =
        new StreamWriter(
          applyStreamPolicies counter fs conf.policies,
          conf.encoding)

      fs, writer, counter

    let writeRequests ilogger conf state (reqs : TargetMessage[]) =
      // `completed` can throw
      let ack (completed, ack) = completed >>=. IVar.fill ack ()
      let ackAll acks = acks |> Seq.map ack |> Job.conIgnore
      let flush (ack, nack) = Ch.give ack () <|> nack
      let flushAll flushes = flushes |> Seq.map flush |> Job.conIgnore

      let acks = ResizeArray<_>() // Updated by the loop.
      let flushes = ResizeArray<_>() // Updated by the loop.
      let mutable forceFlush = false // Updated by the loop.

      for m in reqs do
        match m with
        | Log (message, ack) ->
          // Write and save the ack for later.
          acks.Add (conf.formatter message state.writer, ack)
          // Invariant: always flush fatal messages.
          if message.level = Fatal then forceFlush <- true
        | Flush (ackCh, nack) ->
          Message.event Debug "Scheduling disk flush." |> Logger.logSimple ilogger
          flushes.Add (ackCh, nack)
          // Invariant: calling Flush forces a flush to disk.
          forceFlush <- true

      let flushWriter = // Invariant: only schedule after writing messages.
        memo <|
          // Unless forceFlush is included in the check, there can be state in
          // the TextWriter that is not written to disk.
          if not conf.inProcBuffer || forceFlush then
            Job.fromUnitTask state.writer.FlushAsync
          else
            Job.result ()

      let flushPageCache = // Invariant: only schedule after all messages written
        memo <|
          if conf.flushToDisk || forceFlush then
            Message.event Debug "Flushing to disk." |> Logger.log ilogger >>=.
            Job.Scheduler.isolate (fun _ -> state.underlying.Flush true)
          else
            Job.result ()

      // after the batch, consider flushing the writer
      flushWriter >>=.
      // then consider flushing the page cache
      flushPageCache >>=.
      // finally, start acking; clients not wanting to wait for the above
      // can handle their own concurrency outside of the target
      ackAll acks >>=.
      // deal with all the flush requests
      flushAll flushes

    let writeRequestsSafe ilogger conf state reqs =
      Job.tryIn (writeRequests ilogger conf state reqs)
                (Choice1Of2 >> Job.result)
                (function
                | :? IOException as e ->
                  Job.result (Choice2Of2 e)
                | other ->
                  Job.raises other)

    let loop (conf : FileConf)
             (ri : RuntimeInfo)
             (requests : RingBuffer<_>)
             (shutdown : Ch<_>)
             (saveWill : obj -> Job<unit>)
             (lastWill : obj option) =

      // For type checking purposes, shadows parameters.
      let saveWill (msgs : TargetMessage[], recoverCount : uint16) =
        saveWill (box (msgs, recoverCount))

      // For type checking purposes.
      let lastWill =
        lastWill |> Option.map (fun x ->
          let (msgs : TargetMessage[], recoverCount : uint16) = unbox x
          msgs, recoverCount)

      // In this state the File target opens its file stream and checks if it
      // progress to the recovering state.
      let rec init lastWill =
        let state = State.create (openFile ri conf)
        match lastWill with
        | Some (msgs, recoverCount) ->
          (msgs, recoverCount) ||> recovering state
        | None ->
          checking state

      // In this state we try to write as many log messages as possible.
      and running (state : State) : Job<unit> =
        Alt.choose [
          shutdown ^=> fun ack ->
            Message.event Info "Shutting down file target." |> Logger.log ri.logger >>=.
            Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose()) >>=.
            ack *<= ()

          RingBuffer.takeBatch (uint32 conf.batchSize) requests ^=> fun reqs ->
            writeRequestsSafe ri.logger conf state reqs >>= function
              | Choice1Of2 () ->
                checking state
              | Choice2Of2 err ->
                Message.event Error "IO Exception while writing to file. Batch size is {batchSize}."
                |> Message.setField "batchSize" conf.batchSize
                |> Message.addExn err
                |> Logger.logWithAck ri.logger
                >>= id
                >>=. saveWill (reqs, 1us)
                >>=. Job.raises err
        ] :> Job<_>

      // In this state we verify the state of the file.
      and checking (state : State) =
        // TODO: implement checking logic
        running state

      // In this state we do a single rotation.
      and rotating (state : State) : Job<unit> =
        // TODO: implement rotation logic
        running state

      and recovering (state : State) (lastBatch : TargetMessage[]) = function
        // Called with 1, 2, 3 – 1 is first time around.
        | recoverCount when recoverCount <= conf.attempts ->
          writeRequestsSafe ri.logger conf state lastBatch >>= function
            | Choice1Of2 () ->
              checking state
            | Choice2Of2 ex ->
              Message.event Error "Attempt {attempts} failed, trying again shortly."
              |> Message.setField "attempts" recoverCount
              |> Logger.logWithAck ri.logger
              >>= id
              >>=. saveWill (lastBatch, recoverCount + 1us)
              >>=. Job.raises ex

        | recoverCount ->
          Message.event Fatal "Could not recover in {attempts} attempts."
          |> Message.setField "attempts" recoverCount
          |> Logger.logWithAck ri.logger
          >>= id
          >>=. Job.raises (Exception "File recovery failed, crashing out.")

      init lastWill

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
      // TMPDIR is on OS X and Linux
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

    testList "naming policies" [
      testCase "service – from RuntimeInfo" <| fun _ _ -> Job.result ()
      testCase "year-month-day - date" <| fun _ _ -> Job.result ()
      testCase "year-month-dayThour-minutes-seconds – datetime" <| fun _ _ -> Job.result ()
      testCase "sequence number 007 - sequence" <| fun _ _ -> Job.result ()
    ]

    Targets.basicTests "file" (fun name -> File.create File.empty name)
    Targets.integrationTests "file" (fun name -> File.create File.empty name)

    testCase "can create" <| fun folderPath fileName ->
      //File.create (FileConf.create folderPath fileName)
      Job.result ()
  ]
