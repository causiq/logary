module Logary.Tests.CoreTargets

open System
open System.IO
open System.Threading
open Expecto
open Logary
open Logary.Internals
open Logary.Model
open Logary.Targets
open Hopac
open Hopac.Infixes
open NodaTime

let logger = Expecto.Logging.Log.create "CoreTargets"

module Files =

  open Logary.Targets.FileSystem
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
    Resource.create("my service", "myHost")
    |> RuntimeInfo.create
    |> RuntimeInfo.setGetTimestamp (fun _ -> now.ToUnixTimeTicks() * Constants.NanosPerTick)

  let runtime2016_10_11T13_14_15 =
    runtime (Instant.FromUtc (2016, 10, 11, 13, 14, 15))

  [<Tests>]
  let fileTests =
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

          let! ri = emptyRuntime
          let fileConf = FileConf.create folder (Naming ("10K", "log"))
          let! targetApi = Target.create ri  (File.create fileConf "basic2")
          do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
            let acks = ResizeArray<_>(10000)
            for i in 1L .. 10000L do
              let msg = Model.Event ("Event {number}", None, level=Info, fs=Map [ "number", Value.Int64 i ])
              let ack = logAndWait msg
              acks.Add ack
            Job.conIgnore acks)
        }

      testList "quantities" [
        testCase "1 KiB is 1024 B" <| fun _ ->
          Expect.isTrue (KiB 1L = B 1024L) "1 KiB should be equal 1024 B"
        testCase "1 MiB is 1024 KiB" <| fun _ ->
          Expect.isTrue (MiB 1L = KiB 1024L) "1 MiB should be equal 1024 KiB"
        testCase "1 GiB is 1024 MiB" <| fun _ ->
          Expect.isTrue (GiB 1L = MiB 1024L) "1 GiB should be equal 1024 MiB"
      ]
    ]
    |> testLabel "core targets"
    |> testLabel "logary"

[<Tests>]
let tests =
  testList "core targets" [
    TargetBaseline.basicTests "text writer" (fun name ->
      let _, _, twTargetConf = buildTextWriterTarget name
      twTargetConf) false

    TargetBaseline.basicTests "System.Diagnostics.Trace" (DiagnosticsTrace.create DiagnosticsTrace.empty) false

    testCase "convert DateTimeOffset with timespan delta" <| fun _ ->
      let a = DateTimeOffset(2016, 07, 02, 12, 33, 56, TimeSpan.FromHours(2.0))
      let ts = a.asTimestamp
      let b = ts |> DateTimeOffset.ofEpoch
      Expect.equal a.Ticks b.Ticks "Should be convertible to timestamp and back, including accounting for TimeSpans"

    testList "text writer prints" [
      testCaseJob "message" <| (job {
        let out, error, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait (Model.Event("Hello World!", None, level=Info)) >>- fun _ ->
          Expect.stringContains (string out) "Hello World!" "logging with info level and then finalising the target")
      })

      testCaseJob "fields" <| (job {
        let out, error, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          let x = dict ["foo", "bar"]
          let message =
            let e = Model.Event("textwriter-test-init", None, level=Info)
            e.setFieldValues(Map [ "foo", "bar" ])
            e

          logAndWait message >>- fun _ ->
          Expect.stringContains (string out) "textwriter-test-init" "logging with fields then finalising the target"
          Expect.stringContains (string out) "foo" "logging with fields then finalising the target"
          Expect.stringContains (string out) "bar" "logging with fields then finalising the target"
          Expect.stringContains (string out) "the Name" "logging with fields then finalising the target")
      })

      testCaseJob "to correct stream" <| (job {
        let out, error, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait (Model.Event("Error line", None, level=Error)) >>= fun _ ->
          logAndWait (Model.Event("Fatal line", None, level=Fatal)) >>- fun _ ->
          let errorStr = string error
          Expect.stringContains errorStr "Error line" "logging 'Error line' and 'Fatal line' to the target"
          Expect.stringContains errorStr "Fatal line" "logging 'Error line' and 'Fatal line' to the target")
      })
    ]
  ]
  |> testLabel "logary"

