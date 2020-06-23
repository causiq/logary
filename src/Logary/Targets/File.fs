/// This is the File target of Logary. It can be both a rolling file and a "normal", non-rolling file target, depending
/// on the deletion and rotation policies supplied.
module Logary.Targets.File

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Configuration.Target
open Logary.Targets.FileSystem

/// Bytes
let B (n: int64) = n
/// Kilobytes
let KiB n = n * 1024L
/// Megabytes
let MiB n = n * 1024L * 1024L
/// Gigabytes
let GiB n = n * 1024L * 1024L * 1024L

/// These are the available rotation policies
type RotationPolicy =
  /// The callback rotation policy is called whenever the check to rotate is
  /// made. In the callback, the programmer can choose to read the file
  /// attributes. Reading the files' attributes (such as last-modified) may
  /// have side-effects on Windows, such as flushing the page cache to disk
  /// and thereby fsyncing, so if you end up scheduling flushes often (less
  /// than every second or so), then consider to let your callback remember
  /// that last lookup of the file's attribute(s).
  | Callback of (FileInfo * FileSize -> bool)
  /// Rotate every given file size.
  | FileSize of maxSize:int64
  | FileAge of age:Duration

/// This module contains some rotation policies you may like to use by
/// default.
module RotationPolicies =
  /// This module contains rotation policies that rotate the files after
  /// specific periods.
  module ByAge =
    let everyMinute = FileAge (Duration.FromMinutes 1L)
    let every10minutes = FileAge (Duration.FromMinutes 10L)
    let hourly = FileAge (Duration.FromHours 1.)
    let daily = FileAge (Duration.FromDays 1.)
    let weekly = FileAge (Duration.FromDays 7.)
    let ofDuration dur = FileAge dur

  /// This module contains rotation policies that specify that files should
  /// be rotated every nth byte (e.g. every 100 MiB).
  module BySize =
    let everyMegabyte = FileSize (MiB 1L)
    let everyNthMegabyte n = FileSize (MiB n)
    let everyHundredMegabytes = everyNthMegabyte 100L
    let everyGigabyte = FileSize (GiB 1L)

  /// Implements a custom rotation policy signified by the passed callback
  /// function.
  let callback cb =
    Callback cb

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
  let maxNoOfFiles (n: int16): DeletionPolicy =
    fun (dirInfo, fileInfo) ->
      // TODO: implement
      KeepFile

  /// Deletes all log files (but not the current non-rotated one) older than
  /// the given duration.
  let olderThan (dur: Duration): DeletionPolicy =
    fun (dirInfo, fileInfo) ->
      // TODO: implement
      KeepFile

  /// Deletes files in oldest-first order when the folder size has passed a
  /// given bytes-large threshold. Be aware that you should not use this
  /// policy alone when there are other services with the same policy (alone)
  /// logging to the same folder.
  let folderSize (bytesSize: int64): DeletionPolicy =
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
      [ FileSize (MiB 200L) ],
      [ folderSize (GiB 2L)
        olderThan (Duration.FromDays 14.) ]
    Rotation.Rotate (rotate, delete)

  /// Rotates your log files when each file has reached 200 MiB.
  /// Lets your (for your service; single-purpose) logging folder grow to
  /// 2 GiB.
  let ``rotate >200 MiB, delete if folder >3 GiB`` =
    let rotate, delete =
      [ FileSize (MiB 200L) ],
      [ folderSize (GiB 3L) ]
    Rotation.Rotate (rotate, delete)

  /// Rotates your log files when each file has reached 200 MiB.
  /// Never deletes your log files. Remember to put other sorts of monitoring
  /// in place, e.g. by using Logary's health checks.
  let ``rotate >200 MiB, never delete`` =
    let rotate, delete =
      [ FileSize (MiB 200L) ],
      []
    Rotation.Rotate (rotate, delete)

module internal P =
  // conflicts with Hopac.Hopac.run, Logary.Internals.TimeoutResult
  open FParsec

  type Token =
    | Placeholder of name:string
    | Lit of str:string

  let ph: Parser<Token, unit> =
    between (pstring "{" <?> "Placeholders must start with '{'")
            (pstring "}" <?> "Placeholders must end with '}'")
            (manyChars letter |>> Placeholder
             <?> "Only letters are supported as placeholders")

  let lit: Parser<Token, unit> =
    let invalids =
      [| yield! Path.GetInvalidFileNameChars()
         yield! "{}\n\r\b\a:;\\\"'".ToCharArray() |]

    let error =
      invalids
      |> Array.map (function
        | '\n' -> "\\n"
        | '\t' -> "\\t"
        | '\r' -> "\\r"
        | '\b' -> "\\b"
        | '\a' -> "\\a"
        | c when Char.IsControl c -> sprintf "%x" (int c)
        | c -> "'" + string c + "'")
      |> String.concat ", "
      |> sprintf "Literal fillers in the name may not contain any of these characters (%s)"

    many1Satisfy (fun c -> not (Array.contains c invalids)) |>> Lit
    <?> error

  let tokens: Parser<Token list, unit> =
    many1 (ph <|> lit)

  let combined: Parser<Token list, unit> =
    spaces >>. tokens .>> eof

  let parse spec =
    match run combined spec with
    | Success (results, _, _) ->
      Choice1Of2 results
    | Failure (error, _, _) ->
      Choice2Of2 error

  let private foldStr fph flit =
    fun tokens ->
      let sb = StringBuilder()
      let app (sb: StringBuilder) (value: string) = sb.Append value
      let folder sb = function
        | Placeholder ph -> app sb (fph ph)
        | Lit lit -> app sb (flit lit)
      tokens |> List.fold folder sb |> sprintf "%O"

  let format (known: Map<_, _>) (tokens: Token list) =
    foldStr (flip Map.find known) id tokens

  let formatRegex (known: Map<_, _>) (tokens: Token list) =
    foldStr (flip Map.find known) Regex.Escape tokens

/// The naming specification gives the File target instructions on how to
/// name files when they are created and rotated.
type Naming =
  /// "{date}", "log" => "2016-10-15.log"
  Naming of spec:string * ext:string

  with
    /// Gives back a file-name without extention and an extension from the
    /// given RuntimeInfo.
    member x.format (ri: RuntimeInfo): string * string =
      let (Naming (spec, ext)) = x
      let now = (ri.getTimestamp () |> Instant.ofEpoch).ToDateTimeOffset()

      let known =
        ri.resource.asMap()
          |> Map.add "date" (now.ToString("yyyy-MM-dd"))
          |> Map.add "datetime" (now.ToString("yyyy-MM-ddTHH-mm-ssZ"))

      match P.parse spec with
      | Choice1Of2 tokens ->
        P.format known tokens, ext
      | Choice2Of2 error ->
        failwith error

    /// Gives back a file-name WITH extention given RuntimeInfo.
    member x.formatS (ri: RuntimeInfo): string =
      x.format ri ||> sprintf "%s.%s"

    /// Gives back a regex that matches what this Naming spec will name files
    /// at any point in time (i.e. it will match any date/datetime file name)
    /// for the Naming spec.
    member x.regex (ri: RuntimeInfo) =
      let (Naming (spec, ext)) = x
      let known =
        [ "service", Regex.Escape ri.resource.service
          "date", @"\d{4}-\d{2}-\d{2}"
          "datetime", @"\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}Z"
          yield! ri.resource.asLabels() |> Seq.map (fun (k, v) -> k, Regex.Escape v)
        ] |> Map
      match P.parse spec with
      | Choice1Of2 tokens ->
        P.formatRegex known tokens
        |> fun specRegex -> sprintf @"^%s\.%s$" specRegex (Regex.Escape ext)
        |> Regex
      | Choice2Of2 error ->
        failwith error

/// Module for deleting old log files.
module internal Janitor =
  let globFiles (ri: RuntimeInfo) (fs: FileSystem) (Naming (spec, ext) as naming) =
    fs.glob (naming.regex ri)
    |> Seq.map (fun fi ->
      let dir = Path.GetDirectoryName fi.FullName
      DirectoryInfo dir, fi)

  let iter (globber: unit -> seq<DirectoryInfo * FileInfo>) deleter policies () =
    seq {
      for dir, file in globber () do
        for policy in policies do
          match policy (dir, file) with
          | KeepFile -> ()
          | DeleteFile ->
            yield (file.FullName: FilePath) }
    |> Seq.iter deleter

  type T =
    | NullJanitor
    | LiveJanitor of stop:IVar<unit>

  let create (ri: RuntimeInfo) (fs: FileSystem) (naming: Naming): Rotation -> Job<T> = function
    | Rotation.SingleFile
    | Rotation.Rotate (_, []) ->
      Job.result NullJanitor
    | Rotation.Rotate (_, deletions) ->
      let stopIV = IVar ()
      let glob () = globFiles ri fs naming
      let tickCh = Ch ()
      let reschedule () = timeOutMillis 5000 ^=> Ch.send tickCh
      let loop = Job.delay <| fun () ->
        Alt.choose [
          stopIV :> Alt<unit>
          (tickCh ^-> iter glob fs.deleteFile deletions) ^=> reschedule
        ]
      Job.foreverServer loop >>-. LiveJanitor stopIV

  let shutdown (t:T): Job<unit> =
    match t with
    | NullJanitor ->
      Job.result()
    | LiveJanitor stopIV ->
      IVar.fill stopIV ()

/// The file target configuration record. This is used to customise the behaviour
/// of the file target. You should have a look at the README for more details
/// on how the File target is built – its intended behaviour is rather well
/// specified.
type FileConf =
    /// Whether to buffer the string writer in this process' memory. Defaults to false,
    /// so that the textwriter that writes to the underlying file stream is continuously
    /// flushed.
  { inProcBuffer: bool
    /// Whether to force the operating system's page cache to flush to persistent
    /// storage for each log batch. By default this is false, but sending a
    /// Flush message to the target forces the page cache to be flushed to disk
    /// (and of course will also force-flush the in-process buffer if needed).
    /// If you specify the `writeThrough` flag (which is true by default)
    /// then you can have this flag as false.
    ///
    /// This flag corresponds to `Flush(true)`.
    flushToDisk: bool
    /// Whether the `FileStream` is opened with the flags FILE_FLAG_WRITE_THROUGH
    /// and FILE_FLAG_NO_BUFFERING. Defaults to true, to let this target model a
    /// transaction log. See https://support.microsoft.com/en-us/kb/99794 for more
    /// details. Turning this flag off gives a 21x throughput boost on SSD and
    /// a 140x throughput boost on spinning disk on Windows -
    /// ref https://ayende.com/blog/174785/fast-transaction-log-windows. Linux
    /// gets a 10x performance boost on SSD without this flag.
    writeThrough: bool
    /// Use the `Rotation` discriminated union to specify the policy for
    /// when to rotate the file. If you rotate the files, you can optionally
    /// choose to let this target delete files, too, by supplying a list of
    /// deletion policies.
    policies: Rotation
    /// Where to save the logs.
    logFolder: FolderPath
    /// What encoding to use for writing the text to the file.
    encoding: Encoding
    /// The naming specification gives the File target instructions on how to
    /// name files when they are created and rotated.
    naming: Naming
    /// The file system abstraction to write to.
    fileSystem: FileSystem
    /// The writer is responsible for writing to the TextWriter.
    writer: MessageWriter
    /// How many log messages to write in one go.
    batchSize: uint16
    /// How many times to try to recover a failed batch messages.
    attempts: uint16 }

let defaultMessageFormat = SimpleMessageWriter() :> MessageWriter

/// The empty/default configuration for the Logary file target.
let empty =
  { inProcBuffer = false
    flushToDisk  = false
    writeThrough = true
    policies     = Policies.``rotate >200 MiB, delete if folder >3 GiB``
    logFolder    = Environment.CurrentDirectory
    encoding     = Encoding.UTF8
    naming       = Naming ("{service}-{date}", "log")
    fileSystem   = DotNetFileSystem("/var/lib/logs")
    writer       = defaultMessageFormat
    batchSize    = 100us
    attempts     = 3us }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FileConf =
  /// Creates a new file config from the given folder path with the naming standard.
  let create (folderPath: FolderPath) (naming: Naming) =
    { empty with
        logFolder = folderPath
        fileSystem = DotNetFileSystem folderPath
        naming = naming }

module internal Impl =
  type State =
    { underlying: FileStream
      /// The FileInfo as it were when the file was opened.
      fileInfo: FileInfo
      writer: TextWriter
      written: int64 ref
      janitor: Janitor.T }

    static member create (underlying, fi, writer, written) janitor =
      { underlying = underlying
        fileInfo   = fi
        writer     = writer
        written    = written
        janitor    = janitor }

  let applyRotation counter (fs: Stream) (policies: RotationPolicy list): Stream =
    let rec iter state = function
      | [] -> state
      | Callback _ :: rest -> iter state rest // TODO: also needs counter
      | FileSize _ :: _ ->
        let stream = new CountingStream(state, counter)
        stream.updateBytesRef()
        stream :> Stream
      | FileAge _ :: rest -> iter state rest
    iter fs policies

  /// Takes the counter reference cell, and if there exists a rotation policy
  /// dependent on the
  let applyStreamPolicies counter fs: Rotation -> Stream = function
    | Rotation.SingleFile ->
      fs
    | Rotation.Rotate ([] as rotation, _) ->
      fs
    | Rotation.Rotate (rotations, _) ->
      applyRotation counter fs rotations

  let openFile ri (fs: FileSystem) conf =
    let fileName = conf.naming.formatS ri
    let fi = fs.getFile fileName

    let fs =
      new FileStream(
        fi.FullName,
        FileMode.Append, // only append; seeks error
        FileAccess.Write, // request write access
        FileShare.Read, // allow concurrent readers (tail -f)
        0x1000, // default buffer size
        FileOptions.Asynchronous // IO overlapped
        ||| (if conf.writeThrough then FileOptions.WriteThrough else enum<FileOptions>(0)))

    let counter = ref 0L

    let writer =
      new StreamWriter(
        applyStreamPolicies counter fs conf.policies,
        conf.encoding)

    fs, fi, writer, counter

  let shouldRotate (now: unit -> EpochNanoSeconds)
                   (conf: FileConf) (state: State) =
    let now = now >> Instant.ofEpoch
    let size = !state.written

    let rec apply state = function
      | [] ->
        false
      | Callback cb :: rest ->
        if cb (state.fileInfo, size) then true else apply state rest
      | FileSize maxSize :: rest ->
        if maxSize <= size then true else apply state rest
      | FileAge maxAge :: rest ->
        let created = Instant.FromDateTimeUtc(state.fileInfo.CreationTimeUtc)
        let age = created - now ()
        if maxAge <= age then true else apply state rest

    match conf.policies with
    | Rotation.SingleFile
    | Rotation.Rotate ([], _) ->
      false
    | Rotation.Rotate (policies, _) ->
      policies |> apply state

  let flushWriter (state: State): Job<unit> =
    Job.fromUnitTask state.writer.FlushAsync

  let flushToDisk (state: State): Job<unit> =
    Job.Scheduler.isolate (fun _ -> state.underlying.Flush true)

  let flushAndCloseFile (state: State): Job<unit> =
    flushWriter state >>=. flushToDisk state >>- fun () ->
    state.writer.Dispose()

  let shutdownState (state: State): Job<unit> =
    flushAndCloseFile state >>=.
    Janitor.shutdown state.janitor

  /// Writes all passed requests to disk, handles acks and flushes.
  let writeRequests (ilogger: Logger) (conf: FileConf) (state: State) (reqs: TargetMessage[]) =
    // `completed` can throw
    let ack (completed, ack) = completed >>=. IVar.fill ack ()
    let ackAll acks = acks |> Seq.map ack |> Job.conIgnore
    let flush (ack, nack) = IVar.fill ack () (* TODO: handle NACKing *)
    let flushAll flushes = flushes |> Seq.map flush |> Job.conIgnore

    let acks = ResizeArray<_>() // Updated by the loop.
    let flushes = ResizeArray<_>() // Updated by the loop.
    let mutable forceFlush = false // Updated by the loop.

    for m in reqs do
      match m with
      | Log (message, ack) ->
        // Write and save the ack for later.
        acks.Add (Alt.fromUnitTask (fun ct -> conf.writer.write(state.writer, message, ct)), ack)
        // Invariant: always flush fatal messages.
        if message.level = Fatal then
          ilogger.event("Got fatal message; scheduling disk flush.")
          forceFlush <- true

      | Flush (ack, nack) ->
        ilogger.event("Scheduling disk flush.")
        flushes.Add (ack, nack)
        // Invariant: calling Flush forces a flush to disk.
        forceFlush <- true

    let flushWriter = // Invariant: only schedule after writing messages.
      memo <|
        // Unless forceFlush is included in the check, there can be state in
        // the TextWriter that is not written to disk.
        if not conf.inProcBuffer || forceFlush then
          flushWriter state
        else
          Job.unit ()

    let flushPageCache = // Invariant: only schedule after all messages written
      memo <|
        if conf.flushToDisk || forceFlush then
          flushToDisk state >>=.
          ilogger.eventBP("Flushed to disk.")
        else
          Job.unit ()

    // after the batch, consider flushing the writer
    flushWriter
    // then consider flushing the page cache
    >>=. flushPageCache
    // finally, start acking; clients not wanting to wait for the above
    // can handle their own concurrency outside of the target
    >>=. ackAll acks
    // deal with all the flush requests
    >>=. flushAll flushes

  let writeRequestsSafe ilogger conf state reqs =
    Job.tryIn (writeRequests ilogger conf state reqs)
              (Choice1Of2 >> Job.result)
              (function
              | :? IOException as e ->
                Job.result (Choice2Of2 e)
              | other ->
                Job.raises other)

  let loop (conf: FileConf) (will: Will<TargetMessage[] * uint16>) (api: TargetAPI) =
    let ri, rotateCh = api.runtime, Ch ()

    let shutdownState state =
      ri.logger.timeJob(shutdownState state, measurement="shutDownState")

    // In this state the File target opens its file stream and checks if it
    // progress to the recovering state.
    let rec init () =
      let fs = conf.fileSystem.chroot conf.logFolder
      fs.ensureCurrent () |> ignore
      Janitor.create ri fs conf.naming conf.policies >>-
      State.create (openFile ri fs conf) >>=
      fun state ->
        Will.latest will ^=> function
        | Some (msgs, recoverCount) ->
          (msgs, recoverCount) ||> recovering state
        | None ->
          checking state

    // In this state we try to write as many log messages as possible.
    and running (state: State): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          shutdownState state >>=.
          ack *<= ()

        // TODO: handle scheduled rotation
        rotateCh ^=> fun () -> rotating state

        RingBuffer.takeBatch (conf.batchSize) api.requests ^=> fun reqs ->
          writeRequestsSafe ri.logger conf state reqs >>= function
            | Choice1Of2 () ->
              checking state
            | Choice2Of2 err ->
              ri.logger.errorAck("IO Exception while writing to file. Batch size is {batchSize}.", fun m ->
                m.setField("batchSize", conf.batchSize)
                m.addExn err)
              >>=. Will.update will (reqs, 1us)
              >>=. Job.raises err
      ] :> Job<_>

    // In this state we verify the state of the file.
    and checking (state: State) =
      if shouldRotate ri.getTimestamp conf state then rotating state
      else running state

    // In this state we do a single rotation.
    and rotating (state: State) =
      shutdownState state >>-
      (fun () ->
        let fnNoExt (s: string) = Path.GetFileNameWithoutExtension(s)
        let parse (n: string) =
          match Int32.TryParse n with
          | false, _ -> None
          | true, value -> Some value
        let currName = fnNoExt state.fileInfo.Name
        let nextName, nextExt = conf.naming.format ri
        let targetName =
          if nextName <> currName then
            sprintf "%s.%s" nextName nextExt
          else
            conf.fileSystem.glob (conf.naming.regex ri)
            |> Seq.map (fun fi -> fnNoExt fi.Name)
            |> Seq.filter (String.contains currName)
            |> Seq.map (fun name ->
              match Regex.``match`` @"\d{3}$" name with
              | None -> None
              | Some g -> parse g.[0].Value)
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
            |> Seq.sortDescending
            |> Seq.tryPick Some
            |> Option.fold (fun s t -> t + 1) 0
            |> sprintf "%s-%03i" currName
        conf.fileSystem.moveFile state.fileInfo.Name targetName) >>=.
      init ()

    and recovering (state: State) (lastBatch: TargetMessage[]) = function
      // Called with 1, 2, 3 – 1 is first time around.
      | recoverCount when recoverCount <= conf.attempts ->
        writeRequestsSafe ri.logger conf state lastBatch >>= function
          | Choice1Of2 () ->
            checking state
          | Choice2Of2 ex ->
            ri.logger.fatalAck("Attempt {attempts} failed, trying again shortly.", fun m -> m.setField("attempts", recoverCount))
            >>=. Will.update will (lastBatch, recoverCount + 1us)
            >>=. Job.raises ex

      | recoverCount ->
        ri.logger.fatalAck ("Could not recover in {attempts} attempts.", fun m -> m.setField("attempts", recoverCount))
        >>=. Job.raises (Exception "File recovery failed, crashing out.")

    init ()

/// Create a new File target through this.
[<CompiledName "Create">]
let create conf name =
  TargetConf.create Policy.exponentialBackoffForever 512us (Impl.loop conf) name

/// Use with LogaryFactory.New(s => s.Target<File.Builder>())
type Builder(conf, callParent: ParentCallback<Builder>) =
  let update (conf' : FileConf): Builder =
    Builder(conf', callParent)

  // NOTE: this code is particular to the default configuration values of empty

  member x.BufferInProc() =
    update { conf with inProcBuffer = true }

  member x.FlushToDisk() =
    update { conf with flushToDisk = true }

  member x.NoWriteThrough() =
    update { conf with writeThrough = false }

  member x.Rotate_Gt300GiB_DeleteIfFolder_Gt2GiB_Or_FileAge_Gt2Wks() =
    update { conf with policies = Policies.``rotate >200 MiB, delete if folder >2 GiB or file age >2 weeks`` }

  member x.Rotate_Gt300GiB_DeleteIfFolder_Gt3GiB() =
    update { conf with policies = Policies.``rotate >200 MiB, delete if folder >3 GiB`` }

  member x.Rotate_Gt200MiB_Delete_Never() =
    update { conf with policies = Policies.``rotate >200 MiB, never delete`` }

  member x.LogFolder folderPath =
    update { conf with logFolder = folderPath }

  member x.Encoding enc =
    update { conf with encoding = enc }

  member x.Naming (spec, ext) =
    update { conf with naming = Naming (spec, ext) }

  member x.FileSystem fs =
    update { conf with fileSystem = fs }

  member x.Writer writer =
    update { conf with writer = writer }

  member x.WriterFunction (fn: Func<TextWriter, LogaryMessage, CancellationToken, Task>) =
    let writer = { new MessageWriter with member x.write(w, m, ct) = fn.Invoke(w, m, ct) }
    update { conf with writer = writer }

  member x.BatchSize size =
    { conf with batchSize = size }

  member x.Attempts maxAttempts =
    { conf with attempts = maxAttempts }

  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  member x.Done() =
    ! (callParent x)

  interface SpecificTargetConf with
    member x.Build name = create conf name
