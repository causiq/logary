namespace Logary

open System

open NodaTime

/// The log levels specify the severity of the message.
[<StructuralEquality; NoComparison>]
type LogLine =
    /// The message for the log line
  { message       : string
    /// A dictionary-alike object that keeps data to be logged
    data          : Map<string, obj>
    /// A log level
    level         : LogLevel
    /// A list of tags in the log line
    tags          : string list
    /// When the log line was created
    timestamp     : Instant
    /// Normally a URI or a hierachy: can be filled out by the logging framework.
    /// It denotes the location that the log or metric was sent from.
    path          : string
    /// Optional exception
    ``exception`` : exn option }

/// The main module to create and set properties for a given LogLine.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogLine =
  open Logary.Internals

  /// a module with lenses for LogLine
  module Lenses =
    open Lenses

    let message_ =
      { get = fun x -> x.message
        set = fun v x -> { x with message = v } }

    let data_ =
      { get = fun x -> x.data
        set = fun v x -> { x with data = v } }

    let level_ =
      { get = fun x -> x.level
        set = fun v x -> { x with level = v } }

    let tags_ =
      { get = fun x -> x.tags
        set = fun v x -> { x with tags = v } }

    let timestamp_ =
      { get = fun x -> x.timestamp
        set = fun v x -> { x with timestamp = v } }

    let path_ =
      { get = fun x -> x.path
        set = fun v x -> { x with path = v } }

    let exception_ =
      { get = fun x -> x.``exception``
        set = fun v x -> { x with ``exception`` = v } }

    // extras/patterns:

    /// Gets (KeyNotFoundException if not found) or puts the key (idempotently)
    /// to the data Map
    let dataItem_ k =
      { get = fun x -> x.data |> Map.find k
        set = fun v x -> { x with data = x.data |> Map.put k v } }

  ///////////////////////////
  // 'easy' setter methods //
  ///////////////////////////

  let private s x = Some x

  /// Set the message of the LogLine
  [<CompiledName "SetMsg">]
  let setMsg m line = Lenses.message_.set m line

  /// Sets the level of the log line
  [<CompiledName "SetLevel">]
  let setLevel l line = Lenses.level_.set l line

  /// Add a key-value pair to the data
  [<CompiledName "SetData">]
  let setData k o line = (Lenses.dataItem_ k).set o line

  /// Add the key-value pairs to the data
  [<CompiledName "SetDatas">]
  let setDatas datas line = datas |> Seq.fold (fun s (k, v) -> s |> setData k v) line

  /// Sets the path of the log line
  [<CompiledName "SetPath">]
  let setPath p line = Lenses.path_.set p line

  /// Add a tag 't' to the log line 'line'.
  [<CompiledName "SetTag">]
  let setTag t line = { line with tags = t :: line.tags }

  /// Set the LogLine's main exception property
  [<CompiledName "SetExn">]
  let setExn e line = Lenses.exception_.set (Some e) line

  /// Set the LogLine's timestamp
  [<CompiledName "SetTimestamp">]
  let setTimestamp ts line = Lenses.timestamp_.set ts line

  //////////////////////////
  // Construction methods //
  //////////////////////////

  /// A suggestion for an exception tag to send with log lines that are for
  /// exceptions
  [<Literal>]
  let ExceptionTag = "exn"

  /// An empty log line with a current (now) timestamp. Be warned: using this
  /// doesn't set a new timestamp.
  [<CompiledName "Empty">]
  let empty =
    { message       = ""
      data          = Map.empty
      level         = LogLevel.Info
      tags          = []
      timestamp     = Date.now ()
      path          = ""
      ``exception`` = None }

  /// Create a new log line with the given values.
  [<CompiledName "Create">]
  let create message data level tags path ex =
    { empty with message       = message
                 data          = data
                 level         = level
                 tags          = tags
                 timestamp     = Date.now ()
                 path          = path
                 ``exception`` = ex }

  /// Create a new log line at the specified level for the given message.
  [<CompiledName "Create">]
  let create' level msg =
    { empty with level     = level
                 timestamp = Date.now ()
                 message   = msg }

  /// Create a new log line at the specified path for the given message.
  [<CompiledName "Create">]
  let create'' path msg =
    { empty with path      = path
                 timestamp = Date.now ()
                 message   = msg }

  /// C# interop: BCL friendly parameters!
  /// Create a new log line with the given values.
  [<CompiledName "Create">]
  let create''' message data level tags path (ex : exn) ts =
    let data' =
      (data : Collections.Generic.IDictionary<string, obj>)
      |> Seq.fold (fun s kv -> s |> Map.add kv.Key kv.Value) Map.empty
    let tags' =
      (tags : string [])
      |> Seq.fold (fun s t -> t :: s) []
    let ex = match ex with | null -> None | e -> Some e
    { empty with message       = message
                 data          = data'
                 level         = level
                 tags          = tags'
                 timestamp     = ts
                 path          = path
                 ``exception`` = ex }

  /// Create a LogLine given:
  ///
  ///  - `path`: the path of the log line; best curried at stop of function/module.
  ///  - `level`: the level of the log line
  ///  - `Printf.StringFormat<'a, LogLine> -> 'a`: the sprintf-like data to
  ///
  /// Example usage, at top of module/function:
  ///
  /// ```
  /// let log, logLine = Logger.log logger,
  ///                    LogLine.createf "MyCompany.Module.fn"
  /// ```
  ///
  /// then in the body:
  ///
  /// ```
  /// logLine Info "successfully authenticated user" |> LogLine.setDatas [
  ///   "email", box user_email
  ///   "id", box id
  /// ] |> log)
  /// ```
  ///
  let createf path level =
    Printf.kprintf (
      create'' path
      >> setLevel level
    )

  /// Create a verbose log line with a message
  [<CompiledName "Verbose">]
  let verbose = create' Verbose

  /// Create a verbose log line, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "VerboseFormat">]
  let verbosef fmt = Printf.kprintf (create' Verbose) fmt

  /// Create a verbose log line with a message and a tag
  [<CompiledName "VerboseTag">]
  let verboseTag tag = setTag tag << create' Verbose

  /// Create a debug log line with a message
  [<CompiledName "Debug">]
  let debug = create' Debug

  /// Write a debug log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "DebugFormat">]
  let debugf fmt = Printf.kprintf (create' Debug) fmt

  /// Create a debug log line with a message and a tag
  [<CompiledName "DebugTag">]
  let debugTag tag = setTag tag << create' Debug

  /// Create an info log line with a message
  [<CompiledName "Info">]
  let info = create' Info

  /// Write a info log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "InfoFormat">]
  let infof fmt = Printf.kprintf (create' Info) fmt

  /// Create an info log line with a message and a tag
  [<CompiledName "InfoTag">]
  let infoTag tag = setTag tag << create' Info

  /// Create an warn log line with a message
  [<CompiledName "Warn">]
  let warn = create' Warn

  /// Write a warn log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "WarnFormat">]
  let warnf fmt = Printf.kprintf (create' Warn) fmt

  /// Create a warn log line with a message and a tag
  [<CompiledName "WarnTag">]
  let warnTag tag = setTag tag << create' Warn

  /// Create an error log line with a message
  [<CompiledName "Error">]
  let error = create' Error

  /// Write a error log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "ErrorFormat">]
  let errorf fmt = Printf.kprintf (create' Error) fmt

  /// Create an error log line with a message and a tag
  [<CompiledName "ErrorTag">]
  let errorTag tag = setTag tag << create' Error

  /// Create a fatal log entry with a message
  [<CompiledName "Fatal">]
  let fatal = create' Fatal

  /// Write a fatal log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "FatalFormat">]
  let fatalf fmt = Printf.kprintf (create' Fatal) fmt

  /// Create a fatal log entry with a message and a tag
  [<CompiledName "FatalTag">]
  let fatalTag tag = setTag tag << create' Fatal

  let toMessage (line: LogLine) = ()
