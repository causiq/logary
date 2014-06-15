/// API for writing logs and metrics.
/// For gauges that run continuously based on a timer, have a look at
/// the Registry module. To get a logger, have a look at the Logger module.
///
/// If you are using I recommend doing:
/// `Install-Package Intelliplan.Logary.CSharp` instead of dealing with the
/// interop problems that you will get from using this module directly.
module Logary.Log

module internal Map =
  let put k v (m : Map<_,_>) =
    match m.TryFind k with
    | None -> m |> Map.add k v
    | Some _ -> m |> Map.remove k |> Map.add k v

open System.Diagnostics

open NodaTime

open Logary
open Logary.Internals

/// A suggestion for an exception tag to send with log lines that are for
/// exceptions
[<Literal>]
let ExceptionTag = "exn"

////////////////////
// Setter methods //
////////////////////

/// Add a key-value pair to the data
[<CompiledName "SetData">]
let setData k v line = { line with LogLine.data = line.data |> Map.put k v }

/// Add the key-value pairs to the data
[<CompiledName "SetDatas">]
let setDatas datas line = datas |> Seq.fold (fun s (k, v) -> s |> setData k v) line

// Sets the path of the log line to the passed value
[<CompiledName "SetPath">]
let setPath p line = { line with LogLine.path = p }

/// Add a tag 't' to the log line 'line'.
[<CompiledName "SetTag">]
let setTag t line = { line with tags = t :: line.tags }

/// Set the LogLine's main exception property
[<CompiledName "SetException">]
let setExn e line = { line with ``exception`` = Some e }

/////////////////////
// Logging methods //
/////////////////////

/// Create a new log line at the specified level for the given message.
[<CompiledName "LogLine">]
let logLine level msg = LogLine.Create(msg, level)

/// Write a log entry from a log line.
[<CompiledName "Log">]
let log (logger : Logger) line =
  (line : LogLine)
  |> fun l -> match l.path with "" -> { l with path = logger.Name } | _ -> l
  |> logger.Log

/// Write a verbose log entry, for help constructing format string, see:
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
[<CompiledName "VerboseFormat">]
let verbosef logger fmt = log logger << Printf.kprintf (logLine Verbose) fmt

/// Write a verbose log entry to the logger
[<CompiledName "Verbose">]
let verbose logger = log logger << logLine Verbose

/// Create a verbose log line with a message
[<CompiledName "VerboseString">]
let verboseStr = logLine Verbose

/// Create a verbose log line with a message and a tag
[<CompiledName "VerboseStringTag">]
let verboseStrTag tag = setTag tag << logLine Verbose

/// Write a debug log entry, for help constructing format string, see:
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
[<CompiledName "DebugFormat">]
let debugf logger fmt = log logger << Printf.kprintf (logLine Debug) fmt

/// Write a debug log entry to the logger
[<CompiledName "Debug">]
let debug logger = log logger << logLine Debug

/// Create a debug log line with a message
[<CompiledName "DebugString">]
let debugStr = logLine Debug

/// Create a debug log line with a message and a tag
[<CompiledName "DebugStringTag">]
let debugStrTag tag = setTag tag << logLine Debug

/// Write a info log entry, for help constructing format string, see:
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
[<CompiledName "InfoFormat">]
let infof logger fmt = log logger << Printf.kprintf (logLine Info) fmt

/// Write an info log entry to the logger
[<CompiledName "Info">]
let info logger = log logger << logLine Info

/// Create an info log line with a message
[<CompiledName "InfoString">]
let infoStr = logLine Info

/// Create an info log line with a message and a tag
[<CompiledName "InfoStringTag">]
let infoStrTag tag = setTag tag << logLine Info

/// Write a warn log entry, for help constructing format string, see:
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
[<CompiledName "WarnFormat">]
let warnf logger fmt = log logger << Printf.kprintf (logLine Warn) fmt

/// Write a warn log entry
[<CompiledName "Warn">]
let warn logger = log logger << logLine Warn

/// Create an warn log line with a message
[<CompiledName "WarnString">]
let warnStr = logLine Warn

/// Create a warn log line with a message and a tag
[<CompiledName "WarnStringTag">]
let warnStrTag tag = setTag tag << logLine Warn

/// Write a error log entry, for help constructing format string, see:
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
[<CompiledName "ErrorFormat">]
let errorf logger fmt = log logger << Printf.kprintf (logLine Error)

/// Write an error log entry
[<CompiledName "Error">]
let error logger = log logger << logLine Error

/// Create an error log line with a message
[<CompiledName "ErrorString">]
let errorStr = logLine Error

/// Create an error log line with a message and a tag
[<CompiledName "ErrorStringTag">]
let errorStrTag tag = setTag tag << logLine Error

/// Write a fatal log entry, for help constructing format string, see:
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
[<CompiledName "FatalFormat">]
let fatalf logger fmt = log logger << Printf.kprintf (logLine Fatal) fmt

/// Write a fatal log entry
[<CompiledName "Fatal">]
let fatal logger = log logger << logLine Fatal

/// Create a fatal log entry with a message
[<CompiledName "FatalString">]
let fatalStr = logLine Fatal

/// Create a fatal log entry with a message and a tag
[<CompiledName "FatalStringTag">]
let fatalStrTag tag = setTag tag << logLine Fatal

/////////////
// TRACING //
/////////////

// TODO: tracing with tags
