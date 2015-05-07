namespace Logary

open System
open System.Runtime.CompilerServices

open Logary.Internals

/// Extensions for logging to a Logger
[<Extension>]
module LoggerExtensions =

  /// Log a log line to the log
  [<Extension; CompiledName("Log")>]
  let log (logger : Logger, message, level, data, tags, path,
           ``exception``,
           timestamp : Nullable<NodaTime.Instant>) =
    if message == null then nullArg "message"
    { message       = message
      level         = level
      data          = Map.fromObj data
      path          = match path with null -> logger.Name | _ -> path
      tags          = match tags with null -> [] | _ -> List.ofSeq tags
      ``exception`` = match ``exception`` with null -> None | _ -> Some ``exception``
      timestamp     = if timestamp.HasValue then timestamp.Value else Date.now() }
    |> logger.Log

  /// Log a message with some accompanying data to the log
  [<Extension; CompiledName("Log")>]
  let logAnnotate (logger : Logger, message, level, data) =
    if message == null then nullArg "message"
    { message       = message
      level         = level
      data          = Map.fromObj data
      path          = logger.Name
      tags          = []
      ``exception`` = None
      timestamp     = Date.now () }
    |> logger.Log
