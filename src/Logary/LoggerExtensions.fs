namespace Logary

open System.Runtime.CompilerServices

/// Extensions for logging
[<Extension>]
module LoggerExtensions =
  open Logary.Targets

  open System
  open System.Collections.Generic
  open System.Runtime.InteropServices

  // How you can construct maps in F#:
  //  let inline (=>) a b = a, box b
  //  let private a = dict [ "a" => 4, "b" => 3 ]

  let private toMap : obj -> _ = function
    | null -> Map.empty
    | :? IEnumerable<KeyValuePair<string, obj>> as data ->
      data
      |> Seq.map (fun kv -> (kv.Key, kv.Value))
      |> Map.ofSeq
    | :? System.Collections.IDictionary as dict ->
      dict
      |> Seq.cast<System.Collections.DictionaryEntry>
      |> Seq.filter (fun kv -> match kv.Key with :? string -> true | _ -> false)
      |> Seq.map (fun kv -> (kv.Key :?> string, kv.Value))
      |> Map.ofSeq
    | _ as data ->
      data.GetType()
      |> fun t -> t.GetProperties()
      |> Array.map (fun pi -> (pi.Name, pi.GetValue(data, null)))
      |> Map.ofArray

  /// Log a log line to the log
  [<Extension; CompiledName("Log")>]
  let log (logger : Logger, message, level, data, tags, path, ``exception``) =
    if String.IsNullOrWhiteSpace message then nullArg "message"
    { message       = message
      level         = level
      data          = toMap data
      path          = match path with null -> logger.Name | _ -> path
      tags          = match tags with null -> [] | _ -> List.ofSeq tags
      ``exception`` = match ``exception`` with null -> None | _ -> Some ``exception``
      timestamp     = NodaTime.SystemClock.Instance.Now }
    |> logger.Log

  /// Log a message with some accompanying data to the log
  [<Extension; CompiledName("Log")>]
  let logAnnotate (logger : Logger, message, level, data) =
    if String.IsNullOrWhiteSpace message then nullArg "message"
    { message       = message
      level         = level
      data          = toMap data
      path          = logger.Name
      tags          = []
      ``exception`` = None
      timestamp     = NodaTime.SystemClock.Instance.Now }
    |> logger.Log
