namespace Logary.Ingestion

open System
open System.Text
open Logary
open Logary.Message
open Hopac

/// An ingested value. Either a string or a byte array.
[<Struct>]
type Ingested =
  /// See https://github.com/fsharp/fslang-suggestions/issues/648
  /// Useful for Protobuf/gRPC ingestion points
  | Bytes of bs:ArraySegment<byte> // TO CONSIDER: slices
  /// Useful for JSON/HTTP/TCP-lines ingestion points
  | String of s:string
  /// Create a new Ingested value from a string
  static member ofString s =
    if isNull s then nullArg "s"
    String s
  /// Create a new Ingested value from the ArraySegment passed
  static member ofArraySegment bs =
    Bytes bs
  /// Create a new Ingested value from the byte array
  static member ofBytes (bs: byte[]) =
    if isNull bs then nullArg "bs"
    Bytes (ArraySegment bs)

  /// If a string, returns the string; or if array segment, tries to get the string
  /// from that array segment; ensure there are no half-characters in the array,
  /// or you'll get back a broken string value.
  member x.utf8String() =
    match x with
    | Bytes bs -> Encoding.UTF8.GetString(bs.Array, bs.Offset, bs.Count)
    | String s -> s

/// Callback when there are UDP packets available.
/// TO CONSIDER: `'err` instead of string.
type Ingest = Ingested -> Job<Result<unit, string>>

//module Ingest =
//  let withCodec (next: Logger): Ingest =
//    fun input ->
//        next.verbose (eventX "JFail: {line} => {failure}" >> setField "line" line >> setField "failure" failure)
//        let jsonFail =
//        Job.result (Result.Error jsonFail)