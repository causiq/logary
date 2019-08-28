namespace Logary.Ingestion

open System
open System.Text
open Hopac
open Hopac.Infixes
open Logary

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
  static member forceBytes = function
    | Bytes bs -> bs
    | String s -> failwithf "Unexpected string '%s' in Ingested, when byte[] was expected" s

  /// If a string, returns the string; or if array segment, tries to get the string
  /// from that array segment; ensure there are no half-characters in the array,
  /// or you'll get back a broken string value.
  member x.utf8String() =
    match x with
    | Bytes bs -> Encoding.UTF8.GetString(bs.Array, bs.Offset, bs.Count)
    | String s -> s

/// Callback when there are packets available.
/// TO CONSIDER: `'err` instead of string.
type Ingest = Ingested -> Job<Result<unit, string>>

type IngestServerConfig =
  abstract cancelled: Promise<unit>
  abstract ilogger: Logger

type IngestServer =
  private {
    started: Promise<unit>
    shutdown: Promise<unit>
  }

type ServerFactory<'config when 'config :> IngestServerConfig> =
  'config -> Ingest -> Job<IngestServer>

module IngestServer =
  /// Create a new ingest server. The passed `recv` function is asynchronously queued to run with Hopac. In other words,
  /// executing the returned job doesn't block on `recv` completing. In fact, the contrary is true; this code assumes
  /// that `recv` will block for as long as the loop as running, and then feed any exceptions to the `shutdown`
  /// `IVar<unit>` (second tuple item) that it is passed.
  ///
  /// This function is responsible for instantiating the `IVar<unit>` values to be passed to `recv`, thereby freeing
  /// implementors of doing that manually.
  ///
  /// Returns a `ServerFactory<_>`.
  let create (recv: (_*_) -> 'config -> Ingest -> Job<unit>): ServerFactory<_> =
    fun (config: 'config) (next: Ingest) ->
      let started, shutdown as signals = IVar (), IVar ()
      Job.queue (recv signals config next)
      >>-. { started=started; shutdown=shutdown }
      
  let waitForStart x = x.started
  let waitForShutdown x = x.shutdown