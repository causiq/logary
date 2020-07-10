namespace Logary.Ingestion

open System.Buffers
open System.Collections.Generic
open System.Net
open System.Text
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals

/// An ingested value. Either a string or a byte array.
[<Struct; RequireQualifiedAccess>]
type Ingested =
  /// See https://github.com/fsharp/fslang-suggestions/issues/648
  /// Useful for Protobuf/gRPC ingestion points
  | Bytes of bs: byte[]
  | ByteSeq of ros: ReadOnlySequence<byte>
  /// Useful for JSON/HTTP/TCP-lines ingestion points
  | String of s: string
  /// Create a new Ingested value from a string
  static member ofString s =
    if isNull s then nullArg "s"
    String s
  /// Create a new Ingested value from the ReadOnlySequence passed
  static member ofBytes bs =
    Bytes bs
  /// Create a new Ingested value from the byte array
  static member ofReadOnlySeq (bs: ReadOnlySequence<byte>) =
    ByteSeq bs
  static member forceBytes = function
    | Bytes bs -> bs
    | ByteSeq r -> r.ToArray()
    | String s -> failwithf "Unexpected string '%s' in Ingested, when ReadOnlySequence<byte> was expected" s

  /// If a string, returns the string; or if array segment, tries to get the string
  /// from that array segment; ensure there are no half-characters in the array,
  /// or you'll get back a broken string value.
  member x.utf8String() =
    match x with
    | ByteSeq bs -> Strings.parseAsUTF8 bs
    | Bytes bs -> Encoding.UTF8.GetString bs
    | String s -> s

/// Callback when there are packets available.
/// TO CONSIDER: `'err` instead of string.
type Ingest = Ingested -> Job<Result<unit, string>>


type Scheme = Scheme of scheme: string
type NIC =
  NIC of nic: string
with
  member x.asIPAddress =
    let (NIC n) = x in IPAddress.Parse n

type Port = Port of port: uint16

type Binding =
  Binding of Scheme * NIC * Port
with
  member x.withScheme s =
    let (Binding (_, n, p)) = x
    Binding (Scheme s, n, p)

  member x.nicAndPort =
    let (Binding (_, NIC n, Port p)) = x
    sprintf "%s:%i" n p

  member x.asEndpoint =
    let (Binding (_, NIC n, Port p)) = x
    IPEndPoint(IPAddress.Parse(n), int p)

  override x.ToString() =
    let (Binding (Scheme s, NIC n, Port p)) = x
    sprintf "%s://%s:%i" s n p

  interface IValueFormattable with
    member x.toKeyValues baseKey =
      Choice1Of2(KeyValuePair<_,_>(baseKey, Value.Str (x.ToString())))

  static member create(scheme, nic, port) =
    Binding (scheme, NIC nic, Port port)
  static member create(scheme, nic, port) =
    Binding (Scheme scheme, NIC nic, Port port)

type BindingList =
  BindingList of bindings: Binding list
with
  member x.Length =
    let (BindingList bs) = x
    bs.Length

  member x.toCommaSeparatedString() =
    let (BindingList bs) = x in bs
      |> List.map (fun b -> b.ToString())
      |> String.concat ", "

  static member create (bs: #seq<_>) =
    BindingList (List.ofSeq bs)
  interface IEnumerable<Binding> with
    member x.GetEnumerator() =
      let (BindingList bs) = x
      (bs :> IEnumerable<_>).GetEnumerator()
  interface System.Collections.IEnumerable with
    member x.GetEnumerator() =
      (x :> IEnumerable<Binding>).GetEnumerator() :> _
  interface IValueFormattable with
    member x.toKeyValues baseKey =
      let value = x.toCommaSeparatedString() |> Value.Str
      Choice1Of2 (KeyValuePair(baseKey, value))

type IngestServerConfig =
  abstract cancelled: Promise<unit>
  abstract ilogger: Logger
  abstract bindings: BindingList

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