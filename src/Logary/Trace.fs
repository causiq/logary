namespace Logary.Trace

open System
open System.Buffers
open System.Collections.Generic
open Logary

type TraceId = Id

[<Struct>]
type SpanId =
  { id: int64 }
  member x.isZero = x.id = 0L

  member x.toBase64String() =
    let bs = BitConverter.GetBytes(x.id) // https://docs.microsoft.com/en-us/dotnet/api/system.bitconverter?view=netcore-3.1
    if BitConverter.IsLittleEndian then Array.Reverse(bs) // macOS Catalina 64 is LE, convention: BE
    Convert.ToBase64String(bs)

  member x.toHexString() =
    String.Format("{0:x16}", x.id)

  static member Zero = { id = 0L }

  static member tryOfBase64String (s: string) =
    use bs = MemoryPool.Shared.Rent(8)
    let mutable written = 0
    if Convert.TryFromBase64String(s, bs.Memory.Span, &written) && written = 8 then
      let hS = bs.Memory.Span.Slice(0, 8)
      if BitConverter.IsLittleEndian then
        hS.Reverse()
      { id = BitConverter.ToInt64(Span<_>.op_Implicit(hS)) }
      |> Some
    else
      None

  static member ofBase64String (s: string) =
    match SpanId.tryOfBase64String s with
    | None ->
      SpanId.Zero
    | Some v ->
      v

  override x.ToString() = x.toBase64String()

[<Flags; RequireQualifiedAccess>]
type SpanFlags =
  | None = 0
  | Sampled = 1
  | Debug = 2

[<Struct>]
type TraceStateKey =
  TraceStateKey of k: string * v: string option
with
  member x.key =
    let (TraceStateKey (k, _)) = x in k
  member x.vendor =
    let (TraceStateKey (_, v)) = x in v
  member x.value =
    match x with
    | TraceStateKey (key, Some vendor) -> sprintf "%s@%s" key vendor
    | TraceStateKey (key, _) -> key
  override x.ToString() = x.value


type TraceState private (value: (TraceStateKey * string) list) =
  let build () =
    let hs = HashSet()
    let res = ResizeArray<_>(value.Length)
    for k, v in value do if hs.Add k then res.Add (k, v)
    List.ofSeq res

  let listing = lazy (build ())

  let equal (o: TraceState) =
    o.value = listing.Value

  member x.value = listing.Value

  /// Write appends at the head of the list of values that the TraceState contains; according to the spec when a new hop
  /// receives the request.
  member x.write k v =
    let nextList = listing.Value |> List.filter (fun (existingK, _) -> k <> existingK)
    (k, v) :: nextList
    |> TraceState

  member x.isZero = List.isEmpty value

  override x.Equals o =
    match o with
    | :? TraceState as ts -> equal ts
    | _ -> false

  override x.GetHashCode() = hash listing.Value

  override x.ToString() =
    if x.isZero then
      "TraceState([])"
    else
      listing.Value
        |> List.map fst
        |> List.map (sprintf "'%O'")
        |> String.concat ", "
        |> sprintf "TraceState(%s)"

  interface IEquatable<TraceState> with
    member x.Equals o = equal o

  static member empty = TraceState([])
  static member Zero = TraceState([])
  static member (+) (x: TraceState, (k, v)) = x.write k v

  static member ofList xs = TraceState(xs)
  static member ofSeq xs = TraceState(List.ofSeq xs)
  static member ofArray xs = TraceState(List.ofArray xs)

/// Value object with contextual tracing data.
///
/// https://opentracing.io/specification/#references-between-spans
/// https://www.jaegertracing.io/docs/1.13/client-libraries/#trace-span-identity
/// https://www.w3.org/TR/trace-context/#tracestate-field
/// https://w3c.github.io/correlation-context/
[<Sealed>]
type SpanContext(traceId: TraceId,
                 spanId: SpanId,
                 parentSpanId: SpanId option,
                 ?flags: SpanFlags,
                 ?traceState: TraceState,
                 ?traceContext: IReadOnlyDictionary<string, string>) =

  let flag = defaultArg flags SpanFlags.None
  let state = defaultArg traceState TraceState.empty
  let context = defaultArg traceContext (Map.empty :> _)
  let parentSpan = match parentSpanId with Some psId when psId <> SpanId.Zero -> Some psId | _ -> None
  let equal (other: SpanContext) =
       traceId = other.traceId
    && spanId = other.spanId
    && flag = other.flags
    && parentSpan = other.parentSpanId
    && state = other.traceState
    && context = other.traceContext
  let currentHash =
    hash traceId * 17
    + hash spanId * 17
    + hash flags * 17
    + hash parentSpanId * 17
    + hash state * 17
    + hash context

  member x.traceId = traceId
  member x.parentSpanId = parentSpan
  member x.spanId = spanId
  member x.flags = flag

  /// Primarily supported by (W3C tracing)[https://www.w3.org/TR/trace-context/#tracestate-header) and
  /// (OpenTelemetry)[https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#spancontext].
  /// Carries system-specific configuration data, represented as a list of key-value pairs. TraceState allows multiple
  /// tracing systems to participate in the same trace.
  member x.traceState = state

  /// User-supplied key-value pairs
  /// Jaeger: calls this baggage
  /// Logary: calls this context
  /// Zipkin: doesn't support it, but uses the term Baggage from Jaeger
  /// W3C: calls this correlation context
  member x.traceContext = context

  member x.isRootSpan = Option.isNone parentSpan
  member x.isRecorded = int flag > 0
  member x.isSampled = int flag > 0
  member x.isDebug = flag &&& SpanFlags.Debug = SpanFlags.Debug
  member x.isZero = traceId.isZero || spanId.isZero
  /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md
  member x.isValid = not (x.isZero)
  static member Zero = SpanContext(TraceId.Zero, SpanId.Zero, None)

  member x.withSpanId(spanId: SpanId) =
    SpanContext(x.traceId, spanId, x.parentSpanId, x.flags, x.traceState, x.traceContext)

  member x.withTraceId(traceId: TraceId) =
    SpanContext(traceId, x.spanId, x.parentSpanId, x.flags, x.traceState, x.traceContext)

  member x.withParent (context: SpanContext) =
    SpanContext(context.traceId, x.spanId, Some context.spanId, x.flags ||| context.flags, x.traceState, x.traceContext)

  member x.withParentId (parentId: SpanId option) =
    SpanContext(x.traceId, x.spanId, parentId, x.flags, x.traceState, x.traceContext)

  member x.withState (nextState: TraceState) =
    SpanContext(x.traceId, x.spanId, x.parentSpanId, x.flags, nextState, x.traceContext)

  member x.withContext (nextContext: IReadOnlyDictionary<string, string>) =
    SpanContext(x.traceId, x.spanId, x.parentSpanId, x.flags, x.traceState, nextContext)

  member x.withFlags nextFlags =
    SpanContext(x.traceId, x.spanId, x.parentSpanId, nextFlags, x.traceState, x.traceContext)

  override x.ToString() =
    let ctxKeys =
      if x.traceContext.Count = 0 then "[]"
      else sprintf "[ %s ]" (x.traceContext |> Seq.map (fun (KeyValue (k, _)) -> sprintf "'%s'" k) |> String.concat ", ")
    sprintf "SpanContext(isRootSpan=%b, flags={%O}, isDebug=%b, traceId=%O, spanId=%O, parentSpanId=%O, %O, TraceContext(%s))"
            x.isRootSpan x.flags x.isDebug x.traceId x.spanId x.parentSpanId x.traceState ctxKeys
  override x.Equals(other) = match other with :? SpanContext as sc -> equal sc | _ -> false
  override x.GetHashCode() = currentHash
  interface IEquatable<SpanContext> with member x.Equals other = equal other


type SpanAttr = KeyValuePair<string, Value>

type ContextAttr = string * string

/// A Span may reference zero or more other Spans or Traces that are causally related.
///
/// > OpenTracing presently defines two types of references: ChildOf and FollowsFrom.
/// > Both reference types specifically model direct causal relationships between a child Span and a parent Span.
/// > In the future, OpenTracing may also support reference types for Spans with non-causal relationships
/// > (e.g., Spans that are batched together, Spans that are stuck in the same queue, etc).
///
/// https://opentracing.io/specification/#references-between-spans
/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#add-Links
///
/// It would seem we're not really clear on what sort of links we actually want. Follow this:
/// https://github.com/open-telemetry/opentelemetry-specification/issues/207
///
/// Parent-Child relationships are tracked in SpanContext, not via links.
type SpanLink =
  /// Some parent Spans do not depend in any way on the result of their child Spans. In these cases, we say merely that
  /// the child Span FollowsFrom the parent Span in a causal sense. There are many distinct FollowsFrom reference
  /// sub-categories, and in future versions of OpenTracing they may be distinguished more formally.
  /// Here `TraceId` could be the equivalent of a CommandId as sent from an end-user client, intended to be
  /// atomically & consistently applied to the system.
  | FollowsFromTrace of predecessor: TraceId * attrs: IReadOnlyDictionary<string, Value>
  /// Some Spans cause their own "follow up" work. You can link to those predecessors with `FollowsFromSpan`
  | FollowsFromSpan of predecessor: SpanContext * attrs: IReadOnlyDictionary<string, Value>

/// https://github.com/grpc/grpc/blob/master/doc/statuscodes.md
/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#statuscanonicalcode
[<RequireQualifiedAccess>]
type SpanCanonicalCode =
  /// The operation completed successfully.
  | OK = 0
  /// The operation was cancelled (typically by the caller).
  | Cancelled = 1
  /// Unknown error. For example, this error may be returned when a Status value received from another address space belongs to an error space that is not known in this address space. Also errors raised by APIs that do not return enough error information may be converted to this error.
  | UnknownError = 2
  /// Client specified an invalid argument. Note that this differs from FailedPrecondition. InvalidArgument indicates arguments that are problematic regardless of the state of the system.
  | InvalidArgument = 3
  /// Deadline expired before operation could complete. For operations that change the state of the system, this error may be returned even if the operation has completed successfully.
  | DeadlineExceeded = 4
  /// Some requested entity (e.g., file or directory) was not found.
  | NotFound = 5
  /// Some entity that we attempted to create (e.g., file or directory) already exists.
  | AlreadyExists = 6
  /// The caller does not have permission to execute the specified operation. PermissionDenied must not be used if the caller cannot be identified (use Unauthenticated1 instead for those errors).
  | PermissionDenied = 7
  /// Some resource has been exhausted, perhaps a per-user quota, or perhaps the entire file system is out of space.
  | ResourceExhausted = 8
  /// Operation was rejected because the system is not in a state required for the operation's execution.
  | FailedPrecondition = 9
  /// The operation was aborted, typically due to a concurrency issue like sequencer check failures, transaction aborts, etc.
  | Aborted = 10
  /// Operation was attempted past the valid range. E.g., seeking or reading past end of file. Unlike InvalidArgument, this error indicates a problem that may be fixed if the system state changes.
  | OutOfRange = 11
  /// Operation is not implemented or not supported/enabled in this service.
  | Unimplemented = 12
  /// Internal errors. Means some invariants expected by underlying system has been broken.
  | InternalError = 13
  /// The service is currently unavailable. This is a most likely a transient condition and may be corrected by retrying with a backoff.
  | Unavailable = 14
  /// Unrecoverable data loss or corruption.
  | DataLoss = 15
  /// The request does not have valid authentication credentials for the operation.
  | Unauthenticated = 16

type SpanStatus = SpanCanonicalCode * string option

/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/trace/api.md
/// | `SpanKind` | Synchronous | Asynchronous | Remote Incoming | Remote Outgoing |
/// |------------|-------------|--------------|-----------------|-----------------|
/// | `CLIENT`   | yes         |              |                 | yes             |
/// | `SERVER`   | yes         |              | yes             |                 |
/// | `PRODUCER` |             | yes          |                 | maybe           |
/// | `CONSUMER` |             | yes          | yes             | maybe           |
/// | `INTERNAL` |             |              |                 |                 |
type SpanKind =
  /// Default value. Indicates that the span represents an internal operation within an application, as opposed to an operations with remote parents or children.
  | Internal
  /// Indicates that the span describes a synchronous request to some remote service. This span is the parent of a remote SERVER span and waits for its response.
  | Client
  ///  Indicates that the span covers server-side handling of a synchronous RPC or other remote request. This span is the child of a remote CLIENT span that was expected to wait for a response.
  | Server
  ///  Indicates that the span describes the child of an __asynchronous__ PRODUCER request.
  | Consumer
  /// Indicates that the span describes the parent of an __asynchronous__ request. This parent span is expected to end before the corresponding child CONSUMER span, possibly even before the child span starts. In messaging scenarios with batching, tracing individual messages requires a new PRODUCER span per message to be created.
  | Producer

  member x.asInt =
    match x with
    | Internal -> 0
    | Client -> 1
    | Server -> 2
    | Consumer -> 3
    | Producer -> 4

  member x.next =
    match x with
    | SpanKind.Client -> SpanKind.Server
    | SpanKind.Server -> SpanKind.Internal
    | SpanKind.Internal -> SpanKind.Internal
    | SpanKind.Producer -> SpanKind.Internal
    | SpanKind.Consumer -> SpanKind.Internal

  static member ofInt i =
    match i with
    | 1 -> SpanKind.Client
    | 2 -> SpanKind.Server
    | 3 -> SpanKind.Consumer
    | 4 -> SpanKind.Producer
    | _ | 0 -> SpanKind.Internal
