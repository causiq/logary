namespace Logary.Trace

open System
open System.Collections.Generic
open Logary

[<Struct>]
type TraceId =
  { high: int64
    low: int64 }
  member x.isZero = x.high = 0L && x.low = 0L
  static member Zero = { high=0L; low=0L }
  override x.ToString() =
    if x.high = 0L then String.Format("{0:x16}", x.low)
    else String.Format("{0:x16}{1:x16}", x.high, x.low)

[<Struct>]
type SpanId =
  { id: int64 }
  member x.isZero = x.id = 0L
  static member Zero = { id = 0L }
  override x.ToString() = String.Format("{0:x}", x.id)

[<Flags; RequireQualifiedAccess>]
type SpanFlags =
  | None = 0
  | Sampled = 1
  | Debug = 2

/// Value object with contextual tracing data.
///
/// https://opentracing.io/specification/#references-between-spans
/// https://www.jaegertracing.io/docs/1.13/client-libraries/#trace-span-identity
/// https://www.w3.org/TR/trace-context/#tracestate-field
[<Sealed>]
type SpanContext(traceId: TraceId, spanId: SpanId, ?flags: SpanFlags, ?parentSpanId: SpanId, ?traceState: Map<string, string>) =
  let flag = defaultArg flags SpanFlags.None
  let state = defaultArg traceState Map.empty
  let parentSpan = match parentSpanId with Some psId when psId <> SpanId.Zero -> Some psId | _ -> None
  let equal (other: SpanContext) =
    traceId = other.traceId && spanId = other.spanId && flag = other.flags && parentSpan = other.parentSpanId && state = other.traceState
  let currentHash = hash traceId ^^^ 307 * hash spanId ^^^ 307 * hash flags ^^^ 307 * hash parentSpanId ^^^ 307 * hash state
  member x.traceId = traceId
  member x.parentSpanId = parentSpan
  member x.spanId = spanId
  member x.flags = flag
  member x.traceState = state

  member x.isRootSpan = Option.isNone parentSpan
  member x.isRecorded = int flag > 0
  member x.isSampled = int flag > 0
  member x.isDebug = flag &&& SpanFlags.Debug = SpanFlags.Debug
  member x.isZero = traceId.isZero || spanId.isZero
  static member Zero = SpanContext(TraceId.Zero, SpanId.Zero)

  member x.withParent (context: SpanContext) =
    SpanContext(context.traceId, x.spanId, x.flags ||| context.flags, context.spanId)
  member x.withState (nextState: Map<string, string>) =
    SpanContext(x.traceId, x.spanId, x.flags, ?parentSpanId=x.parentSpanId, ?traceState=Some nextState)
  override x.ToString() =
    let keys =
      if Map.isEmpty x.traceState then "[]"
      else sprintf "[ %s ]" (x.traceState |> Seq.map (fun (KeyValue (k, _)) -> k) |> String.concat ", ")
    sprintf "SpanContext(isRootSpan=%b, isSampled=%b, isDebug=%b, traceId=%O, spanId=%O, parentSpanId=%O, traceState(keys)=%s)"
            x.isRootSpan x.isSampled x.isDebug x.traceId x.spanId x.parentSpanId keys
  override x.Equals(other) = match other with :? SpanContext as sc -> equal sc | _ -> false
  override x.GetHashCode() = currentHash
  interface IEquatable<SpanContext> with member x.Equals other = equal other


[<Struct; RequireQualifiedAccess>]
type SpanAttrValue =
  /// Booleans
  | B of boolean: bool
  /// Numbers
  | V of value: Value
  /// Strings
  | S of string: string

  member x.uriEncode () =
    match x with
    | SpanAttrValue.B true -> "true"
    | SpanAttrValue.B false -> "false"
    | SpanAttrValue.S s -> Uri.EscapeUriString s
    | SpanAttrValue.V v -> Uri.EscapeUriString (v.ToString())

type SpanAttr = string * SpanAttrValue
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
  /// Some parent Spans do not depend in any way on the result of their child Spans. In these cases, we say merely that the child Span FollowsFrom the parent Span in a causal sense. There are many distinct FollowsFrom reference sub-categories, and in future versions of OpenTracing they may be distinguished more formally.
  /// Here `TraceId` could be the equivalent of a CommandId as sent from an end-user client, intended to be
  /// atomically & consistently applied to the system.
  | FollowsFromTrace of predecessor: TraceId * attrs: (string * SpanAttrValue) list
  /// Some Spans cause their own "follow up" work. You can link to those predecessors with `FollowsFromSpan`
  | FollowsFromSpan of predecessor: SpanContext * attrs: (string * SpanAttrValue) list

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

/// Operations that are available on a Span
/// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span-operations
type SpanOps =
  // Wait with these ones
  // https://github.com/open-telemetry/opentelemetry-specification/issues/223
  // Returns the flag whether this span will be recorded. Returns true if this Span is active and recording information like events with the AddEvent operation and attributes using SetAttributes.
  //abstract isRecordingEvents: bool

  /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#add-links
  /// A Span MUST have the ability to record links to other Spans. Linked Spans can be from the same or a different trace. See Links description.
  abstract addLink: link: SpanLink -> unit
  /// An API to set a single Attribute where the attribute properties are passed as arguments. This MAY be called SetAttribute. To avoid extra allocations some implementations may offer a separate API for each of the possible value types.
  abstract setAttribute: key: string * value: Value -> unit
  /// An API to set a single Attribute where the attribute properties are passed as arguments. This MAY be called SetAttribute. To avoid extra allocations some implementations may offer a separate API for each of the possible value types.
  abstract setAttribute: key: string * value: bool -> unit
  /// An API to set a single Attribute where the attribute properties are passed as arguments. This MAY be called SetAttribute. To avoid extra allocations some implementations may offer a separate API for each of the possible value types.
  abstract setAttribute: key: string * value: string -> unit

  /// Sets the Span's status; anything other than OK will bump the level to Warn
  abstract setStatus: status: SpanCanonicalCode -> unit
  /// Sets the Span's status and a description; anything other than OK will bump the level to Warn
  abstract setStatus: status: SpanCanonicalCode * description: string -> unit

  /// Writes the flags for this Span
  abstract setFlags: setFlagCallback: (SpanFlags -> SpanFlags) -> unit

  // addEvent via logger's methods
  // updateName / updateLabel via x.label <- "new label"

  /// Call to stop the timer/Span. This method call is non-blocking, but executes the callback on the caller's thread.
  abstract finish: transform: (Message -> Message) -> unit
  /// Call to stop the timer/Span. This method call is non-blocking.
  abstract finish: unit -> unit

type SpanOpsAdvanced =
  /// Call to stop the timer/Span. The `transform` callback is not run until the LogResult is selected/awaited/run,
  /// and unless the `LogResult` is used, the `Message` will not be logged. This function is used internally to allow
  /// testing and ensuring the logging pipeline, end to end.
  abstract finishWithAck: transform: (Message -> Message) -> LogResult

type SpanKind =
  | Internal = 0
  | Client = 1
  | Server = 2

/// SpanData is readonly, so all its properties are thread-safe to access and share.
type SpanData =
  /// The Span's context. An API that returns the SpanContext for the given Span. The returned value may be used even after the Span is finished. The returned value MUST be the same for the entire Span lifetime. This MAY be called GetContext.
  abstract context: SpanContext
  // parentSpanId is in x.context.parentSpanId
  // resource = logger.name
  /// Is this Span a Client, Server or Internal Span value?
  abstract kind: SpanKind
  /// The name of the Span
  abstract label: string
  /// When this Span was started. The Span's start and end timestamps reflect the elapsed real time of the operation. A Span's start time SHOULD be set to the current time on span creation.
  abstract started: EpochNanoSeconds
  /// When this Span was finished
  abstract finished: EpochNanoSeconds
  /// How long this Span has been in existence, or otherwise its total duration, if finished
  abstract elapsed: NodaTime.Duration
  /// The Span's flags, e.g. whether to sample or debug this particular span.
  /// Logary-specific: this turns to Sampled if a Warn or Error event is logged into this Span.
  abstract flags: SpanFlags
  /// A list of Links to other Spans
  abstract links: IReadOnlyList<SpanLink>
  /// A set of events / Messages this span has seen
  abstract events: IReadOnlyList<Message>
  /// Gets the attributes of the Span
  abstract attrs: IReadOnlyDictionary<string, SpanAttrValue>
  /// Returns the Span's status
  abstract status: SpanStatus

/// A Span represents a single operation within a trace. Spans can be nested to form a trace tree. Each trace contains a root span, which typically describes the end-to-end latency and, optionally, one or more sub-spans for its sub-operations.
/// Reference: https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span
/// All Span's methods and properties are thread-safe to use/access/read.
type Span =
  inherit SpanOps
  inherit SpanOpsAdvanced
  inherit SpanData
  /// The operation name / Span label. After the Span is created, it SHOULD be possible to change the its name [label in this case].
  abstract label: string
  /// Setting this property, updates the Span name. Upon this update, any sampling behavior based on Span name will depend on the implementation.
  abstract setLabel: setLabelCallback: (string -> string) -> unit
  /// When this Span was finished
  abstract finished: EpochNanoSeconds option

type SpanLogger =
  inherit LoggerScope
  inherit Span

  /// Makes the `SpanLogger` log through to its Logger as well as keeping the events
  /// in the Span.
  abstract logThrough: unit -> unit

namespace Logary.Trace.Propagation

open Logary.Trace

/// Gets a (key: string, value: string[]) pair from some object-like data structure. Getters return a list of
/// headers; for the given key/prefix passed. It's up to each getter's implementation to decide whether to interpret
/// the passed string as a prefix (like in the case of Jaeger baggage) or as a complete string (all other cases)
type Getter<'t> =
  't
    -> (* key / prefix *) string
    -> (* all key-values pairs *) ( (* key *) string * (* values *) string list) list

/// Sets a (key: string, value: string[]) pair into some object-like data structure
type Setter<'t> = string * string list -> 't -> 't

/// A propagator is responsible for reading/writing into a carrier. A specific implementation of
/// a propagator may know the Jaeger `uber-trace-id` format, of the Zipkin B3 format or the W3C Trace Context format.
type Propagator =
  /// Extracts:
  /// - Span attributes (e.g. debug-id, correlation-id, request-id),
  /// - (distributed) Context attributes, and
  /// - the Span context from the carrier.
  abstract extract:
    getter: Getter<'t> * carrier: 't
      -> spanAttrs: SpanAttr list * spanContextO: SpanContext option
  /// Injects the given (distributed) context attributes and span context (parent id, trace-state) into the target carrier.
  abstract inject:
    setter: Setter<'t> * spanContext: SpanContext * target: 't
      -> 't
