namespace Logary

open Logary.Trace
open System.Collections.Generic

[<RequireQualifiedAccess>]
type MessageKind =
  | Control
  | Event
  | Span
  | Gauge
  | Histogram
  | IdentifyUser
  | SetUserProperty

  override x.ToString() =
    match x with
    | Control -> "control"
    | Event -> "event"
    | Span -> "span"
    | Gauge -> "gauge"
    | Histogram -> "histogram"
    | IdentifyUser -> "identifyUser"
    | SetUserProperty -> "setUserProperty"

type LogaryMessage =
  abstract kind: MessageKind
  /// This is an id for the LogaryMessage; it's different from the SpanId (64 bits) and TraceId (128 bits, generated
  /// remotely).
  abstract id: Id
  abstract name: PointName
  abstract level: LogLevel
  /// When did the Logary infrastructure first start processing this event? This field is much more trusted than the
  /// `timestamp` field.
  abstract received: EpochNanoSeconds option
  abstract timestamp: EpochNanoSeconds
  abstract context: IReadOnlyDictionary<string, Value>
  abstract fields: IReadOnlyDictionary<string, Value>
  abstract gauges: IReadOnlyDictionary<string, Gauge>

  /// Optional SpanId; useful for streaming SDK implementations; is overridden in SpanMessage to point to the
  /// SpanContext within.
  abstract parentSpanId: SpanId option

[<RequireQualifiedAccess>]
type ControlMessageKind =
  | BufferFull of target: string
  | ClientAborted
  | RegistryClosed
  | Aggregated

/// Internal messages; don't accept from the network.
type ControlMessage =
  inherit LogaryMessage
  abstract ckind: ControlMessageKind
  abstract children: ControlMessage[]

type EventMessage =
  inherit LogaryMessage
  abstract event: string
  abstract monetaryValue: Money option
  abstract error: ErrorInfo option

type GaugeMessage =
  inherit LogaryMessage
  abstract gauge: Gauge
  /// The labels this Gauge "is for"; like its name.
  abstract labels: IReadOnlyDictionary<string, string>

type HistogramMessage =
  inherit LogaryMessage
  /// A mapping between bucket upper bounds, inclusive, and its count
  abstract buckets: IReadOnlyDictionary<float, float>
  /// The labels this Histogram "is for"; like its name.
  abstract labels: IReadOnlyDictionary<string, string>
  /// Total sum of observed values
  abstract sum: float

/// SpanMessage is readonly, so all its properties are thread-safe to access and share. Also see SpanImpl for a DTO value.
type SpanMessage =
  inherit LogaryMessage
  /// The name of the Span; this shows what the Span denotes.
  abstract label: string
  /// The Span's context. An API that returns the SpanContext for the given Span. The returned value may be used even after the Span is finished. The returned value MUST be the same for the entire Span lifetime. This MAY be called GetContext.
  abstract context: SpanContext
  // parentSpanId is in x.context.parentSpanId
  // resource = logger.name
  /// Is this Span a Client, Server or Internal Span value?
  abstract kind: SpanKind
  /// When this Span was started. The Span's start and end timestamps reflect the elapsed real time of the operation. A Span's start time SHOULD be set to the current time on span creation.
  abstract started: EpochNanoSeconds
  /// When this Span was finished
  abstract finished: EpochNanoSeconds
  /// The Span's flags, e.g. whether to sample or debug this particular span.
  /// Logary-specific: this turns to Sampled if a Warn or Error event is logged into this Span.
  abstract flags: SpanFlags
  /// A list of Links to other Spans
  abstract links: IReadOnlyList<SpanLink>
  /// A set of events / Messages this span has seen
  abstract events: IReadOnlyList<EventMessage>
  /// Gets the attributes of the Span; this is the same as the fields of the LogaryMessage.
  abstract attrs: IReadOnlyDictionary<string, Value>
  /// Returns the Span's status
  abstract status: SpanStatus

/// Lets you identify a user by associating two user identities together. Useful when you generate a random UserId when
/// the user first visits the app/site and then want to correlate that UserId with a logged-in-UserId.
type IdentifyUserMessage =
  inherit LogaryMessage
  abstract prevUserId: string
  abstract newUserId: string

/// Sets a user-level property.
type SetUserPropertyMessage =
  inherit LogaryMessage
  abstract key: string
  abstract value: Value
