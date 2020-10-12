namespace Logary.Trace

open Logary

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

  /// Clears the span status
  abstract unsetStatus: unit -> unit

  /// Sets the Span's status; anything other than OK will bump the level to Warn
  abstract setStatus: status: SpanStatusCode -> unit
  /// Sets the Span's status and a description; anything other than OK will bump the level to Warn
  abstract setStatus: status: SpanStatusCode * description: string -> unit
  /// Sets the Span's status and a description; anything other than OK will bump the level to Warn. Also allows setting
  /// WHO (infrastructure or user) is setting the span status.
  abstract setStatus: status: SpanStatusCode * description: string * source: SpanStatusSource -> unit

  /// Writes the flags for this Span
  abstract setFlags: setFlagCallback: (SpanFlags -> SpanFlags) -> unit

  // Primarily do addEvent via logger's methods; but you have it available here, too
  abstract addEvent: event: EventMessage -> unit

  // updateName / updateLabel via x.label <- "new label"

  abstract finish: ts:EpochNanoSeconds -> SpanMessage
  /// Call to stop the timer/Span. This method call is non-blocking.
  abstract finish: unit -> SpanMessage

/// A Span represents a single operation within a trace. Spans can be nested to form a trace tree. Each trace contains a root span, which typically describes the end-to-end latency and, optionally, one or more sub-spans for its sub-operations.
/// Reference: https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span
/// All Span's methods and properties are thread-safe to use/access/read.
type Span =
  inherit SpanOps
  inherit SpanMessage
  /// How long this Span has been in existence, or otherwise its total duration, if finished
  abstract elapsed: NodaTime.Duration
  /// Returns true if this Span is recording information like events with the AddEvent operation, attributes using
  /// SetAttributes, status with SetStatus, etc.
  /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#isrecording
  abstract isRecording: bool
  /// Setting this property, updates the Span name. Upon this update, any sampling behavior based on Span name will depend on the implementation.
  abstract setLabel: setLabelCallback: (string -> string) -> unit
  /// When this Span was finished
  abstract finished: EpochNanoSeconds option

