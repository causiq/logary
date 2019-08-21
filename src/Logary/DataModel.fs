namespace Logary

open Hopac
open NodaTime
open System
open System.Collections.Generic
open Logary

/// The main alias for time in Logary - the # of nanoseconds since 1970-01-01
/// in Unix time. In effect it denotes the # of nanoseconds passed in
/// international atomic time (TAI), but corrected for leap seconds – because
/// this is how system clocks normally work.
type EpochNanoSeconds = int64

type Units =
  | Bits
  | Bytes
  | Seconds
  | Metres
  | Scalar
  | Amperes
  | Kelvins
  | Moles
  | Candelas
  | Percent
  | Watts
  | Hertz
  | Joules
  | Grams
  | Other of unit:string
  // E.g. to denote nano-seconds since epoch;
  // 1474139353507070000 would be Scaled(Seconds, 10.**9.) since year 1970
  // so to get back to seconds, you'd divide the value by 10.**9.
  // E.g. an op that takes 5ms would be represented as
  // Gauge(5000000, Scaled(Seconds, 10.**9.)) (ns) OR:
  // Gauge(50000, Scaled(Seconds, 10**7.)) (ticks):
  | Scaled of unit:Units * value:float
  | Offset of unit:Units * value:float
  | Mul of unitA:Units * unitB:Units
  | Pow of ``base``:Units * power:float
  | Div of nom:Units * denom:Units
  | Root of ``base``:Units
  | Log10 of ``base``:Units // Log of base:float * BaseUnit

  /// E.g. 5 degrees celsius is (5 + 273.15) K
  static member Celsius = Offset (Kelvins, +273.15)

  static member SquareMetres = Pow (Metres, 2.)
  static member KiloGrams = Scaled (Grams, 1.0e-3)
  /// https://en.wikipedia.org/wiki/Newton_(unit)
  static member Newtons = Div (Mul (Units.KiloGrams, Metres), Pow (Seconds, 2.))
  /// https://en.wikipedia.org/wiki/Pascal_(unit)
  static member Pascal = Div (Units.Newtons, Units.SquareMetres)

  /// 5 min = 5 / (1/60) seconds = 360 s
  static member Minutes = Scaled (Seconds, 1. / 60.)
  static member Hours = Scaled (Seconds, 1. / 3600.)
  static member Days = Scaled (Seconds, 1. / (24. * 3600.))

  member x.name: string option =
    match x with
    | Bits ->
      Some "bits"
    | Bytes ->
      Some "bytes"
    | Seconds ->
      Some "seconds"
    | Metres ->
      Some "metres"
    | Scalar ->
      Some "units"
    | Amperes ->
      Some "amperes"
    | Kelvins ->
      Some "kelvins"
    | Joules ->
      Some "Joules"
    | Grams ->
      Some "Grams"
    | Moles ->
      Some "moles"
    | Candelas ->
      Some "candelas"
    | Percent ->
      Some "percent"
    | Watts ->
      Some "watts"
    | Hertz ->
      Some "hertz"
    | Other u ->
      Some u
    | Scaled _
    | Offset _
    | Mul _
    | Pow _
    | Div _
    | Root _
    | Log10 _ ->
      None

  member x.symbol =
    match x with
    | Bits -> "bit"
    | Bytes -> "B"
    | Seconds -> "s"
    | Metres -> "m"
    | Scalar -> ""
    | Amperes -> "A"
    | Kelvins -> "K"
    | Joules -> "J"
    | Grams -> "g"
    | Moles -> "mol"
    | Candelas -> "cd"
    | Percent -> "%"
    | Watts -> "W"
    | Hertz -> "Hz"
    | Other other -> other
    | Scaled (units, scale) -> sprintf "%s/%f" units.symbol scale
    | Offset (units, offset) ->
      sprintf "%s %s %f" units.symbol (if offset < 0. then "-" else "+") offset
    | Mul (a, b) -> String.Concat [ "("; a.symbol; "*"; b.symbol; ")" ]
    | Pow (a, b) -> String.Concat [ a.symbol; "^("; string b; ")" ]
    | Div (a, b) -> String.Concat [ a.symbol; "/"; b.symbol ]
    | Root a -> String.Concat [ "sqrt("; a.symbol; ")" ]
    | Log10 a -> String.Concat [ "log10("; a.symbol; ")" ]

[<Struct>]
type PointName =
  PointName of hierarchy:string[]
with
  member x.isEmpty =
    match x with
    | PointName xs when xs.Length = 0 -> true
    | _ -> false

  override x.ToString() =
    let (PointName hiera) = x in String.concat "." hiera

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PointName =

  let empty = PointName Array.empty

  let (|Empty|_|) (pn: PointName) =
    if pn.isEmpty then Some () else None

  [<CompiledName "OfSingle">]
  let ofSingle (segment: string) =
    PointName [| segment |]

  [<CompiledName "OfList">]
  let ofList (hiera: string list) =
    PointName (Array.ofList hiera)

  /// Creates a point name of the array. As a performance optimisation, Logary
  /// assumes that you do not change the array contents afterwards, and will
  /// treat this value like an immutable value.
  [<CompiledName "OfArray">]
  let ofArray (hiera: string[]) =
     PointName hiera

  [<CompiledName "Parse">]
  let parse (s: string) =
    String.splita '.' s |> ofArray

  [<CompiledName "Format">]
  let format (pn: PointName) =
    pn.ToString()

  [<CompiledName "SetEnding">]
  let setEnding (nameEnding: string) (PointName segments as original) =
    if String.IsNullOrWhiteSpace nameEnding then original else
    PointName (Array.append segments [| nameEnding |])

/// Allows you to clearly delineate the accuracy and type of the measurement/gauge.
[<Struct>]
type Value =
  /// A CLR Double / F# float represented as a DU case
  | Float of float:float
  /// A CLR Int64 / F# int64 represented as a DU case
  | Int64 of int64:int64
  | BigInt of bigint:bigint
  | Fraction of numerator:int64 * denominator:int64
  /// Convert the Gauge value to a float (best as possible; this **may** lead to
  /// a loss of accuracy).
  member x.toFloat () =
    match x with
    | Float f -> f
    | Int64 i -> float i
    | BigInt i -> float i
    | Fraction (n, d) -> float n / float d

[<Struct>]
type Gauge =
  Gauge of Value * Units
with
  member x.value =
    let (Gauge (v, _)) = x in v
  member x.unit =
    let (Gauge (_, u)) = x in u
  static member ofNanos (ns: Value) =
    Gauge (ns, Scaled (Seconds, float Constants.NanosPerSecond))
  static member ofNanos (ns: int64) =
    Gauge (Int64 ns, Scaled (Seconds, float Constants.NanosPerSecond))
  static member ofNanos (ns: float) =
    Gauge (Float ns, Scaled (Seconds, float Constants.NanosPerSecond))
  static member ofMillis (ms: Value) =
    Gauge (ms, Scaled (Seconds, float Constants.MillisPerSecond))
  static member ofMillis (ms: float) =
    Gauge (Float ms, Scaled (Seconds, float Constants.MillisPerSecond))
  static member ofMillis (ms: int64) =
    Gauge (Int64 ms, Scaled (Seconds, float Constants.MillisPerSecond))
  static member ofBclTicks (bclTicks: Value) =
    Gauge (bclTicks, Scaled (Seconds, float Constants.TicksPerSecond))
  static member ofBclTicks (bclTicks: int64) =
    Gauge (Int64 bclTicks, Scaled (Seconds, float Constants.TicksPerSecond))
  static member ofBclTicks (bclTicks: float) =
    Gauge (Float bclTicks, Scaled (Seconds, float Constants.TicksPerSecond))
  static member ofStopwatchTicks (swTicks: Value) =
    Gauge (swTicks, Scaled (Seconds, float System.Diagnostics.Stopwatch.Frequency))
  static member ofStopwatchTicks (swTicks: int64) =
    Gauge (Int64 swTicks, Scaled (Seconds, float System.Diagnostics.Stopwatch.Frequency))
  static member ofStopwatchTicks (swTicks: float) =
    Gauge (Float swTicks, Scaled (Seconds, float System.Diagnostics.Stopwatch.Frequency))

/// This is record that is logged.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse message template in message.value
    name: PointName
    /// Event (template or raw message) E.g. "{user} logged in"
    value: string
    /// Where in the code? Who did the operation? What tenant did the principal
    /// who did it belong to? ... context can be anything, you can decide how to deal with them in target
    /// through its key.
    context: HashMap<string, obj>
    /// How important? See the docs on the LogLevel type for details.
    level: LogLevel
    /// When? The # of nanoseconds since the UNIX epoch (1970-01-01T00:00:00Z)
    timestamp: EpochNanoSeconds }

    /// Gets the timestamp as NodaTime ticks (100 ns per tick). If you're getting
    /// for DateTime and/or DateTimeOffset, remember that those start at
    /// 0001-01-01; use the functions on the DateTime/DateTimeOffset modules then instead.
    member x.timestampTicks: int64 =
      x.timestamp / Constants.NanosPerTick
    /// The # of seconds since UNIX epoch 1970-01-01T00:00:00Z
    member x.timestampEpochS: int64 =
      x.timestamp / Constants.NanosPerSecond


/// Patterns to match against the context; useful for extracting the data
/// slightly more semantically than "obj"-everything. Based on the known prefixes
/// in `KnownLiterals`.
module MessagePatterns =
  open KnownLiterals

  /// Pattern match the key
  let (|Intern|Field|Gauge|Tags|Context|) (KeyValue (key: string, value: obj)) =
    match key with
    | _ when key = TagsContextName ->
      let tags = unbox<Set<string>> value
      Tags tags

    | _ when key.StartsWith FieldsPrefix ->
      let k = key.Substring FieldsPrefix.Length
      Field (k, value)

    | _ when key.Equals(DefaultGaugeName, StringComparison.InvariantCulture) ->
      Gauge (String.Empty, unbox<Gauge> value)

    | _ when key.StartsWith GaugeNamePrefix ->
      let k = key.Substring GaugeNamePrefix.Length
      Gauge (k, unbox<Gauge> value)

    | _ when key.StartsWith LogaryPrefix ->
      Intern

    | _ ->
      match value with
      | :? Gauge as g ->
        Gauge (key, g)
      | _ ->
        Context (key, value)

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
  override x.ToString() = String.Format("{0:x16}", x.id)

[<Flags; RequireQualifiedAccess>]
type SpanFlags =
  | None = 0
  | Sampled = 1
  | Debug = 2

/// https://www.jaegertracing.io/docs/1.13/client-libraries/#trace-span-identity
/// TO CONSIDER: https://www.w3.org/TR/trace-context/#tracestate-field
/// TO CONSIDER: codecs from binary/http, etc...
[<Sealed>]
type SpanContext(traceId: TraceId, spanId: SpanId, ?flags: SpanFlags, ?parentSpanId: SpanId) =
  let flag = defaultArg flags SpanFlags.None
  member x.traceId = traceId
  member x.parentSpanId = parentSpanId
  member x.spanId = spanId
  member x.flags = flag

  member x.isRootSpan = Option.isNone parentSpanId
  member x.isRecorded = int flag > 0
  member x.isSampled = int flag > 0
  member x.isDebug = flag &&& SpanFlags.Debug = SpanFlags.Debug

  member x.withParent (context: SpanContext) =
    new SpanContext(context.traceId, x.spanId, x.flags ||| context.flags, context.spanId)

type SpanLink = SpanContext * IReadOnlyList<KeyValuePair<string, string>>

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
  abstract addLink: SpanLink -> unit
  /// An API to set a single Attribute where the attribute properties are passed as arguments. This MAY be called SetAttribute. To avoid extra allocations some implementations may offer a separate API for each of the possible value types.
  abstract setAttribute: string * Value -> unit
  /// An API to set a single Attribute where the attribute properties are passed as arguments. This MAY be called SetAttribute. To avoid extra allocations some implementations may offer a separate API for each of the possible value types.
  abstract setAttribute: string * bool -> unit
  /// An API to set a single Attribute where the attribute properties are passed as arguments. This MAY be called SetAttribute. To avoid extra allocations some implementations may offer a separate API for each of the possible value types.
  abstract setAttribute: string * string -> unit

  /// Sets the Span's status; anything other than OK will bump the level to Warn
  abstract setStatus: SpanCanonicalCode -> unit
  /// Sets the Span's status and a description; anything other than OK will bump the level to Warn
  abstract setStatus: SpanCanonicalCode * string -> unit

  // addEvent via logger's methods
  // updateName / updateLabel via x.label <- "new label"

  /// Call to stop the timer/Span. This method call is non-blocking, but executes the callback on the caller's thread.
  abstract finish: (Message -> Message) -> unit
  /// Call to stop the timer/Span. This method call is non-blocking.
  abstract finish: unit -> unit

  // Logary-specific:
  abstract setFlags: SpanFlags -> unit

type SpanKind =
  | Internal = 0
  | Client = 1
  | Server = 2

type SpanData =
  /// The Span's context. An API that returns the SpanContext for the given Span. The returned value may be used even after the Span is finished. The returned value MUST be the same for the entire Span lifetime. This MAY be called GetContext.
  abstract context: SpanContext
  // parentSpanId is in x.context.parentSpanId
  // resource = logger.name

  abstract kind: SpanKind

  /// The name of the Span
  abstract label: string
  /// When this Span was started. The Span's start and end timestamps reflect the elapsed real time of the operation. A Span's start time SHOULD be set to the current time on span creation.
  abstract started: EpochNanoSeconds
  /// When this Span was finished
  abstract finished: EpochNanoSeconds
  /// How long this Span has been in existence, or otherwise its total duration, if finished
  abstract elapsed: Duration
  /// The Span's flags, e.g. whether to sample or debug this particular span.
  /// Logary-specific: this turns to Sampled if a Warn or Error event is logged into this Span.
  abstract flags: SpanFlags
  /// A list of Links to other Spans
  abstract links: IReadOnlyList<SpanLink>
  /// A set of events / Messages this span has seen
  abstract events: IReadOnlyList<Message>
  /// Returns the Span's status
  abstract status: SpanStatus

/// A Span represents a single operation within a trace. Spans can be nested to form a trace tree. Each trace contains a root span, which typically describes the end-to-end latency and, optionally, one or more sub-spans for its sub-operations.
/// Reference: https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/api-tracing.md#span
type Span =
  inherit SpanOps
  inherit SpanData
  /// The operation name / Span label. After the Span is created, it SHOULD be possible to change the its name [label in this case].
  abstract label: string with get
  /// Setting this property, updates the Span name. Upon this update, any sampling behavior based on Span name will depend on the implementation.
  abstract setLabel: (string -> string) -> unit
  /// When this Span was finished
  abstract finished: EpochNanoSeconds option

type internal ProcessResult = Result<Promise<unit>, Message>
type internal LogResult = Alt<ProcessResult>

/// See the docs on the functions for descriptions on how Ack works in conjunction
/// with the Promise.
type Logger =
  /// The PointName for this `Logger`: corresponds to the `name` field for the
  /// `Messages` produced from this instance.
  ///
  /// To use a car metaphor, this could be Fleet.Cars.Volvo.X90 and you could then
  /// add gauges for each sub-measurement/sensor; like Engine.RPM or Engine.Temperature.
  ///
  /// This field is NOT the same as the Span operation/label/name, instead see `Span.label` for that.
  abstract name: PointName

  /// Returns an Alt that commits on ALL N Targets' buffers accepting the message.
  /// Even if the Alt was not committed to, one or more targets (fewer than N) may have accepted the message.
  /// And this can not be canceled.
  ///
  /// - `waitForBuffers`: `true` means waiting for each target buffer to be available to take the message.
  ///   `false` means it will try to detect whether the buffer is full and return immediately.
  abstract logWithAck: waitForBuffers:bool * level:LogLevel -> messageFactory:(LogLevel -> Message) -> LogResult

  /// Gets the currently set log level (minimal,inclusive),
  /// aka. the granularity with which things are being logged.
  abstract level: LogLevel

type internal LoggerWrapper(logger: Logger) =
  abstract name: PointName
  abstract level: LogLevel
  abstract logWithAck: bool * LogLevel -> (LogLevel -> Message) -> LogResult
  default x.name = logger.name
  default x.level = logger.level
  default x.logWithAck (waitForBuffers, logLevel) messageFactory =
    logger.logWithAck (waitForBuffers, logLevel) messageFactory

  interface Logger with
    member x.name = x.name
    member x.level = x.level
    member x.logWithAck (waitForBuffers, logLevel) messageFactory =
      x.logWithAck (waitForBuffers, logLevel) messageFactory

/// A disposable interface to use with `use` constructs and to create child-
/// contexts. Since it inherits Logger, you can pass this scope down into child
/// function calls. This interface should dovetail with how Zipkin/Dapper
/// manages parent/child spans.
type LoggerScope =
  inherit IDisposable
  inherit Logger

type TimeLogger =
  inherit LoggerScope
  /// How long this Span has been in existence, or otherwise its total duration, if finished
  abstract elapsed: Duration
  abstract bisect: string -> unit
  /// Call to stop the timer/Span. This method call is non-blocking, but executes the callback on the caller's thread.
  abstract finish: (Message -> Message) -> unit

type SpanLogger =
  inherit LoggerScope
  inherit Span

  /// Makes the `SpanLogger` log through to its Logger as well as keeping the events
  /// in the Span.
  abstract logThrough: unit -> unit