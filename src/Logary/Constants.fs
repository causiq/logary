namespace Logary

/// Literals Logary uses for placing values in the Message structure.
module KnownLiterals =

  /// Avoid conflict with user defined context key
  [<Literal>]
  let LogaryPrefix = "_logary."

  /// To recognise the regular fields when generating the templated message
  [<Literal>]
  let FieldsPrefix = "_fields."

  /// To recognise the gauge fields when generating the templated message
  [<Literal>]
  let GaugeNamePrefix = LogaryPrefix + "gauge."

  /// All gauges should have this tag. It's added whenever a gauge is added to the message.
  [<Literal>]
  let GaugeTag = "gauge"

  /// for api compatibility, when user don't provide gauge type
  [<Literal>]
  let DefaultGaugeName = "default-gauge"

  [<Literal>]
  let ErrorsContextName = LogaryPrefix + "errors"

  [<Literal>]
  let ServiceContextName =  LogaryPrefix + "service"

  [<Literal>]
  let HostContextName =  LogaryPrefix + "host"

  /// The tags context field
  [<Literal>]
  let TagsContextName =  LogaryPrefix +  "tags"

  [<Literal>]
  let SinkTargetsContextName =  LogaryPrefix +  "sink.targets"

  [<Literal>]
  let WaitForBuffers = LogaryPrefix + "waitForBuffers"

  [<Literal>]
  let WaitForBuffersTimeout = LogaryPrefix + "waitForBuffersTimeout"

  [<Literal>]
  let SpanIdContextName = LogaryPrefix + "spanId"

  [<Literal>]
  let SpanDataContextName = LogaryPrefix + "spanData"

  [<Literal>]
  let CounterMetricConfContextName = LogaryPrefix + "counter.conf"

/// Time calculation constants
module Constants =
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let SecondsPerTick = 0.0000001

  [<Literal>]
  let MillisPerTick = 0.0001
  [<Literal>]
  let MicrosPerTick = 0.1

  [<Literal>]
  let NanosPerTick = 100L
  [<Literal>]
  let NanosPerMicro = 1_000L
  [<Literal>]
  let NanosPerMilli = 1_000_000L
  [<Literal>]
  let NanosPerSecond = 1_000_000_000L
  [<Literal>]
  let NanosPerMinute = 60_000_000_000L

  [<Literal>]
  let MicrosPerSecond = 1_000_000L

  [<Literal>]
  let MillisPerSecond = 1_000L

  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerMinute = 600_000_000L
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerSecond = 10_000_000L
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerMilli = 10000L
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerMicro = 10L