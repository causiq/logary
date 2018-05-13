namespace Logary

/// Literals Logary uses for placing values in the Message structure.
module KnownLiterals =

  /// Avoid conflict with user defined context key
  [<Literal>]
  let internal LogaryPrefix = "_logary."

  /// To recognize all fields for generate formatted msg
  [<Literal>]
  let FieldsPrefix = "_fields."

  /// To recognize all gauge fields for generate formatted msg
  [<Literal>]
  let GaugeNamePrefix = LogaryPrefix + "gauge."

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