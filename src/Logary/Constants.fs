namespace Logary

/// Literals Logary uses for placing values in the Message structure.
module KnownLiterals =

  [<Literal>]
  let ErrorsFieldName = "errors"

  [<Literal>]
  let ServiceContextName = "service"

  [<Literal>]
  let HostContextName = "host"

  /// The tags context field
  [<Literal>]
  let TagsContextName = "tags"

  /// Used to tag gauges which are 'composite' at-the-same-instant measurements
  /// of something. This makes targets aware that they should not send the main
  /// PointValue
  [<Literal>]
  let SuppressPointValue = "suppress-point-value"

/// Time calculation constants
module Constants =

  [<Literal>]
  let SecondsPerTick = 0.0000001
  [<Literal>]
  let MillisPerTick = 0.0001
  [<Literal>]
  let MicrosPerTick = 0.1
  [<Literal>]
  let NanosPerTick = 100L
  [<Literal>]
  let NanosPerMicro = 1000L
  [<Literal>]
  let NanosPerMilli = 1000000L
  [<Literal>]
  let NanosPerSecond = 1000000000L
  [<Literal>]
  let NanosPerMinute = 60000000000L

  [<Literal>]
  let TicksPerMinute = 600000000L
  [<Literal>]
  let TicksPerSecond = 10000000L
  [<Literal>]
  let TicksPerMilli = 10000L
  [<Literal>]
  let TicksPerMicro = 10L