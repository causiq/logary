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
  let GaugeTypePrefix = LogaryPrefix + "gauge."
  
  /// for api compatibility, when user don't provide gauge type
  [<Literal>]
  let DefaultGaugeType = "default-gauge"
  

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