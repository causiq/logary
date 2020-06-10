namespace Logary

/// Time calculation constants
module Constants =
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let SecondsPerTick = 0.0000001

  [<Literal>]
  let MillisPerTick = 0.0001
  [<Literal>]
  let MicrosPerTick = 0.1

  /// Nanos per BCL tick. Not the same as Stopwatch.ElapsedTicks; see
  /// https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.stopwatch.frequency?view=netcore-3.1)
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

  let BCLTicksAt1970 = System.DateTimeOffset(1970, 1, 1, 0, 0, 0, System.TimeSpan.Zero).Ticks
