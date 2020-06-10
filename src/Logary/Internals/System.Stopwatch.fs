namespace Logary

open NodaTime
open System
open System.Diagnostics
open System.Runtime.CompilerServices

/// http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx
type StopwatchTicks = int64

/// Extensions to facilitate reading Diagnostics.Stopwatch as a value that
/// suits Logary
[<AutoOpen; Extension>]
module StopwatchEx =

  type Stopwatch with
    /// Convert the current value of the Stopwatch to a gauge's value and unit.
    [<CompiledName "ToGauge">]
    member sw.toGauge() =
      Gauge.ofStopwatchTicks sw.ElapsedTicks

    [<Extension; CompiledName "Time">]
    static member time (fn: unit -> 'res) =
      let sw = Stopwatch.StartNew()
      let res = fn ()
      sw.Stop()
      res, sw.toGauge()

    static member getTimestamp (): StopwatchTicks =
      Stopwatch.GetTimestamp()

    static member ticksPerNanosecond =
      // [ tick / s ] / [ ns / s ] => [ tick / ns ]
      float Stopwatch.Frequency / 1_000_000_000.

    static member toNanoseconds (ticks: StopwatchTicks) =
      // e.g. 256 ticks * 100L (100 nanoseconds per stopwatch tick)
      (1_000_000_000L / Stopwatch.Frequency) * ticks

    static member toTimeSpan (ticks: StopwatchTicks) =
      TimeSpan.FromTicks ((ticks * TimeSpan.TicksPerSecond) / Stopwatch.Frequency)

    static member toDuration (ticks: StopwatchTicks) =
      Duration.FromNanoseconds (Stopwatch.toNanoseconds ticks)