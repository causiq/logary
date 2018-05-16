namespace Logary

open NodaTime
open System
open System.Diagnostics
open System.Runtime.CompilerServices

/// Extensions to facilitate reading Diagnostics.Stopwatch as a value that
/// suits Logary
[<AutoOpen; Extension>]
module StopwatchEx =

  type Stopwatch with
    /// Convert the current value of the Stopwatch to a gauge's value and unit.
    [<Extension; CompiledName "ToGauge">]
    member sw.toGauge() =
      Gauge.ofStopwatchTicks sw.ElapsedTicks

    [<Extension; CompiledName "Time">]
    static member time (fn: unit -> 'res) =
      let sw = Stopwatch.StartNew()
      let res = fn ()
      sw.Stop()
      res, sw.toGauge()

/// http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx
type StopwatchTicks = int64

/// http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx
module StopwatchTicks =
  let inline getTimestamp (): StopwatchTicks =
    Stopwatch.GetTimestamp()

  let ticksPerNanosecond =
    // [ tick / s ] / [ ns / s ] => [ tick / ns ]
    float Stopwatch.Frequency / 1_000_000_000.

  let toNanoseconds (ticks: StopwatchTicks) =
    // e.g. 1 tick / 0.01 = 100 ns
    int64 (float ticks / ticksPerNanosecond)

  let toTimeSpan (ticks: StopwatchTicks) =
    TimeSpan.FromTicks ((ticks * TimeSpan.TicksPerSecond) / Stopwatch.Frequency)

  let toDuration (ticks: StopwatchTicks) =
    Duration.FromNanoseconds (toNanoseconds ticks)