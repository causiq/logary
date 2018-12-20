namespace Logary

open NodaTime
open System
open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module DurationEx =
  let timeSpanMaxDuration = Duration.FromTimeSpan(TimeSpan.MaxValue)
  let timeSpanMinDuration = Duration.FromTimeSpan(TimeSpan.MinValue)

  type Duration with
    [<Extension; CompiledName "ToGauge">]
    member dur.toGauge () =
      Gauge (BigInt (dur.ToBigIntegerNanoseconds()), Scaled (Seconds, float Constants.NanosPerSecond))

    [<Extension; CompiledName "ToTimeSpanSafe">]
    member dur.toTimeSpanSafe() =
      if dur < timeSpanMinDuration then TimeSpan.MinValue
      elif dur > timeSpanMaxDuration then TimeSpan.MaxValue
      else dur.ToTimeSpan()
