namespace Logary

open NodaTime
open System

[<AutoOpen>]
module DurationEx =
  let timeSpanMaxDuration = Duration.FromTimeSpan(TimeSpan.MaxValue)
  let timeSpanMinDuration = Duration.FromTimeSpan(TimeSpan.MinValue)

  type Duration with
    [<CompiledName "ToGauge">]
    member dur.toGauge () =
      Gauge (Value.BigInt (dur.ToBigIntegerNanoseconds()), U.Scaled (U.Seconds, float Constants.NanosPerSecond))

    [<CompiledName "ToTimeSpanSafe">]
    member dur.toTimeSpanSafe() =
      if dur < timeSpanMinDuration then TimeSpan.MinValue
      elif dur > timeSpanMaxDuration then TimeSpan.MaxValue
      else dur.ToTimeSpan()
