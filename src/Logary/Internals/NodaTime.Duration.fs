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


    static member ofNanoseconds (s: float) = Duration.FromNanoseconds s
    static member ofNanoseconds (s: int64) = Duration.FromNanoseconds s
    static member ofNanoseconds (s: bigint) = Duration.FromNanoseconds s
    static member ofMicroSeconds (s: float) = Duration.FromNanoseconds (s * 1_000.)
    static member ofMicroSeconds (s: int64) = Duration.FromNanoseconds (s * 1_000L)
    static member ofMilliseconds (s: float) = Duration.FromMilliseconds s
    static member ofMilliseconds (s: int64) = Duration.FromMilliseconds s
    static member ofSeconds (s: float) = Duration.FromSeconds s
    static member ofSeconds (s: int64) = Duration.FromSeconds s
    static member ofMinutes (s: float) = Duration.FromMinutes s
    static member ofMinutes (s: int64) = Duration.FromMinutes s
    static member ofHours (s: float) = Duration.FromHours s
    static member ofHours (s: int) = Duration.FromHours s
    static member ofDays (s: float) = Duration.FromDays s
    static member ofDays (s: int) = Duration.FromDays s
