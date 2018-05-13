namespace Logary

open NodaTime
open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module DurationEx =

  type Duration with
    [<Extension; CompiledName "ToGauge">]
    member dur.toGauge () =
      Gauge (Int64 (dur.ToInt64Nanoseconds()), Scaled (Seconds, float Constants.NanosPerSecond))
