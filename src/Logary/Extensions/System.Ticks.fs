namespace Logary

open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Extension>]
module Ticks =
  /// Convert the ticks value to a Gauge.
  [<CompiledName("ToGauge"); Extension>]
  let toGauge (ticks: int64) =
    let f = float ticks * float Constants.NanosPerTick
    let u = Scaled (Seconds, float Constants.NanosPerSecond)
    Gauge (Float f, u)