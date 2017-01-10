namespace Logary

open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Extension>]
module Ticks =
  /// Convert the ticks value to a Gauge.
  [<CompiledName("ToGauge"); Extension>]
  let toGauge (ticks : int64) =
    Int64 (ticks * Constants.NanosPerTick),
    Scaled (Seconds, float Constants.NanosPerSecond)