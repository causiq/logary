namespace Logary

open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module DurationEx =
  open NodaTime

  type Duration with
    [<Extension; CompiledName "ToGauge">]
    member dur.toGauge () =
      Int64 (dur.Ticks * Constants.NanosPerTick),
      Scaled (Seconds, float Constants.NanosPerSecond)