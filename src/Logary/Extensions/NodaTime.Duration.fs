namespace Logary

open NodaTime
open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module DurationEx =

  type Duration with
    [<Extension; CompiledName "ToGauge">]
    member dur.toGauge () =
      Ticks.toGauge dur.BclCompatibleTicks