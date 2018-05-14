namespace Logary

open NodaTime
open System
open System.Runtime.CompilerServices
open Logary

[<AutoOpen; Extension>]
module DurationEx =

  type Duration with
    [<Extension; CompiledName "ToGauge">]
    member dur.toGauge () =
      Gauge (BigInt (dur.ToBigIntegerNanoseconds()), Scaled (Seconds, float Constants.NanosPerSecond))