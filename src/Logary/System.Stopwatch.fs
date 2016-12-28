namespace Logary

/// Extensions to facilitate reading Diagnostics.Stopwatch as a value that
/// suits Logary
[<AutoOpen; Extension>]
module StopwatchEx =
  open System.Diagnostics

  type Stopwatch with
    /// Convert the current value of the Stopwatch to a gauge's value and unit.
    [<Extension; CompiledName "ToGauge">]
    member sw.toGauge() : (Value * Units) =
      Int64 (sw.ElapsedTicks * Constants.NanosPerTick),
      Scaled (Seconds, float Constants.NanosPerSecond)

    [<Extension; CompiledName "Time">]
    static member time (fn : unit -> 'res) : 'res * (Value * Units) =
      let sw = Stopwatch.StartNew()
      let res = fn ()
      sw.Stop()
      res, sw.toGauge()