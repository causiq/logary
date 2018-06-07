namespace Logary

open System
open System.Runtime.CompilerServices

/// Extensions to facilitate converting DateTime and DateTimeOffset to EpochNanoSeconds.
[<AutoOpen; Extension>]
module SystemDateEx =

  type DateTimeOffset with
    /// Gets the EpochNanoSeconds from the DateTimeOffset.
    [<CompiledName("Timestamp"); Extension>]
    member x.timestamp: EpochNanoSeconds =
      (x.Ticks - DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks)
      * Constants.NanosPerTick

/// Helper functions for transforming DateTimeOffset to timestamps in unix epoch.
module DateTimeOffset =

  let private ticksAt1970 =
    DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks

  /// Get the DateTimeOffset ticks from EpochNanoSeconds
  let ticksUTC (epoch: EpochNanoSeconds): int64 =
    epoch / Constants.NanosPerTick
    + ticksAt1970

  /// Get the DateTimeOffset from EpochNanoSeconds
  let ofEpoch (epoch: EpochNanoSeconds): DateTimeOffset =
    DateTimeOffset(ticksUTC epoch, TimeSpan.Zero)

module DateTime =
  /// Get the DateTime from EpochNanoSeconds
  let ofEpoch (epoch: EpochNanoSeconds): DateTime =
    DateTime(DateTimeOffset.ticksUTC epoch, DateTimeKind.Utc)