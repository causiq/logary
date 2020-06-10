namespace Logary

open System

/// Extensions to facilitate converting DateTime and DateTimeOffset to EpochNanoSeconds.
[<AutoOpen>]
module SystemDateEx =
  type DateTimeOffset with
    /// Gets the EpochNanoSeconds from the DateTimeOffset.
    [<CompiledName("Timestamp")>]
    member x.asTimestamp: EpochNanoSeconds =
      (x.Ticks - Constants.BCLTicksAt1970) * Constants.NanosPerTick

    /// Get the DateTimeOffset ticks from EpochNanoSeconds
    static member ticksUTC (epoch: EpochNanoSeconds): int64 =
      epoch / Constants.NanosPerTick + Constants.BCLTicksAt1970

    /// Get the DateTimeOffset from EpochNanoSeconds
    static member ofEpoch (epoch: EpochNanoSeconds): DateTimeOffset =
      DateTimeOffset(DateTimeOffset.ticksUTC epoch, TimeSpan.Zero)

  type DateTime with
    /// Get the DateTime from EpochNanoSeconds
    static member ofEpoch (epoch: EpochNanoSeconds): DateTime =
      DateTime(DateTimeOffset.ticksUTC epoch, DateTimeKind.Utc)

