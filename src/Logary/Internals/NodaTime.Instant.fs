namespace Logary

open NodaTime

[<AutoOpen>]
module InstantEx =
  type NodaTime.Instant with
    member x.asTimestamp =
      x.ToUnixTimeTicks() * Constants.NanosPerTick

    static member ofEpoch (e: EpochNanoSeconds) =
      Instant.FromUnixTimeTicks(e / Constants.NanosPerTick)

