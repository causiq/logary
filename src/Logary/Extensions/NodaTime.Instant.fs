namespace Logary

open NodaTime

module Instant =

  let ofEpoch (e: EpochNanoSeconds) =
    NodaTime.Instant.FromUnixTimeTicks(e / Constants.NanosPerTick)