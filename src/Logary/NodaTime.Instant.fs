namespace Logary

open NodaTime

module Instant =
  let ofEpoch (e : EpochNanoSeconds) =
    Instant.FromTicksSinceUnixEpoch(e / Constants.NanosPerTick)