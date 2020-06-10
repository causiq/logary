namespace Logary

open NodaTime

[<AutoOpen>]
module OffsetDateTimeEx =
  type OffsetDateTime with
    member x.asTimestamp = x.ToInstant().asTimestamp
