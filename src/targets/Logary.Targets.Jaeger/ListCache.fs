namespace Logary.Targets

open System
open Logary
open NodaTime
open System.Collections.Generic

/// A memory cache when all key-value pairs are to be cached the same time.
type ListCache<'k, 'v when 'k : equality>(getTimestamp: unit -> EpochNanoSeconds, keepFor: Duration) =
  // We will use a queue, because it's O(1) on dequeue/peek and since the duration is a constant for the
  // life time of this validator, we know the head is due closest in time.
  let timeouts = Queue<_>()
  // The cache stores the values and when they were added
  let cache = Dictionary<'k, 'v list>()
  let keepFor = keepFor.ToInt64Nanoseconds()

  member x.gc() =
    let now = getTimestamp ()
    let gced = ResizeArray<_>()

    /// Iterate until the head is due later than now.
    let rec iter () =
      if timeouts.Count = 0 then gced else
      let key, due = timeouts.Peek()

      if due > now then gced else
      gced.Add (timeouts.Dequeue() |> fst)
      cache.Remove key |> ignore

      iter ()

    iter ()

  member x.tryGetAndRemove key =
    match cache.TryGetValue key with
    | false, _ ->
      []
    | true, values ->
      cache.Remove key |> ignore
      List.rev values

  member x.enqueue (key: 'k, value: 'v) =
    let now = getTimestamp ()
    timeouts.Enqueue (key, now + keepFor)

    let values =
      match cache.TryGetValue key with
      | true, values -> values
      | false, _ -> []

    cache.[key] <- value :: values

  interface IDisposable with
    member x.Dispose() =
      timeouts.Clear()
      cache.Clear()
