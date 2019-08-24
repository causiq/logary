namespace Logary.Targets

open System
open Logary
open NodaTime
open System.Collections.Generic

type DeferBuffer<'k, 'v when 'k : equality>(getTimestamp: unit -> EpochNanoSeconds, keepFor: Duration) =
  // We will use a queue, because it's O(1) on dequeue/peek and since the duration is a constant for the
  // life time of this validator, we know the head is due closest in time.
  let timeouts = Queue<_>()
  // The cache stores the Spans and when they were added
  let cache = Dictionary<'k, 'v>()
  let keepFor = keepFor.ToInt64Nanoseconds()

  member x.count = cache.Count

  member x.tryModify (key: 'k, onFound: 'v -> 'v): bool =
    match cache.TryGetValue key with
    | false, _ ->
      false
    | true, value ->
      cache.[key] <- onFound value
      true

  member x.dequeueAll () =
    let res = cache |> List.ofSeq
    cache.Clear()
    timeouts.Clear()
    res

  member x.dequeueDue () =
    let now = getTimestamp ()

    /// Iterate until the head is due later than now.
    let rec iter acc =
      if timeouts.Count = 0 then List.rev acc else
      let spanId, due = timeouts.Peek()

      if due > now then List.rev acc else
      ignore (timeouts.Dequeue())
      let value = cache.[spanId]
      cache.Remove spanId |> ignore

      iter (new KeyValuePair<_,_>(spanId, value) :: acc)

    iter []

  member x.defer(key: 'k, value: 'v) =
    let now = getTimestamp ()
    match cache.TryGetValue key with
    | true, _ ->
      failwithf "Did not expect SpanId to repeat itself in the DeferBuffer! %O" key
    | false, _ ->
      cache.[key] <- value
      timeouts.Enqueue (key, now + keepFor)

  interface IDisposable with
    member x.Dispose() =
      timeouts.Clear()
      cache.Clear()