namespace Logary.Internals

module internal Date =
  open System
  open NodaTime
  let utcNow () = SystemClock.Instance.Now

module internal Map =
  let put k v (m : Map<_,_>) =
    match m.TryFind k with
    | None -> m |> Map.add k v
    | Some _ -> m |> Map.remove k |> Map.add k v

[<AutoOpen>]
module internal Set =
  let (|EmptySet|_|) = function
    | (s : _ Set) when s.Count = 0 -> Some EmptySet
    | _ -> None

// TODO: consider moving NackDescription and Acks to Logary ns instead of Internals

/// A description of why no Ack was received like was expected.
type NackDescription = string

/// A discriminated union specifying Ack | Nack; a method for
/// specifying the success of an asynchronous call.
type Acks =
  /// It went well.
  | Ack
  /// It didn't go well.
  | Nack of NackDescription
