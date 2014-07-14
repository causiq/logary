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

/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  { /// Name of the service. Will show up as 'service' in e.g. LogStash/Kibana and
    /// is the basis for a lot of the sorting and health checking that Riemann
    /// does.
    serviceName : string }


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
