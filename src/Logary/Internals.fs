namespace Logary.Internals

module internal Ns =
  open FSharp.Actor

  /// The actor's root namespace
  [<Literal>]
  let ActorRootNs = "logary"

  /// Create a namespace from the subcomponent identifier
  /// A single place to create Actor options -- until FSharp.Actor changes again
  let create subcomponent =
    sprintf "%s/%s" ActorRootNs subcomponent
    |> fun str -> Actor.Options.Create str

module internal Seq =
  let all f s = Seq.fold (fun acc t -> acc && f t) true s
  let any f s = Seq.fold (fun acc t -> acc || f t) false s
  let pmap (f : _ -> Async<_>) s = s |> Seq.map f |> Async.Parallel

[<AutoOpen>]
module internal UtilFns =
  let flip f a b = f b a

[<AutoOpen>]
module internal Comparison =
  let thenCompare (a : 'a) (b : 'a) = function
    | 0 -> compare a b
    | x -> x

module internal Rnd =
  open System

  /// buffer for random values
  let private buf = Array.zeroCreate sizeof<int64>
  let private BitsPerLong = 63

  let random = new Threading.ThreadLocal<_>(fun () -> Random())

  let nextInt () =
    random.Value.Next (Int32.MinValue, Int32.MaxValue)

  /// get the next int within [0, max]
  let nextInt' max =
    random.Value.Next max

  let nextInt64 () =
    random.Value.NextBytes buf
    BitConverter.ToInt64 (buf, 0)

  /// get the next int64 within [0, max]
  let nextInt64' (max : int64) =
    let mutable bits = 0L
    let mutable value = 0L
    let mutable first = true
    while first || bits - value + (max - 1L) < 0L do
      bits <- nextInt64 () &&& (~~~(1L <<< BitsPerLong))
      value <- bits % max
      first <- false
    value

module internal Date =
  open NodaTime
  let now () = SystemClock.Instance.Now

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
