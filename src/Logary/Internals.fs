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

module Date =
  open NodaTime
  let now () = SystemClock.Instance.Now

module Map =
  let put k v (m : Map<_,_>) =
    match m.TryFind k with
    | None -> m |> Map.add k v
    | Some _ -> m |> Map.remove k |> Map.add k v
    
  open System
  open System.Collections.Generic

  /// This is basically an assembly-internal function; depend on at
  /// your own misery.
  let fromObj : obj -> _ = function
    | null -> Map.empty
    | :? IEnumerable<KeyValuePair<string, obj>> as data ->
      try
        data
        |> Seq.map (fun kv -> (kv.Key, kv.Value))
        |> Map.ofSeq
      with
      | :? InvalidCastException -> Map.empty
    | :? System.Collections.IDictionary as dict ->
      try
        dict
        |> Seq.cast<System.Collections.DictionaryEntry>
        |> Seq.filter (fun kv -> match kv.Key with :? string -> true | _ -> false)
        |> Seq.map (fun kv -> (kv.Key :?> string, kv.Value))
        |> Map.ofSeq
      with
      | :? InvalidCastException -> Map.empty
    | _ as data ->
      let props = data.GetType() |> fun t -> t.GetProperties()
      // If you find yourself reading these lines of code, because you logged a
      // data object, but never found it in the output; it's because you didn't
      // log an object with properties (such as a string) - instead log an
      // anonymous object or a dictionary of some sort, which allows you to specify
      // the key names to use.
      // This function will return an empty Map if there are no properties on the
      // object.
      match props with
      | null | _ when props.Length = 0 -> Map.empty
      | _    ->
      try
        props
        |> Array.filter (fun pi -> pi <> null && pi.Name <> null)
        |> Array.map (fun pi -> (pi.Name, pi.GetValue(data, null)))
        |> Map.ofArray
      with
      | :? InvalidCastException -> Map.empty
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
