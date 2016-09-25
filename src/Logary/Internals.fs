namespace Logary.Internals

open Logary
open NodaTime
open System.Threading
open System.Runtime.CompilerServices
open Hopac
open Hopac.Extensions.Seq

module internal Seq =

  let all f s = Seq.fold (fun acc t -> acc && f t) true s
  let any f s = Seq.fold (fun acc t -> acc || f t) false s
  let pjmap = Con.mapJob
  let last xs = Seq.reduce (fun _ x -> x) xs

type Timeout =
  | Infinite
  | Timeout of Duration

type TimeoutResult<'a> =
  | TimedOut
  | Success of 'a

module internal Job =

  open Hopac
  open Hopac.Infixes

  /// Returns a new job with a timeout.
  /// If the job finishes before the timeout, it will return a Success.
  /// If the job takes longer than the timeout to execute, it will return a TimedOut.
  let withTimeout timeout j =
    match timeout with
    | Infinite ->
      Job.map Success j

    | Timeout ts -> job {
      let! isDone = Promise.start j
      return!
        timeOut (ts.ToTimeSpan()) ^->. TimedOut
        <|> Promise.read isDone ^-> Success
    }

  let apply fJob xJob =
    fJob <*> xJob >>- fun (fN, x) -> fN x

module internal Alt =
  open Hopac.Infixes

  let apply (fAlt : Alt<'a -> 'b>) (xAlt : Alt<'a>) =
    let one = fAlt <+> xAlt
    one ^-> fun (fA, x) -> fA x

module internal List =
  open Hopac.Infixes

  /// Map a Job producing function over a list to get a new Job using
  /// applicative style (parallel). ('a -> Job<'b>) -> 'a list -> Job<'b list>
  let rec traverseJobA (f : 'a -> Job<'b>) (list : 'a list) : Job<'b list> =
    let cons head tail = head :: tail
    let initState = Job.result []
    let folder head tail =
      Job.apply (Job.apply (Job.result cons) (f head)) tail

    List.foldBack folder list initState

  let rec traverseAltA (f : _ -> Alt<'b>) list : Alt<'b list> =
    let cons head tail = head :: tail
    let initState = Alt.always []
    let folder head tail =
      Alt.apply (Alt.apply (Alt.always cons) (f head)) tail

    List.foldBack folder list initState

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

  // TO CONSIDER: Make IClock configurable
  let internal clock = ref SystemClock.Instance

  /// Returns the number of nanoseconds since epoch
  let timestamp () : int64 =
    (!clock).Now.Ticks * Constants.NanosPerTick

  /// Gets the current instant from the global clock
  let instant () : Instant =
    (!clock).Now

module Cache =
  open System
  open System.Collections.Concurrent

  let memoize<'input, 'output> (f : 'input -> 'output) : ('input -> 'output) =
    let cache = ConcurrentDictionary<'input, 'output>()
    fun x -> cache.GetOrAdd(x, f)

module Map =
  open System
  open System.Collections
  open System.Collections.Generic
  open System.Globalization
  open System.Reflection
  open Logary.YoLo

  // TODO: cache
  let private props =
    (fun (typ : Type) ->
      typ.GetProperties() |> Array.map (fun p -> p.Name) |> Set.ofSeq)

  // TODO: cache
  let private prop =
    fun (name : string, typ : Type) ->
      typ.GetProperty name

  /// This is basically an assembly-internal function; depend on at
  /// your own misery.
  let ofObject : obj -> _ =
    let toS o =
      match o with
      | null ->
        ""
      | _ ->
        try Convert.ToString(box o, CultureInfo.InvariantCulture)
        with | :? InvalidCastException -> o.ToString()

    let tryMap f xs =
      Seq.map f xs |> Seq.filter Option.isSome |> Seq.map Option.get

    let foldPut s (k, v) = s |> Map.put k v

    let kvLike x =
      let typ = x.GetType()
      Set.contains "Key" (props typ) && Set.contains "Value" (props typ)

    let tupleLike x =
      let typ = x.GetType()
      Set.contains "Item1" (props typ) && Set.contains "Item2" (props typ)

    let read (kp : PropertyInfo) (vp : PropertyInfo) x =
      if kp = null then raise (invalidArg "kp" "should not be null")
      if vp = null then raise (invalidArg "vp" "should not be null")
      try
        let k   = kp.GetValue(x, null)
        let v   = vp.GetValue(x, null)
        Some(toS k, v)
      with
      | :? TargetException -> // bug in F# compiler
        None

    let readInner : obj -> (string * obj) option =
      function
      | :? KeyValuePair<string, obj> as kvp ->
        Some (kvp.Key, kvp.Value)
      | :? Tuple<string, obj> as t ->
        Some (t.Item1, t.Item2)
      | x when kvLike x ->
        let typ = x.GetType()
        let kp, vp  = prop ("Key", typ), prop ("Value", typ)
        read kp vp x
      | x when tupleLike x ->
          let typ = x.GetType()
          let kp, vp = prop ("Item1", typ), prop ("Item2", typ)
          read kp vp x
      | x -> None

    function
    | null ->
      Map.empty

    | :? System.Collections.IDictionary as dict
      when dict.GetType().IsGenericType ->
      let values = seq {
         let e = dict.GetEnumerator()
         while e.MoveNext() do
           match e.Current with
           | :? DictionaryEntry as entry ->
             yield toS entry.Key, entry.Value
           | _ -> () }
      values |> Seq.fold foldPut Map.empty

    | :? System.Collections.IDictionary as dict ->
      dict
      |> Seq.cast<System.Collections.DictionaryEntry>
      |> Seq.map (fun de -> toS de.Key, de.Value)
      |> Seq.fold foldPut Map.empty

    | :? IEnumerable as data ->
      seq {
        let e = data.GetEnumerator()
        while e.MoveNext() do
          yield e.Current }
      |> tryMap readInner
      |> Seq.fold (fun s (k, v) -> s |> Map.put k v) Map.empty

    | _ as data ->
      let props =
        data.GetType().GetProperties(BindingFlags.Instance ||| BindingFlags.Public)
      // If you find yourself reading these lines of code, because you logged a
      // data object, but never found it in the output; it's because you didn't
      // log an object with properties (such as a string) - instead log an
      // anonymous object or a dictionary of some sort, which allows you to specify
      // the key names to use.
      // This function will return an empty Map if there are no properties on the
      // object.
      match props with
      | null
      | _ when props.Length = 0 ->
        Map.empty

      | _ ->
        props
        |> Array.filter (fun pi -> pi <> null && pi.Name <> null)
        |> Array.map (fun pi ->
          let mutable value = null
          try
            value <- pi.GetValue(data, null)
          with ex ->
            value <- (sprintf "Property accessor %s on %s threw an exception: %s" pi.Name pi.ReflectedType.FullName (ex.ToString()))
          pi.Name, value)
        |> Map.ofArray

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

module internal CSharp =
  open System
  open Hopac
  open System.Threading.Tasks

  [<CompiledName "ToTask">]
  let internal toTask<'a> (jj : Job<'a>) : Task<'a> =
    let tcs = new TaskCompletionSource<'a>()
    jj |> Job.map (tcs.SetResult >> ignore) |> start
    tcs.Task

  [<CompiledName "ToFSharpFunc">]
  let toFSharpFunc (f : Func<_>) : unit -> _ =
    f.Invoke

  [<CompiledName "ToFSharpFunc">]
  let toFSharpFunc1 (f : Func<_, _>) : _ -> _ =
    fun x -> f.Invoke x
