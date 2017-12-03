namespace Logary.Internals

open Logary
open NodaTime
open System
open System.Threading
open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes

module internal Seq =
  let any f s = Seq.fold (fun acc t -> acc || f t) false s
  let last xs = Seq.reduce (fun _ x -> x) xs

module internal Promise =
  let instaPromise =
    Alt.always (Promise (())) // new promise with unit value

module internal Choice =
  let bimap f1 f2 = function
    | Choice1Of2 x1 -> Choice1Of2 (f1 x1)
    | Choice2Of2 x2 -> Choice2Of2 (f2 x2)

  let get = function
    | Choice1Of2 x1 -> x1
    | Choice2Of2 x2 -> x2

module internal Alt =
  open Hopac.Infixes

  let apply (fAlt : Alt<'a -> 'b>) (xAlt : Alt<'a>) =
    let one = fAlt <+> xAlt
    one ^-> fun (fA, x) -> fA x

module internal List =

  module Job =
    let apply fJob xJob = fJob <*> xJob >>- fun (fN, x) -> fN x

  /// Map a Job producing function over a list to get a new Job using
  /// applicative style (parallel). ('a -> Job<'b>) -> 'a list -> Job<'b list>
  let rec traverseJobA (f : 'a -> #Job<'b>) (list : 'a list) : Job<'b list> =
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

[<RequireQualifiedAccess>]
module HashMap =
  open Logary.Internals.Aether

  /// Prism to a value associated with a key in a map.
  let key_ (k: 'k) : Prism<HashMap<'k,'v>,'v> =
    HashMap.tryFind k,
    (fun v x ->
      if HashMap.containsKey k x then x else HashMap.set k v x )

  /// Lens to a value option associated with a key in a map.
  let value_ (k: 'k) : Lens<HashMap<'k,'v>, 'v option> =
    HashMap.tryFind k,
    (fun v x ->
      match v with
      | Some v -> HashMap.set k v x
      | _ -> HashMap.unset k x)

module Cache =
  open System.Collections.Concurrent

  let memoize<'input, 'output> (f : 'input -> 'output) : ('input -> 'output) =
    let cache = ConcurrentDictionary<'input, 'output>()
    fun x -> cache.GetOrAdd(x, f)

module Map =
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

    let foldPut s (k, v) = s |> HashMap.add k v

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
      // | :? Tuple<string, obj> as t -> // in vs 15, https://github.com/Microsoft/visualfsharp/pull/3729
      //   Some (t.Item1, t.Item2)
      | :? (string * obj) as t ->
        Some t
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
      HashMap.empty

    | :? System.Collections.IDictionary as dict
      when dict.GetType().IsGenericType ->
      let values = seq {
         let e = dict.GetEnumerator()
         while e.MoveNext() do
           match e.Current with
           | :? DictionaryEntry as entry ->
             yield toS entry.Key, entry.Value
           | _ -> () }
      values |> Seq.fold foldPut HashMap.empty

    | :? System.Collections.IDictionary as dict ->
      dict
      |> Seq.cast<System.Collections.DictionaryEntry>
      |> Seq.map (fun de -> toS de.Key, de.Value)
      |> Seq.fold foldPut HashMap.empty

    | :? IEnumerable as data ->
      seq {
        let e = data.GetEnumerator()
        while e.MoveNext() do
          yield e.Current }
      |> tryMap readInner
      |> Seq.fold (fun s (k, v) -> s |> HashMap.add k v) HashMap.empty

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
        HashMap.empty

      | _ ->
        props
        |> Array.filter (fun pi -> pi <> null && pi.Name <> null)
        |> Array.map (fun pi ->
          let value =
            try pi.GetValue(data, null)
            with ex ->
              box (sprintf "Property accessor %s on %s threw an exception: %O" pi.Name pi.ReflectedType.FullName ex)
          pi.Name, value)
        |> HashMap.ofArray