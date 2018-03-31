namespace Logary.Internals

open Logary
open NodaTime
open System
open System.Threading
open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes

module internal Promise =
  let instaPromise =
    Alt.always (Promise (())) // new promise with unit value

module internal Alt =
  open Hopac.Infixes

  let apply (fAlt: Alt<'a -> 'b>) (xAlt: Alt<'a>) =
    let one = fAlt <+> xAlt
    one ^-> fun (fA, x) -> fA x

module internal List =

  module Job =
    let apply fJob xJob = fJob <*> xJob >>- fun (fN, x) -> fN x

  /// Map a Job producing function over a list to get a new Job using
  /// applicative style (parallel). ('a -> Job<'b>) -> 'a list -> Job<'b list>
  let rec traverseJobA (f: 'a -> #Job<'b>) (list: 'a list): Job<'b list> =
    let cons head tail = head :: tail
    let initState = Job.result []
    let folder head tail =
      Job.apply (Job.apply (Job.result cons) (f head)) tail

    List.foldBack folder list initState

  let rec traverseAltA (f: _ -> Alt<'b>) list: Alt<'b list> =
    let cons head tail = head :: tail
    let initState = Alt.always []
    let folder head tail =
      Alt.apply (Alt.apply (Alt.always cons) (f head)) tail

    List.foldBack folder list initState

[<AutoOpen>]
module internal Comparison =
  let thenCompare (a: 'a) (b: 'a) = function
    | 0 -> compare a b
    | x -> x

module internal Rnd =
  let private BitsPerLong = 63

  let random = new Threading.ThreadLocal<_>(fun () -> Random())

  let nextInt () =
    random.Value.Next (Int32.MinValue, Int32.MaxValue)

  /// Get the next int within [0, max]
  let nextIntMax max =
    random.Value.Next max

  let nextInt64 () =
    let buf = Array.zeroCreate sizeof<int64>
    random.Value.NextBytes buf
    BitConverter.ToInt64 (buf, 0)

  /// Get the next int64 within [0, max]
  let nextInt64Max (max: int64) =
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
  let key_ (k: 'k): Prism<HashMap<'k,'v>,'v> =
    HashMap.tryFind k,
    (fun v x ->
      if HashMap.containsKey k x then x else HashMap.set k v x )

  /// Lens to a value option associated with a key in a map.
  let value_ (k: 'k): Lens<HashMap<'k,'v>, 'v option> =
    HashMap.tryFind k,
    (fun v x ->
      match v with
      | Some v -> HashMap.set k v x
      | _ -> HashMap.unset k x)

module Cache =
  open System.Collections.Concurrent

  let memoize<'input, 'output> (f: 'input -> 'output) : ('input -> 'output) =
    let cache = ConcurrentDictionary<'input, 'output>()
    fun x -> cache.GetOrAdd(x, f)

module Reflection =
  open Logary.Internals.TypeShape.Core
  open Logary.Internals.TypeShape.Core.Utils

  /// Read the props from a plain old CLR object.
  let propsFrom (value: obj): seq<string * obj> =
    match TypeShape.FromValue value with
    | Shape.Poco shape when shape.Properties.Length > 0 ->
      seq {
        for prop in shape.Properties do
          match prop with
          | :? Reflection.PropertyInfo as pi ->
            yield prop.Label, pi.GetValue value
          | _ ->
            ()
      }
    | _ ->
      Seq.empty