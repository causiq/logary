namespace Logary

[<AutoOpen>]
module internal GlobalFunctions =
  let inline nullIsNone x =
    match x with
    | null ->
      None
    | x ->
      Some x

module internal Promise =
  open Hopac
  let unit: Promise<unit> = Promise (())

module internal Result =
  let inline isOk r =
    match r with Result.Ok _ -> true | Result.Error _ -> false

  let inline isError r =
    match r with Result.Ok _ -> false | Result.Error _ -> true

  let private folder t s =
    match s, t with
    | Ok _, Result.Error err ->
      Result.Error [ err ]
    | Ok oks, Ok ok ->
      Ok (ok :: oks)
    | Result.Error errors, Result.Error err ->
      Result.Error (err :: errors)
    | Result.Error errors, Ok _ ->
      Result.Error errors

  let sequence rs =
    (rs, Ok [])
    ||> Array.foldBack folder

module internal Seq =
  /// Get the head and the tail of the sequence, throwing if the sequence contains no elements.
  let headTail (xs: #seq<_>): 't * seq<'t> =
    let e = xs.GetEnumerator()
    if not (e.MoveNext()) then
      failwithf "Could not get first element of sequence"
    e.Current, seq {
      try
        while e.MoveNext() do
          yield e.Current
      finally
        e.Dispose()
    }

module internal List =
  open Hopac
  /// Map a Job producing function over a list to get a new Job using
  /// applicative style (parallel). ('a -> Job<'b>) -> 'a list -> Job<'b list>
  let rec traverseJobA (f: 'a -> #Job<'b>) (list: #seq<'a>): Job<'b list> =
    let cons head tail = head :: tail
    let initState = Job.result []
    let folder head tail =
      Job.apply tail (Job.apply (f head) (Job.result cons) )
    Seq.foldBack folder list initState

[<AutoOpen>]
module internal Comparison =
  let thenCompare (a: 'a) (b: 'a) = function
    | 0 -> compare a b
    | x -> x
