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

module internal LogResult =
  open Hopac
  let success: Alt<Result<Promise<unit>, LogError>> = Alt.always (Result.Ok Promise.unit)
  let bufferFull target: Alt<Result<Promise<unit>, LogError>> = Alt.always (Result.Error (BufferFull target))
  let rejected: Alt<Result<Promise<unit>, LogError>> = Alt.always (Result.Error Rejected)

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
