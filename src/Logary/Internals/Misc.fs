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
  let instaPromise =
    Alt.always (Promise (())) // new promise with unit value

module internal List =
  open Hopac
  /// Map a Job producing function over a list to get a new Job using
  /// applicative style (parallel). ('a -> Job<'b>) -> 'a list -> Job<'b list>
  let rec traverseJobA (f: 'a -> #Job<'b>) (list: 'a list): Job<'b list> =
    let cons head tail = head :: tail
    let initState = Job.result []
    let folder head tail =
      Job.apply tail (Job.apply (f head) (Job.result cons) )
    List.foldBack folder list initState

[<AutoOpen>]
module internal Comparison =
  let thenCompare (a: 'a) (b: 'a) = function
    | 0 -> compare a b
    | x -> x
