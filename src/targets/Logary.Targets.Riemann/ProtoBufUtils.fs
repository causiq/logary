module private Logary.ProtoBufUtils

open System

// source https://gist.githubusercontent.com/mausch/571158/raw/e39c0d099bdf0dd69497112e3e5c6124f4c48094/Nullable.fs
// http://bugsquash.blogspot.se/2010/09/nullable-in-f.html

module Option =
  let fromNullable (n: _ Nullable) =
    if n.HasValue then Some n.Value else None

  let toNullable = function
    | None -> Nullable()
    | Some x -> Nullable(x)

let (|Null|Value|) (x: _ Nullable) =
  if x.HasValue then Value x.Value else Null

module Nullable =
  let create x = Nullable x
  let getOrDefault n v = match n with Value x -> x | _ -> v
  let getOrElse (n: 'a Nullable) (v: 'a Lazy) = match n with Value x -> x | _ -> v.Force()
  let get (x: _ Nullable) = x.Value
  let fromOption = Option.toNullable
  let toOption = Option.fromNullable
  let fromCLRNull<'a when 'a : null and 'a : (new: unit -> 'a)> = function
    | null as v -> v
    | (v : 'a)  -> v
  let bind f = function
    | Null    -> Nullable()
    | Value v -> f v
  let hasValue (x: _ Nullable) = x.HasValue
  let isNull (x: _ Nullable) = not x.HasValue
  let count (x: _ Nullable) = if x.HasValue then 1 else 0

  /// Apply f to the state and the value in the nullable, or otherwise return
  /// the state.
  /// match x with Null -> state | Value v -> f state v
  let fold f state = function
    | Null -> state
    | Value v -> f state v

  /// Apply f to x and state if there's a value in the nullable, throwing
  /// away the nullable.
  /// match x with Null -> state | Value v -> f x state
  let foldBack f x state =
    match x with
    | Null    -> state
    | Value v -> f x state

  let exists p x =
    match x with
    | Null    -> false
    | Value v -> p x

  let forall p x =
    match x with
    | Null    -> true
    | Value v -> p x

  let iter f x =
    match x with
    | Null    -> ()
    | Value v -> f v

  let map f x =
    match x with
    | Null -> Nullable ()
    | Value v -> Nullable (f v)

  let toArray x = 
    match x with
    | Null -> [||]
    | Value v -> [| v |]

  let toList x =
    match x with
    | Null -> []
    | Value v -> [v]

let liftNullable op (a: _ Nullable) (b: _ Nullable) =
  if a.HasValue && b.HasValue
  then Nullable (op a.Value b.Value)
  else Nullable ()

let mapBoolOp op a b =
  match a, b with
  | Value x, Value y -> op x y
  | _ -> false

let structEq a b =
  match a, b with
  | Value x, Value y -> x = y
  | Null, Null       -> true // in this case, null=null
  | _                -> false // if one is, other isn't, not eq

let inline (+?) a b = (liftNullable (+)) a b
let inline (-?) a b = (liftNullable (-)) a b
let inline ( *?) a b = (liftNullable ( *)) a b
let inline (/?) a b = (liftNullable (/)) a b
let inline (>?) a b = (mapBoolOp (>)) a b
let inline (=?) a b = structEq a b
let inline (>=?) a b = a >? b || a = b
let inline (<?) a b = (mapBoolOp (<)) a b
let inline (<=?) a b = a <? b || a = b

let inline notn (a: bool Nullable) =
  if a.HasValue then Nullable (not a.Value) else Nullable ()

let inline (&?) a b =
  let rec and' a b =
    match a,b with
    | Null, Value y when not y -> Nullable false
    | Null, Value y when y -> Nullable ()
    | Null, Null -> Nullable ()
    | Value x, Value y -> Nullable (x && y)
    | _ -> and' b a
  and' a b

let inline (|?) a b = notn ((notn a) &? (notn b))

type Int32 with
  member x.n = Nullable x

type Double with
  member x.n = Nullable x

type Single with
  member x.n = Nullable x

type Byte with
  member x.n = Nullable x

type Int64 with
  member x.n = Nullable x

type Boolean with
  member x.n = Nullable x

let compareCore a b = compare a b

/// compare a and b
let compare a b =
  match a with
  | Null    -> -1
  | Value v -> b |> Nullable.fold (fun s t -> compare v t) 1

/// continue the comparison on a and b if prev eq 0 otherwise prev
let compareElse a b prev = if prev = 0 then compareCore a b else prev
