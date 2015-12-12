module Logary.Tests.TestDSL

open Logary
open System
open Hopac

type AssertResult =
  | Success
  | Failure of string

and AssertContext<'a> =
  { message : string
    item    : 'a
    negated : bool }

and Asserter<'a> = 'a -> AssertContext<'a> -> AssertResult

let because msg fBecause =
  { message = msg
    item    = fBecause()
    negated = false }

let theSubject item =
  { message = ""
    item    = item
    negated = false }

let executing msg (f : unit -> _) =
  { message = msg
    item    = f
    negated = false }

let private nl = Environment.NewLine

open Logary

/// Alias for PointName.ofSingle
let pn  = PointName.ofSingle

/// Wrapper for PointName.parse
let pnp = PointName.parse

let theTuple f (tupleCtx : AssertContext<'a * 'b>) =
  f ({ item    = tupleCtx.item |> fst
       message = tupleCtx.message
       negated = tupleCtx.negated })
    ({ item    = tupleCtx.item |> snd
       message = tupleCtx.message
       negated = tupleCtx.negated })

let should (asserter : Asserter<'a>) (expected : 'a) (ctx : AssertContext<_>) =
  match asserter expected ctx with
  | Success -> ctx
  | Failure msg ->
    failwith <| sprintf "because %s:%s%s%s" ctx.message nl nl msg

let should' asserter (ctx : AssertContext<_>) =
  match asserter ctx with
  | Success -> ctx
  | Failure msg ->
    failwith <| sprintf "because %s:%s%s%s" ctx.message nl nl msg

let should_not (asserter : Asserter<'a>) (expected : 'a) (ctx : AssertContext<_>) =
  match asserter expected { ctx with negated = true } with
  | Success -> ctx
  | Failure msg ->
    failwith <| sprintf "because %s:%s%s%s" ctx.message nl nl msg

// asserters

let be expected ctx =
  match ctx.item with
  | _ as v when
    (ctx.negated || v = expected)
    || (not ctx.negated || (not <| v = expected)) -> Success
  | _ as v ->
    Failure
      (sprintf "expected\n%A to%s have the same value as %A"
        v (if ctx.negated then " NOT" else "") expected)

let contain substr ctx =
  if (ctx.negated || (ctx.item : string).Contains(substr))
    && (not ctx.negated || (not <| (ctx.item : string).Contains(substr))) then
    Success
  else
    Failure (sprintf "expected the %s '%s'%s to%s contain substring '%s'"
      (ctx.item.GetType().Name)
      ctx.item
      nl
      (if ctx.negated then " NOT" else "")
      substr)

let equal other ctx =
  if other = ctx.item && not ctx.negated then
    Success
  else
    Failure (sprintf "expected\n%A\nto%s equal\n%A"
      ctx.item
      (if ctx.negated then " NOT" else "")
      other)

let fulfil f ctx =
  let msg, success = f ctx.item
  if success && not ctx.negated then
    Success
  else
    Failure(sprintf "failed predicate: %s" msg)

let raiseExn<'TExn when 'TExn :> exn> ctx =
  if not ctx.negated then
    try
      ctx.item ()
      Failure (sprintf "expected %s to raise\n%A\n, but it never raised an exception"
        ctx.message
        typeof<'TExn>)
    with
    | :? 'TExn -> Success
    | e -> Failure (sprintf "expected %s to raise\n%A\n, but it raised\n%A\ninstead"
             ctx.message
             typeof<'TExn>
             (e.GetType()))
  else // = should not raise TExn exception
    try
      ctx.item ()
      Success
    with
    | :? 'TExn -> Failure (sprintf "didn't expect %s to raise\n%A\n, but it did."
                    ctx.message
                    typeof<'TExn>)
    | _ -> Success // it raised another exception, that's a success

let thatsIt = ignore