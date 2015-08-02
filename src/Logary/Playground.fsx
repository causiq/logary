#!/usr/bin/env fsharpi
#I "bin/Debug"
#r "FSharp.Actor.dll"
#r "Logary.dll"
#r "NodaTime.dll"
#I "../../packages/Hopac/lib/net45"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "FsMachines.dll"
open System
open Hopac
open Hopac.Alt.Infixes
open Hopac.TopLevel

// see https://github.com/arnolddevos/FlowLib/blob/master/src/main/scala/flowlib/Monitored.scala

type Context<'T> = Job<'T>
let inContext x = Job.result x
let mapContext f m = Job.map (f >> box) m

type Reducer<'A, 'R, 'State> =
  abstract zero      : 'State
  abstract apply     : 'State -> 'A -> Context<'State>
  abstract isReduced : 'State -> bool
  abstract complete  : 'State -> 'R

type ReducerT<'A, 'R> = Reducer<'A, 'R, obj>

let reducer (zero : 'State) apply isReduced complete =
  { new Reducer<_, _, obj> with
      member x.zero = box zero
      member x.apply state a = apply (state :?> 'State) a
      member x.isReduced state = isReduced (state :?> 'State)
      member x.complete state = complete (state :?> 'State) }

let reducerf (zero : 'R) (apply : 'R -> 'A -> Context<'R>) =
  { new Reducer<'A, 'R, 'R> with
      member x.zero = zero
      member x.apply state a = apply state a
      member x.isReduced state = false
      member x.complete state = state }

type Educed<'XS> = (obj * 'XS) option

type Educers = Educers with
  // typeclass Educer<'R<_>> =
  //   abstract step<'A> : 'R<'A> -> Educed<'A, 'R<'A>>

  static member inline Step (xs : _ list) : Educed<_ list> =
    match xs with
    | []      -> None
    | x :: xs -> Some (box x, xs)

  static member inline Step (xs : _ seq) : Educed<_ seq> =
    match xs with
    | xs when Seq.isEmpty xs -> None
    | xs -> Some (box (Seq.head xs), Seq.skip 1 xs)

  static member inline Step (xs : System.Collections.IEnumerator) : Educed<_> =
    if xs.MoveNext() then

  static member inline Step (mx : _ option) : Educed<_ option> =
    mx
    |> Option.bind (fun x -> Some (box x, None))

type Transducer<'A, 'B> =
  abstract apply<'R> : ReducerT<'A, 'R> -> ReducerT<'B, 'R>

// def transduce[R[_]:Educible, A, B, S](r: R[A], t: Transducer[B, A], f: Reducer[B, S]): Context[S] = 
//    educe(r, t(f))

type Cat<'A>() =
  interface Transducer<'A, 'A> with
    member x.apply r = r

let cat () : Transducer<'A, 'A> = upcast Cat ()

/// This helper performs the basic transformation for a stateless transducer.
type Proxy<'A, 'B, 'R, 'State>(f : Reducer<'A, 'R, 'State>,
                               g : 'State -> 'B -> Context<'State>) =
  interface Reducer<'B, 'R, 'State> with
    member x.zero = f.zero
    member x.apply state b = g state b
    member x.isReduced state = f.isReduced state
    member x.complete state = f.complete state

let proxy f g : Reducer<_, _, _> =
  upcast Proxy (f, g)

/// Fundamental Mapping transducer
let map f =
  { new Transducer<'A, 'B> with
      member x.apply r =
        proxy r <| fun state b ->
          r.apply state (f b)
  }

let filter pred =
  { new Transducer<'A, 'A> with
      member x.apply r =
        proxy r <| fun state a ->
          if pred a then r.apply state a else inContext state
  }

let remove pred = filter (pred >> not)

type Take<'A>(n) =
  interface Transducer<'A, 'A> with
    member x.apply (r : ReducerT<'A, 'R>) =
      reducer (r.zero, n)
              (fun (so, counter) a ->
                mapContext (fun so' -> so', counter - 1UL) (r.apply so a))
              (fun (so, counter) -> counter = 0UL || r.isReduced so)
              (fun (so, counter) -> r.complete so)

let take n : Transducer<_, _> =
  upcast Take n

let inline internal stepDefaults (a: ^a, _: ^b) =
  ((^a or ^b) : (static member Step : ^a -> Educed< ^a>) a)

let inline internal step (x : 'a) : Educed<'a> =
  stepDefaults (x, Educers)

// like xlist
let inline educe (xs : _ seq) (r : Reducer<'A, 'R, 'State>) : Context<'R> =
  use e = xs.GetEnumerator()
  let rec loop (state : 'State) : Context<'R> =
    if r.isReduced state then
      r.complete state |> inContext
    else
      if e.MoveNext() then
        r.apply state e.Current |> Job.bind loop
      else
        r.complete state |> inContext
  loop r.zero

educe [ 1; 2; 3 ] (reducerf 0 (fun acc x -> acc + x |> inContext))