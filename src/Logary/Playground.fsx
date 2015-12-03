#!/usr/bin/env fsharpi
#I "bin/Debug"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "Logary.dll"
#r "NodaTime.dll"
#r "FsMachines.dll"
open System
open Hopac

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
  { new ReducerT<'A, 'R> with
      member x.zero = box zero
      member x.apply state a = apply (state :?> 'R) a |> mapContext id
      member x.isReduced state = false
      member x.complete state = state :?> 'R }

type Transducer<'A, 'B> =
  abstract apply<'R> : ReducerT<'A, 'R> -> ReducerT<'B, 'R>

let compose (tb : Transducer<_, _>) (ta : Transducer<_, _>) =
  { new Transducer<'C, 'B> with
      member x.apply fc = tb.apply (ta.apply fc)  }

let (<.<) tb ta = compose ta tb
let (>.>) ta tb = compose ta tb

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

let inline educe (r : Reducer<'A, 'R, 'State>) (xs : _ seq) : Context<'R> =
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

// educe (reducerf 0 (fun acc x -> acc + x |> inContext)) [ 1; 2; 3 ] |> run

module List =
  let consReducer _ : ReducerT<'A, 'A list> =
    reducerf [] (fun xs x -> x :: xs |> inContext)

  let transduce (tr : Transducer<_, _>) =
    educe (tr.apply (consReducer ()))

let tf = take 2UL >.> map (fun x -> x * 4)

printfn "%A" (List.transduce tf [ 1; 2; 3 ] |> run)
