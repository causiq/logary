namespace Logary.Internals

open System
open System.Threading

type DVar<'a> = private { mutable cell : 'a ; event : Event<'a> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DVar =

  let create (a:'a) : DVar<'a> =
    { cell = a ; event = new Event<'a>() }

  let get (d:DVar<'a>) : 'a =
    d.cell

  let put (a:'a) (d:DVar<'a>) : unit =
    Interlocked.Exchange (&d.cell, a) |> ignore
    d.event.Trigger a

  let inline set d a =
    put a d

  let ping (d:DVar<'a>) =
    put (get d) d

  let changes (d:DVar<'a>) : IEvent<'a> =
    d.event.Publish

  let subs (d:DVar<'a>) (f:'a -> unit)  : unit =
    d |> changes |> Event.add f

  let iter (f:'a -> unit) (d:DVar<'a>) =
    f (get d)
    subs d f

  let map (f:'a -> 'b) (v:DVar<'a>) : DVar<'b> =
    let dv = create (f (get v))
    subs v (f >> set dv)
    dv

  let ap (f:DVar<'a -> 'b>) (v:DVar<'a>) : DVar<'b> =
    let b = (get f) (get v)
    let db = create b
    subs f <| fun f -> let b = f (get v) in put b db
    subs v <| fun a -> let f = get f in let b = f a in put b db
    db

  let combineLatestWith (f:'a -> 'b -> 'c) (a:DVar<'a>) (b:DVar<'b>) : DVar<'c> =
    ap (ap (create f) a) b

  let combineLatest (a:DVar<'a>) (b:DVar<'b>) : DVar<'a * 'b> =
    combineLatestWith (fun a b -> a,b) a b

  let toFun (v:DVar<'a -> 'b>) : 'a -> 'b =
    fun a -> (get v) a

  let mapFun (f:'c -> ('a -> 'b)) (d:DVar<'c>) : 'a -> 'b =
    map f d |> toFun

  let ofEvent (initial:'a) (e:IEvent<'a>) : DVar<'a> =
    let dv = create initial
    e |> Event.add (fun a -> put a dv)
    dv

  let ofObservable (initial:'a) (e:IObservable<'a>) : DVar<'a> =
    let dv = create initial
    e |> Observable.add (fun a -> put a dv)
    dv

  let bindToRef (r:'a ref) (a:DVar<'a>) =
    r := (get a)
    subs a <| fun a -> r := a

  let update (f:'a -> 'a) (d:DVar<'a>) : unit =
    put (f (get d)) d

  let updateIfDistinctBy (key:'a -> 'k) (update:'a -> 'a) (d:DVar<'a>) : bool =
    let current = get d
    let currentKey = key current
    let updated = update current
    let updatedKey = key updated
    if currentKey <> updatedKey then
      put updated d
      true
    else
      false

  let updateIfDistinct (update:'a -> 'a) (d:DVar<'a>) : bool =
    updateIfDistinctBy id update d

  let distinctBy (key:'a -> 'k) (d:DVar<'a>) : DVar<'a> =
    let a = get d
    let mutable prevKey = key a
    let d' = create a
    subs d <| fun a ->
      let key = key a
      if key <> Interlocked.Exchange (&prevKey, key) then
        put a d'
    d'

  let distinct (d:DVar<'a>) : DVar<'a> =
    distinctBy id d

  let extract = get

  let extend (f:DVar<'a> -> 'b) (da:DVar<'a>) : DVar<'b> =
    let b = f da
    let db = create b
    subs da (fun _ -> let b = f da in put b db)
    db


  let choice (cd:Choice<DVar<'a>, DVar<'b>>) : DVar<Choice<'a, 'b>> =
    match cd with
    | Choice1Of2 da -> da |> map Choice1Of2
    | Choice2Of2 db -> db |> map Choice2Of2

  let observe (d:DVar<'a>) : 'a * IEvent<'a> =
    (get d),(changes d)

  let choose (first:DVar<'a>) (second:DVar<'a>) : DVar<'a> =
    let da = create (get first)
    Event.merge (changes first) (changes second) |> Event.add (set da)
    da
