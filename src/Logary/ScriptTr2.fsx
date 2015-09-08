let mapping : ('a -> 'b) -> ('r -> 'b -> 'r) -> ('r -> 'a -> 'r) =
  fun f red1 ->
    fun state item ->
      red1 state (f item)

let filtering : ('a -> bool) -> ('r -> 'a -> 'r) -> ('r -> 'a -> 'r) =
  fun p xf r a -> if p a then xf r a else r

let flatmapping : ('a -> 'b list) -> ('r -> 'b -> 'r) -> ('r -> 'a -> 'r) =
  fun f xf r a -> List.fold xf r (f a)

let conjRed xs x = xs @ [x]

let xlist (tr : ('r -> 'b -> 'r) -> ('r -> 'a -> 'r)) =
  List.fold (tr conjRed) [] // fold = educer,

let xmap : ('a -> 'b) -> 'a list -> 'b list =
  fun f -> xlist <| mapping f

let xfilter : ('a -> bool) -> 'a list -> 'a list =
  fun p -> xlist <| filtering p

let xflatmap : ('a -> 'b list) -> 'a list -> 'b list =
  fun f -> xlist <| flatmapping f

// transducer
let xform (r1 : ('r -> int -> 'r)) : ('r -> int -> 'r) =
  mapping ((+) 1) r1 // << filtering (fun x -> x % 2 = 0) << flatmapping (fun x -> printfn "fm: %A" x ; [0 .. x])

printfn "%A" <| xlist xform [1..5]
printfn "%A" <| xlist (mapping ((+) 1) << filtering (fun x -> x % 2 = 0) << flatmapping (fun x -> printfn "fm: %A" x ; [0 .. x])) [1..5]


(*let reduce (xs : _ seq) (reducer : Reducer<'A, 'R, 'State>) : Job<'R> =
   let mutable reduced = false
   let mutable state = reducer.zero
   for x in xs do // this will eagerly consume the full sequence
     if reduced then
      ()
     else
       state <- Job.bind (fun state' -> reducer.apply state' x) state
   state |> Job.bind reducer.complete *)
