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
open Logary
open Logary.DataModel
open Logary.Metric.Reservoir

// use case: pipeline of input mb, fn that changes the name, output mb
// arity(Message 
type Segment = Message -> Message list

/// sample segments you can tie together to scale/calculate
module Segments =
  let changeName toSomething : Segment =
    fun m -> { m with Message.name = toSomething } :: []

  let scale scale : Segment =
    let rnd = System.Random()
    let scaleValue = function
      | PointValue.Gauge (value, units) ->
        match value with
        | Float f -> Float (scale * f * (rnd.NextDouble())), units
        | x -> x, units
        |> DataModel.PointValue.Gauge
      | x -> x
    fun m -> { m with Message.value = scaleValue m.value } :: []

let segmentJob (seg : Segment) (inp : BoundedMb<_>) (outp : BoundedMb<_>) =
  let proc = Job.delay <| fun () ->
    BoundedMb.take inp |>>? seg >>=? BoundedMb.put outp
  Job.foreverServer proc

let print xs =
  let rec fixp = function
  | [] -> ()
  | (x : Message) :: xs -> printfn "%A" x.value ; fixp xs
  fixp xs

let msg = Message.Create(["cpu_jiffies"], PointValue.Gauge(Value.Float 56., Units.Div(BaseUnit.Metre, BaseUnit.Second)))

let loggerMb : BoundedMb<Message> = BoundedMb.create 1 |> run
let targetMb : BoundedMb<Message list> = BoundedMb.create 1 |> run

segmentJob (Segments.scale 4.) loggerMb targetMb |> run

// BoundedMb.put loggerMb msg <|>? timeOutMillis 10 |> run
// BoundedMb.take targetMb <|>? (timeOutMillis 1 |>>? (fun () -> [])) |> run |> print

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
  List.fold (tr conjRed) []

let xmap : ('a -> 'b) -> 'a list -> 'b list =
  fun f -> xlist <| mapping f

let xfilter : ('a -> bool) -> 'a list -> 'a list =
  fun p -> xlist <| filtering p

let xflatmap : ('a -> 'b list) -> 'a list -> 'b list =
  fun f -> xlist <| flatmapping f

// transducer
let xform (r1 : ('r -> int -> 'r)) : ('r -> int -> 'r) =
  mapping ((+) 1) r1 // << filtering (fun x -> x % 2 = 0) << flatmapping (fun x -> printfn "fm: %A" x ; [0 .. x])

//printfn "%A" <| xlist xform [1..5]
printfn "%A" <| xlist (mapping ((+) 1) << filtering (fun x -> x % 2 = 0) << flatmapping (fun x -> printfn "fm: %A" x ; [0 .. x])) [1..5]