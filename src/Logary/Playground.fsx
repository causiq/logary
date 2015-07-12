#!/usr/bin/env fsharpi
#I "bin/Debug"
#r "FSharp.Actor.dll"
#r "Logary.dll"
#r "NodaTime.dll"
#I "../../packages/Hopac/lib/net45"
#r "Hopac.Core.dll"
#r "Hopac.dll"
open System
open Hopac
open Hopac.Alt.Infixes
open Hopac.TopLevel
open Logary
open Logary.DataModel

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

BoundedMb.put loggerMb msg <|>? timeOutMillis 10 |> run

BoundedMb.take targetMb <|>? (timeOutMillis 1 |>>? (fun () -> [])) |> run |> print