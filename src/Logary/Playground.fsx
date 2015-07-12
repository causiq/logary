#!/usr/bin/env fsharpi
#I "bin/Debug"
#r "FSharp.Actor.dll"
#r "Logary.dll"
#r "NodaTime.dll"
#I "../../packages/Hopac/lib/net45"
#r "Hopac.Core.dll"
#r "Hopac.dll"
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
    let scaleValue = function
      | DataModel.PointValue.Gauge (value, units) ->
        match value with
        | Float f -> Float (scale * f), units
        | x -> x, units
        |> DataModel.PointValue.Gauge
      | x -> x
    fun m -> { m with Message.value = scaleValue m.value } :: []

let segmentJob (seg : Segment) (inp : BoundedMb<_>) (outp : BoundedMb<_>) =
  let proc = Job.delay <| fun () ->
    BoundedMb.take inp |>>? seg >>=? BoundedMb.put outp
  Job.foreverServer proc

let mb : BoundedMb<int> = BoundedMb.create 2 |> run
BoundedMb.put mb 42 <|>? timeOutMillis 10 |> run
BoundedMb.take mb |>>? printfn "Got %A" <|>? timeOutMillis 1 |> run

let msg = Message.Create(["cpu_jiffies"], PointValue.Gauge(Value.Float 56., Units.Div(BaseUnit.Metre, BaseUnit.Second)))

let loggerMb : BoundedMb<Message> = BoundedMb.create 2 |> run
let targetMb : BoundedMb<Message list> = BoundedMb.create 2 |> run

let scaleSegment = segmentJob (Segments.scale 4.) loggerMb targetMb

BoundedMb.put loggerMb msg <|>? timeOutMillis 10 |> run

run scaleSegment

let msgs = BoundedMb.take targetMb
           <|>? (timeOutMillis 1 |>>? (fun () -> []))
           |> run
