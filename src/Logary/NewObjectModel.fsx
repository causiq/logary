#!/usr/bin/env fsharpi
#r "../../packages/FSharp.Core/lib/net40/FSharp.Core.dll"
#I "bin/Debug"
#r "FSharp.Actor.dll"
#r "Logary.dll"
#r "NodaTime.dll"
#I "../../packages/Hopac/lib/net45"
#r "../../packages/Hopac/lib/net45/Hopac.Core.dll"
#r "../../packages/Hopac/lib/net45/Hopac.dll"

open NodaTime

module DataModel =

  type ContentType = string

  type Value =
    | String of string
    | Float of float
    | Int64 of int64
    | BigInt of bigint
    | Binary of byte [] * ContentType
    | Fraction of int * int

  type ComplexValue =
    | Value of Value
    | Object of Map<string, ComplexValue>
    | Array of ComplexValue list

  // when we start doing calculations:
  // https://msdn.microsoft.com/en-us/library/hh289750.aspx

  type BaseUnit =
    | Bit
    | Byte
    | Second
    | Metre
    | Scalar
    | Ampere
    | Kelvin
    | Mole
    | Candela

  type Units =
    | BaseUnit of BaseUnit
    | Mul of BaseUnit * BaseUnit
    | Pow of BaseUnit * BaseUnit
    | Div of BaseUnit * BaseUnit
    | Root of BaseUnit
    | Sqrt of BaseUnit
    | Log10 of BaseUnit // Log of base:float * BaseUnit

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Units =

    let symbol value = function
      | Bit -> "b"
      | Byte -> "B"
      | Second -> "s"
      | Metre -> "m"
      | Scalar -> ""
      | Ampere -> "A"
      | Kelvin -> "K"
      | Mole -> "mol"
      | Candela -> "cd"

    type UnitOrientation =
      | Prefix
      | Suffix

    let formatValue = function
      | String s -> s
      | Float f -> f.ToString()
      | Int64 i -> i.ToString()
      | BigInt bi -> bi.ToString()
      | Binary (b, ct) -> System.BitConverter.ToString b |> fun s -> s.Replace("-", "")
      | Fraction (n, d) -> sprintf "%d/%d" n d

    let scaleSeconds = int64 >> ((*) 1000000000000L) >> function
      | value when value < 1000L -> 1L
      | value when value < 1000000L -> 1000L
      | value when value < 1000000000L -> 1000000L
      | value when value < 1000000000000L -> 1000000000L
      | value when value < 60L * 1000000000000L -> 1000000000000L
      | value when value < 3600L * 1000000000000L -> 3600L * 1000000000000L
      | value -> 86400L * 1000000000000L

    // grafana/public/app/kbn.js:374@g20d5d0e
    let doScale (fFactor : float -> int64) (scaledUnits : string list) =
      fun (decimals : uint16) (scaledDecimals : uint16) (value : float) ->
        (value : float), "KiB"
          
    let scale units : uint16 -> uint16 -> float -> float*string =
      match units with
      | Bit -> (fun _ -> 1000L), ["b"; "Kib"; "Mib"; "Gib"; "Tib"; "Pib"; "Eib"; "Zib"; "Yib"]
      | Byte -> (fun _ -> 1024L), ["B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]
      | Second -> scaleSeconds, ["ns"; "µs"; "ms"; "s"; "min"; "h"; "days"]
      ||> doScale

    let formatWithUnit orient un v =
      match orient with
      | Prefix ->
        let value, prefix = scale un 3us 3us v
        //let value = formatValue value
        sprintf "%s %f" prefix value
      | Suffix -> 
        let value, suffix = scale un 3us 3us v
        sprintf "%f %s" value suffix


  type TraceId = int64
  type SpanId  = int64

  type Span =
    { traceId      : TraceId
      spanId       : SpanId
      parentSpanId : SpanId option
      sampled      : bool }

  type LogContext =
    { datacenter : string option
      hostname   : string
      service    : string
      ns         : string
      func       : string
      file       : string option
      lineNo     : uint32
      envVersion : string
      span       : Span option }

  type PointName = string list

  type PointValue =
    /// Value at point in time
    | Gauge of Value
    /// A rate, hence is Value per TimeUnit
    | RateOf of Value * float //seconds
    /// Any sort of derived measure
    | Derived of Value * Units
    /// All simple-valued fields' values can be templated into the template string
    /// when outputting the value in the target.
    | LogLine of template:string

  type Field =
    { name  : PointName
      unit  : Units
      value : ComplexValue }

open DataModel

type Message =
  { name      : PointName
    value     : PointValue
    fields    : Field list
    context   : LogContext
    timestamp : Instant }

open Hopac
open Hopac.Alt.Infixes
open Hopac.TopLevel

let mb : BoundedMb<int> = BoundedMb.create 2 |> run
BoundedMb.put mb 42 <|>? timeOutMillis 10 |> run
BoundedMb.take mb |>>? printfn "Got %A" <|>? timeOutMillis 1 |> run

// use case: pipeline of input mb, fn that changes the name, output mb
// arity(Message 
type Segment = Message -> Message list

/// sample segments you can tie together to scale/calculate
module Segments =
  let changeName toSomething : Segment =
    fun m -> { m with Message.name = toSomething } :: []

  let scale scale : Segment =
    let scaleValue = function
      | DataModel.PointValue.Gauge value ->
        match value with
        | Float f -> Float (scale * f)
        | x -> x
        |> DataModel.PointValue.Gauge
      | x -> x
    fun m -> { m with Message.value = scaleValue m.value } :: []

let segmentJob (seg : Segment) (inp : BoundedMb<_>) (outp : BoundedMb<_>) =
  let proc = Job.delay <| fun () ->
    BoundedMb.take inp |>>? seg >>=? BoundedMb.put outp
  Job.foreverServer proc

let loggerMb : BoundedMb<Message> = BoundedMb.create 2 |> run
let targetMb : BoundedMb<Message list> = BoundedMb.create 2 |> run
let scaleIt = segmentJob (Segments.scale 4.) loggerMb targetMb
