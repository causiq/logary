// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r "../../../packages/FSharp.Core/lib/net40/FSharp.Core.dll"
#r "bin/Debug/FSharp.Actor.dll"
#r "bin/Debug/Logary.dll"
#r "../../../packages/NodaTime/lib/net35-Client/NodaTime.dll"
#r "../../../packages/protobuf-net/lib/net40/protobuf-net.dll"
#load "../Logary.Targets.Riemann/ProtoBufUtils.fs"
#load "Messages.fs"
#load "Constants.fs"
#load "Types.fs"
#load "Client.fs"
#load "Targets_Heka.fs"
open Logary.Targets.Heka
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

    let scaleSeconds = ((*) 1000000000000L) >> function
      | value when value < 1000L -> 1L
      | value when value < 1000000L -> 1000L
      | value when value < 1000000000L -> 1000000L
      | value when value < 1000000000000L -> 1000000000L
      | value when value < 60L * 1000000000000L -> 1000000000000L
      | value when value < 3600L * 1000000000000L -> 3600L * 1000000000000L
      | value -> 86400L * 1000000000000L

    // grafana/public/app/kbn.js:374@g20d5d0e
    let doScale factor (scaledUnits : _ list) =
      fun value decimals scaledDecimals ->
        (value : float), "KiB"
          
    let scale units =
      match units with
      | Bit -> (fun _ -> 1000L), ["b"; "Kib"; "Mib"; "Gib"; "Tib"; "Pib"; "Eib"; "Zib"; "Yib"]
      | Byte -> (fun _ -> 1024L), ["B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]
      | Second -> scaleSeconds, ["ns"; "µs"; "ms"; "s"; "min"; "h"; "days"]
      ||> doScale

    let formatWithUnit orient un v =
      let fv = formatValue v
      match orient with
      | Prefix ->
        sprintf "%s %s" (formatUnit v un) fv
      | Suffix -> 
        sprintf "%s %s" fv (formatUnit v un)


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

  type DataPointValue =
    /// Value at point in time
    | Gauge of Value
    /// A rate, hence is Value per TimeUnit
    | RateOf of Value * TimeUnit
    /// Any sort of derived measure
    | Derived of Value * Units

  type Field =
    { name  : PointName
      unit  : Units
      value : ComplexValue }

  type LineValue =
    | Measure of DataPointValue
    /// All simple-valued fields' values can be templated into the template string
    /// when outputting the value in the target.
    | LogLine of template:string

open DataModel

type Message =
  { name      : PointName
    value     : LineValue
    fields    : Field list
    context   : LogContext
    timestamp : Instant }