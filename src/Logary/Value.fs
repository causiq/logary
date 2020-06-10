namespace Logary

open System
open System.Collections.Generic
open NodaTime

/// The main alias for time in Logary - the # of nanoseconds since 1970-01-01
/// in Unix time. In effect it denotes the # of nanoseconds passed in
/// international atomic time (TAI), but corrected for leap seconds – because
/// this is how system clocks normally work.
type EpochNanoSeconds = int64

[<RequireQualifiedAccess>]
type Currency =
  | USD
  | EUR
  | Other of name: string
  static member SEK = Other "SEK"
  interface IFormattable with
    member x.ToString(format, _) = // "G" or "c" format
      match if String.IsNullOrWhiteSpace format then "G" else format with
      | "G" -> match x with | USD -> "USD" | EUR -> "EUR" | Other o -> o
      | "c" | _ -> match x with | USD -> "$" | EUR -> "€" | Other o -> o

/// A unit type
[<RequireQualifiedAccess>]
type U =
  | Bits
  | Bytes
  | Seconds
  | Metres
  | Scalar
  | Amperes
  | Kelvins
  | Moles
  | Candelas
  | Percent
  | Watts
  | Hertz
  | Joules
  | Grams
  | Currency of currency: Currency
  | Other of unit:string
  // E.g. to denote nano-seconds since epoch;
  // 1474139353507070000 would be Scaled(Seconds, 10.**9.) since year 1970
  // so to get back to seconds, you'd divide the value by 10.**9.
  // E.g. an op that takes 5ms would be represented as
  // Gauge(5000000, Scaled(Seconds, 10.**9.)) (ns) OR:
  // Gauge(50000, Scaled(Seconds, 10**7.)) (ticks):
  | Scaled of unit:U * value:float
  | Offset of unit:U * value:float
  | Mul of unitA:U * unitB:U
  | Pow of ``base``:U * power:float
  | Div of nom:U * denom:U
  | Root of ``base``:U
  | Log10 of ``base``:U // Log of base:float * BaseUnit

  /// E.g. 5 degrees celsius is (5 + 273.15) K
  static member Celsius = U.Offset (Kelvins, +273.15)

  static member SquareMetres = U.Pow (U.Metres, 2.)
  static member KiloGrams = U.Scaled (U.Grams, 1.0e-3)
  /// https://en.wikipedia.org/wiki/Newton_(unit)
  static member Newtons = U.Div (U.Mul (U.KiloGrams, U.Metres), U.Pow (U.Seconds, 2.))
  /// https://en.wikipedia.org/wiki/Pascal_(unit)
  static member Pascal = U.Div (U.Newtons, U.SquareMetres)

  /// 5 min = 5 / (1/60) seconds = 360 s
  static member Minutes = U.Scaled (U.Seconds, 1. / 60.)
  static member Hours = U.Scaled (U.Seconds, 1. / 3600.)
  static member Days = U.Scaled (U.Seconds, 1. / (24. * 3600.))

  static member ReqsPerSecond = U.Div (U.Other "requests", U.Seconds)

  member x.name: string option =
    match x with
    | Bits ->
      Some "bits"
    | Bytes ->
      Some "bytes"
    | Seconds ->
      Some "seconds"
    | Metres ->
      Some "metres"
    | Scalar ->
      Some "units"
    | Amperes ->
      Some "amperes"
    | Kelvins ->
      Some "kelvins"
    | Joules ->
      Some "Joules"
    | Grams ->
      Some "Grams"
    | Moles ->
      Some "moles"
    | Candelas ->
      Some "candelas"
    | Percent ->
      Some "percent"
    | Watts ->
      Some "watts"
    | Hertz ->
      Some "hertz"
    | Other u ->
      Some u
    | Currency c ->
      Some ((c :> IFormattable).ToString("G", Culture.invariant))
    | Scaled _
    | Offset _
    | Mul _
    | Pow _
    | Div _
    | Root _
    | Log10 _ ->
      None

  member x.symbol =
    match x with
    | Bits -> "bit"
    | Bytes -> "B"
    | Seconds -> "s"
    | Metres -> "m"
    | Scalar -> ""
    | Amperes -> "A"
    | Kelvins -> "K"
    | Joules -> "J"
    | Grams -> "g"
    | Moles -> "mol"
    | Candelas -> "cd"
    | Percent -> "%"
    | Watts -> "W"
    | Hertz -> "Hz"
    | Currency c ->
      (c :> IFormattable).ToString("c", Culture.invariant)
    | Other other -> other
    | Scaled (units, scale) -> sprintf "%s/%f" units.symbol scale
    | Offset (units, offset) ->
      sprintf "%s %s %f" units.symbol (if offset < 0. then "-" else "+") offset
    | Mul (a, b) -> String.Concat [ "("; a.symbol; "*"; b.symbol; ")" ]
    | Pow (a, b) -> String.Concat [ a.symbol; "^("; string b; ")" ]
    | Div (a, b) -> String.Concat [ a.symbol; "/"; b.symbol ]
    | Root a -> String.Concat [ "sqrt("; a.symbol; ")" ]
    | Log10 a -> String.Concat [ "log10("; a.symbol; ")" ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module U =

  type UnitOrientation =
    | Prefix
    | Suffix

  let formatValue (v: 'a when 'a :> IFormattable) = v.ToString("G", Culture.invariant)

  let scaleSeconds: float -> float * string =
    ((*) (float Constants.NanosPerSecond)) >> int64 >> function
    | value when value < Constants.NanosPerSecond / 1000000L ->
      float Constants.NanosPerSecond, "ns"
    | value when value < Constants.NanosPerSecond / 1000L ->
      float (Constants.NanosPerSecond / 1000L), "µs"
    | value when value < Constants.NanosPerSecond ->
      float (Constants.NanosPerSecond / 1000000L), "ms"
    | value when value < 60L * Constants.NanosPerSecond ->
      1., "s"
    | value when value < 3600L * Constants.NanosPerSecond ->
      1. / 60., "min"
    | value when value < 86400L * Constants.NanosPerSecond ->
      1. / 3600., "h"
    | value ->
      1. / 86400., "days"

  // https://en.wikipedia.org/wiki/International_System_of_U#Prefixes
  let multiplePrefixes =
    [| ""; "k"; "M"; "G"; "T"; "P"; "E"; "Z" |]

  let fractionsPrefixes: string[] =
    [| "m"; "μ"; "n"; "p"; "f"; "a"; "z"; "y" |]

  let scaleBy10 (units: U) (value: float): float * string =
    let value = abs value
    if value = 0. || value = infinity then 1., units.symbol else
    let fraction = value < 1.
    let prefixes = if fraction then fractionsPrefixes else multiplePrefixes
    let index =
      // at boundaries, like 0.001 s we want index to be (abs(-3) - 1) / 3
      // done with integer division
      let ioffset = if fraction then -1. else 0.
      let tenPower = ceil (abs (log10 value)) // 3 from abs (log10 (1e-3)) // ceil is for handling e.g. log10 0.00099 = -3.004364805
      let index = int (tenPower + ioffset) / 3 // each index in steps of a thousand
      let maxIndex = prefixes.Length - 1
      //printfn "(index) ioffset=%f, tenPower=%f, index=%i, maxIndex=%i" ioffset tenPower index maxIndex
      min index maxIndex
    let scaled = if fraction then 10.**(float (index + 1) * 3.) else 1. / 10.**(float index * 3.)
    //printfn "to scale with factor=%f, symbol=%s, index=%i" scaled symbol index
    scaled,
    sprintf "%s%s" prefixes.[index] units.symbol

  let scaleBytes (value: float): float * string =
    let log2 x = log x / log 2.
    let prefixes = [| ""; "Ki"; "Mi"; "Gi"; "Ti"; "Pi" |] // note the capital K and the 'i'
    let index = int (log2 value) / 10
    1. / 2.**(float index * 10.),
    sprintf "%s%s" prefixes.[index] U.Bytes.symbol

  /// Takes a function that returns a *factor* (not the value multiplied)
  /// by the factor!
  let calculate (calcFactor: float -> float * string) =
    fun (value: float) ->
      let factor, unitStr = calcFactor value
      value * factor, unitStr

  // Given a Unit, returns the scaled value and a composite unit (e.g. MiB or µs)
  let rec scale (units: U) value: float * string =
    let noopScale v = 1., units.symbol
    match units with
    | U.Bytes ->
      calculate scaleBytes value
    | U.Bits
    | U.Metres
    | U.Scalar
    | U.Amperes
    | U.Kelvins
    | U.Joules
    | U.Grams
    | U.Moles
    | U.Candelas
    | U.Watts
    | U.Hertz
    | U.Currency _ ->
      //printfn "scaling value=%f, units=%A" value units
      calculate (scaleBy10 units) value
    | U.Offset _
    | U.Mul (_, _)
    | U.Pow (_, _)
    | U.Div (_, _)
    | U.Root _
    | U.Log10 _
    | U.Other _ ->
      calculate noopScale value
    | U.Percent _ ->
      calculate (fun _ -> 100., U.Percent.symbol) value
    | U.Seconds ->
      calculate scaleSeconds value
    | U.Scaled (iu, scalef) ->
      scale iu (value / scalef)



  let formatWithUnit orient (units: U) value =
    let fval = formatValue value
    match units.symbol with
    | "" ->
      fval
    | funit when orient = Prefix ->
      String.Concat [ funit; " "; fval ]
    | funit ->
      String.Concat [ fval; " "; funit ]

  /// Convert the string to a unit, defaulting to U.Other if unsuccessful.
  let parse (units: string) =
    match String.toLowerInvariant units with
    | "seconds" | "s" ->
      U.Seconds
    | "milliseconds" | "ms" ->
      U.Scaled (U.Seconds, 1.0e3)
    | "microseconds" | "us" | "µs" ->
      U.Scaled (U.Seconds, 1.0e6)
    | "nanoseconds" | "ns" ->
      U.Scaled (U.Seconds, 1.0e9)
    | "bit" | "bits" ->
      U.Bits
    | "b" | "bytes" ->
      U.Bytes
    | "m" | "metres" ->
      U.Metres
    | "" ->
      U.Scalar
    | "a" | "amps" | "amperes" ->
      U.Amperes
    | "k" ->
      U.Kelvins
    | "mol" | "moles" ->
      U.Moles
    | "cd" | "candelas" ->
      U.Candelas
    | "%" ->
      U.Percent
    | "w" ->
      U.Watts
    | "hz" | "hertz" ->
      U.Hertz
    | "" | "scalar" | "unit" | "units" | "item" | "items" ->
      U.Scalar
    | _ ->
      U.Other units

[<Struct>]
type PointName =
  PointName of hierarchy:string[]
with
  member x.isEmpty =
    match x with
    | PointName xs when xs.Length = 0 -> true
    | _ -> false

  override x.ToString() =
    let (PointName hiera) = x in String.concat "." hiera

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PointName =
  let empty = PointName Array.empty

  let (|Empty|_|) (pn: PointName) =
    if pn.isEmpty then Some () else None

  [<CompiledName "OfSingle">]
  let ofSingle (segment: string) =
    PointName [| segment |]

  [<CompiledName "OfList">]
  let ofList (hiera: string list) =
    PointName (Array.ofList hiera)

  /// Creates a point name of the array. As a performance optimisation, Logary
  /// assumes that you do not change the array contents afterwards, and will
  /// treat this value like an immutable value.
  [<CompiledName "OfArray">]
  let ofArray (hiera: string[]) =
     PointName hiera

  [<CompiledName "Parse">]
  let parse (s: string) =
    String.splita '.' s |> ofArray

  [<CompiledName "Format">]
  let format (pn: PointName) =
    pn.ToString()

  [<CompiledName "SetEnding">]
  let setEnding (nameEnding: string) (PointName segments as original) =
    if String.IsNullOrWhiteSpace nameEnding then original else
    PointName (Array.append segments [| nameEnding |])

/// Allows you to clearly delineate the accuracy and type of the measurement/gauge.
[<Struct; RequireQualifiedAccess>]
type Value =
  /// A CLR Double / F# float represented as a DU case
  | Float of float: float
  /// A CLR Int64 / F# int64 represented as a DU case
  | Int64 of int64: int64
  | BigInt of bigint: bigint
  | Fraction of numerator:int64 * denominator:int64
  /// Strings
  | Str of string: string
  /// Booleans
  | Bool of boolean: bool

  /// Convert the Value to a float (best as possible; this **may** lead to
  /// a loss of accuracy).
  member x.asFloat =
    match x with
    | Value.Float f -> f
    | Value.Int64 i -> float i
    | Value.BigInt i -> float i
    | Value.Fraction (_, d) when d = 0L -> 0.
    | Value.Fraction (n, d) -> float n / float d
    | Value.Bool b when b -> 1.
    | _ -> 0.

  override x.ToString() =
    let iif = x :> IFormattable
    iif.ToString("G", Culture.invariant)

  /// https://docs.microsoft.com/en-us/dotnet/api/system.iformattable?view=netcore-3.1
  interface IFormattable with
    member x.ToString(format, fp) =
      match x with
      | Float f -> f.ToString(format, fp)
      | Int64 i -> i.ToString(format, fp)
      | BigInt bi -> bi.ToString(format, fp)
      | Fraction (n, d) -> String.Concat [| n.ToString(format, fp) ; "/"; d.ToString(format, fp) |]
      | Str s -> s
      | Bool b when b -> "true"
      | Bool _ -> "false"

[<Struct>]
type Gauge =
  Gauge of Value * U
with
  member x.value =
    let (Gauge (v, _)) = x in v
  member x.unit =
    let (Gauge (_, u)) = x in u

  override x.ToString() =
    let (Gauge (v, u)) = x
    let gv, gu = U.scale u v.asFloat
    sprintf "%.2f %s" gv gu

  static member ofNanos (ns: Value) =
    Gauge (ns, U.Scaled (U.Seconds, float Constants.NanosPerSecond))
  static member ofNanos (ns: int64) =
    Gauge (Value.Int64 ns, U.Scaled (U.Seconds, float Constants.NanosPerSecond))
  static member ofNanos (ns: float) =
    Gauge (Value.Float ns, U.Scaled (U.Seconds, float Constants.NanosPerSecond))
  static member ofMillis (ms: Value) =
    Gauge (ms, U.Scaled (U.Seconds, float Constants.MillisPerSecond))
  static member ofMillis (ms: float) =
    Gauge (Value.Float ms, U.Scaled (U.Seconds, float Constants.MillisPerSecond))
  static member ofMillis (ms: int64) =
    Gauge (Value.Int64 ms, U.Scaled (U.Seconds, float Constants.MillisPerSecond))
  static member ofBclTicks (bclTicks: Value) =
    Gauge (bclTicks, U.Scaled (U.Seconds, float Constants.TicksPerSecond))
  static member ofBclTicks (bclTicks: int64) =
    Gauge (Value.Int64 bclTicks, U.Scaled (U.Seconds, float Constants.TicksPerSecond))
  static member ofBclTicks (bclTicks: float) =
    Gauge (Value.Float bclTicks, U.Scaled (U.Seconds, float Constants.TicksPerSecond))
  static member ofStopwatchTicks (swTicks: Value) =
    Gauge (swTicks, U.Scaled (U.Seconds, float System.Diagnostics.Stopwatch.Frequency))
  static member ofStopwatchTicks (swTicks: int64) =
    Gauge (Value.Int64 swTicks, U.Scaled (U.Seconds, float System.Diagnostics.Stopwatch.Frequency))
  static member ofStopwatchTicks (swTicks: float) =
    Gauge (Value.Float swTicks, U.Scaled (U.Seconds, float System.Diagnostics.Stopwatch.Frequency))
  static member ofDuration (duration: Duration) =
    Gauge.ofBclTicks duration.BclCompatibleTicks
