namespace Logary

open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units =
  open YoLo

  type UnitOrientation =
    | Prefix
    | Suffix

  let rec formatValue = function
    | Float f ->
      f.ToString(Culture.invariant)
    | Int64 i ->
      i.ToString(Culture.invariant)
    | BigInt bi ->
      bi.ToString(Culture.invariant)
    | Fraction (n, d) ->
      sprintf "%s/%s" (n.ToString(Culture.invariant)) (d.ToString(Culture.invariant))

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

  // https://en.wikipedia.org/wiki/International_System_of_Units#Prefixes
  let multiplePrefixes =
    [| ""; "k"; "M"; "G"; "T"; "P"; "E"; "Z" |]

  let fractionsPrefixes: string[] =
    [| "m"; "μ"; "n"; "p"; "f"; "a"; "z"; "y" |]

  let scaleBy10 (units: Units) (value: float): float * string =
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
    sprintf "%s%s" prefixes.[index] Bytes.symbol

  /// Takes a function that returns a *factor* (not the value multiplied)
  /// by the factor!
  let calculate (calcFactor: float -> float * string) =
    fun (value: float) ->
      let factor, unitStr = calcFactor value
      value * factor, unitStr

  // Given a Unit, returns the scaled value and a composite unit (e.g. MiB or µs)
  let rec scale (units: Units) value: float * string =
    let noopScale v = 1., units.symbol
    match units with
    | Bytes ->
      calculate scaleBytes value
    | Bits
    | Metres
    | Scalar
    | Amperes
    | Kelvins
    | Joules
    | Grams
    | Moles
    | Candelas
    | Watts
    | Hertz ->
      //printfn "scaling value=%f, units=%A" value units
      calculate (scaleBy10 units) value
    | Offset _
    | Mul (_, _)
    | Pow (_, _)
    | Div (_, _)
    | Root _
    | Log10 _
    | Other _ ->
      calculate noopScale value
    | Percent _ ->
      calculate (fun v -> 100., Percent.symbol) value
    | Seconds ->
      calculate scaleSeconds value
    | Scaled (iu, scalef) ->
      scale iu (value / scalef)

  let formatWithUnit orient (units: Units) value =
    let fval = formatValue value
    match units.symbol with
    | "" ->
      fval
    | funit when orient = Prefix ->
      String.Concat [ funit; " "; fval ]
    | funit ->
      String.Concat [ fval; " "; funit ]

  /// Try to convert the string to a unit.
  let tryParse (units: string) =
    match String.toLowerInvariant units with
    | "seconds" | "s" ->
      Units.Seconds
    | "milliseconds" | "ms" ->
      Units.Scaled (Units.Seconds, 1.0e3)
    | "microseconds" | "us" | "µs" ->
      Units.Scaled (Units.Seconds, 1.0e6)
    | "nanoseconds" | "ns" ->
      Units.Scaled (Units.Seconds, 1.0e9)
    | "bit" | "bits" ->
      Bits
    | "b" | "bytes" ->
      Bytes
    | "m" | "metres" ->
      Metres
    | "" ->
      Scalar
    | "a" | "amps" | "amperes" ->
      Amperes
    | "k" ->
      Kelvins
    | "mol" | "moles" ->
      Moles
    | "cd" | "candelas" ->
      Candelas
    | "%" ->
      Percent
    | "w" ->
      Watts
    | "hz" | "hertz" ->
      Hertz
    | "" | "scalar" | "unit" | "units" | "item" | "items" ->
      Units.Scalar
    | _ ->
      Units.Other units
