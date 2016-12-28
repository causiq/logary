namespace Logary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Units =

  type UnitOrientation =
    | Prefix
    | Suffix
  let rec formatValue = function
    | String s -> s
    | Bool true -> "true"
    | Bool false -> "false"
    | Float f -> f.ToString()
    | Int64 i -> i.ToString()
    | BigInt bi -> bi.ToString()
    | Binary (b, ct) -> System.BitConverter.ToString b |> fun s -> s.Replace("-", "")
    | Fraction (n, d) -> sprintf "%d/%d" n d
    | Array values -> String.Concat ["["; values |> List.map formatValue |> String.concat ", "; "]"]
    | Object m -> "Object"

  let scaleSeconds : float -> float * string =
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

  let fractionsPrefixes : string[] =
    [| "m"; "μ"; "n"; "p"; "f"; "a"; "z"; "y" |]

  let scaleBy10 units (value : float) : float * string =
    let symbol = Units.symbol units
    if value = 0. || value = infinity || value = -infinity then 1., symbol else
    let fraction = value > -1. && value < 1.
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
    sprintf "%s%s" prefixes.[index] symbol

  let scaleBytes (value : float) : float * string =
    let log2 x = log x / log 2.
    let prefixes = [| ""; "Ki"; "Mi"; "Gi"; "Ti"; "Pi" |] // note the capital K and the 'i'
    let index = int (log2 value) / 10
    1. / 2.**(float index * 10.),
    sprintf "%s%s" prefixes.[index] (Units.symbol Bytes)

  /// Takes a function that returns a *factor* (not the value multiplied)
  /// by the factor!
  let calculate (calcFactor : float -> float * string) =
    fun (value : float) ->
      let factor, unitStr = calcFactor value
      value * factor, unitStr

  // Given a Unit, returns the scaling function and the list of units available.
  let rec scale units value : float * string =
    let noopScale v = 1., Units.symbol units
    match units with
    | Bytes ->
      calculate scaleBytes value
    | Bits
    | Metres
    | Scalar
    | Amperes
    | Kelvins
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
      calculate (fun v -> 100., Units.symbol Percent) value
    | Seconds ->
      calculate scaleSeconds value
    | Scaled (iu, scalef) ->
      scale iu (value / scalef)

  let formatWithUnit orient un value =
    let fval = formatValue value
    match Units.symbol un with
    | "" ->
      fval
    | funit when orient = Prefix ->
      String.Concat [ funit; " "; fval ]
    | funit ->
      String.Concat [ fval; " "; funit ]
