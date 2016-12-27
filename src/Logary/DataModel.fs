namespace Logary

open System
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open Logary
open Logary.Serialisation.Chiron
open Logary.Serialisation.Chiron.Operators
open Logary.Internals
open Logary.Internals.Aether
open Logary.Internals.Aether.Operators
open Logary.Internals.TypeShape

/// A content-type annotation for a byte-array.
type ContentType = string

/// The main alias for time in Logary - the # of nanoseconds since 1970-01-01
/// in Unix time. In effect it denotes the # of nanoseconds passed in
/// international atomic time (TAI), but corrected for leap seconds – because
/// this is how system clocks normally work.
type EpochNanoSeconds = int64

/// Logary's structured logging is centered around the `Value` type.
///
/// All objects logged into Logary are translated into a structure such as this
/// one.
///
/// The trade-off between passing all the CLR type information along versus
/// fitting it all into these cases is one of completeness versus convenience
/// in writing targets and computing on the data.
type Value =
  | String of string
  | Bool of bool
  | Float of float
  | Int64 of int64
  | BigInt of bigint
  | Binary of byte [] * ContentType
  | Fraction of int64 * int64
  | Object of Map<string, Value>
  | Array of Value list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Value =

  module Optic = 

    (* Epimorphisms *)

    let internal String__ =
      (function | String x -> Some x
                | _ -> None), String

    let internal Bool__ =
      (function | Bool x -> Some x
                | _ -> None), Bool

    let internal Float__ =
      (function | Float x -> Some x
                | _ -> None), Float

    let internal Int64__ =
      (function | Int64 x -> Some x
                | _ -> None), Int64

    let internal BigInt__ =
      (function | BigInt x -> Some x
                | _ -> None), BigInt

    let internal Binary__ =
      (function | Binary (bs, ct) -> Some (bs, ct)
                | _ -> None), Binary

    let internal Fraction__ =
      (function | Fraction (n, d) -> Some (n, d)
                | _ -> None), Fraction

    let internal Array__ =
      (function | Array x -> Some x
                | _ -> None), Array

    let internal Object__ =
      (function | Object x -> Some x
                | _ -> None), Object

    (* Prisms *)

    let String_ =
      Prism.ofEpimorphism String__

    let Bool_ =
      Prism.ofEpimorphism Bool__

    let Float_ =
      Prism.ofEpimorphism Float__

    let Int64_ =
      Prism.ofEpimorphism Int64__

    let BigInt_ =
      Prism.ofEpimorphism BigInt__

    let Binary_ =
      Prism.ofEpimorphism Binary__

    let Fraction_ =
      Prism.ofEpimorphism Fraction__

    let Array_ =
      Prism.ofEpimorphism Array__

    let Object_ =
      Prism.ofEpimorphism Object__

  let rec internal exceptionToStringValueMap (valueOfObject : obj->Value) (e : exn) =
    let fields =
      [ yield "type", String (e.GetType ()).FullName
        yield "message", String e.Message
        if e.TargetSite <> null then
          yield "targetSite", String (e.TargetSite.ToString ())
        if e.StackTrace <> null then
          yield "stackTrace", String e.StackTrace
        if e.Source <> null then
          yield "source", String e.Source
        if e.HelpLink <> null then
          yield "helpLink", String e.HelpLink
        if e.HResult <> 0 then
          yield "hResult", Int64 (int64 e.HResult)
        if e.Data <> null && e.Data.Count > 0 then
          yield "data", valueOfObject e.Data ]

    Map.ofSeq <|
      if e.InnerException <> null then
        ("inner", Object <| exceptionToStringValueMap valueOfObject e.InnerException) :: fields
      else
        fields

  [<CompiledName "OfObject">]
  let rec create : obj -> Value = function
    | null -> String ""
    // Built-in types
    | :? bool as b     -> Bool b
    | :? int8 as i     -> Int64 (int64 i)
    | :? uint8 as i    -> Int64 (int64 i)
    | :? int16 as i    -> Int64 (int64 i)
    | :? uint16 as i   -> Int64 (int64 i)
    | :? int32 as i    -> Int64 (int64 i)
    | :? uint32 as i   -> Int64 (int64 i)
    | :? int64 as i    -> Int64 (int64 i)
    | :? uint64 as i   -> Float (float i)
    | :? bigint as i   -> BigInt i
    | :? decimal as d  -> Float (float d)
    | :? float32 as f32-> Float (float f32)
    | :? float as f    -> Float f
    | :? char as c     -> String (string c)
    | :? string as s   -> String s

    // Common BCL types
    | :? Guid as g             -> String (string g)
    | :? DateTime as dt        -> String (dt.ToUniversalTime().ToString("o"))
    | :? DateTimeOffset as dto -> String (dto.ToString("o"))
    | :? TimeSpan as ts        -> String (ts.ToString())
    | :? System.Uri as u       -> String (u.ToString())
    | :? NodaTime.Duration as d-> String (d.ToString())
    | :? exn as e              -> Object (exceptionToStringValueMap create e)

    // Collections
    | :? (byte array) as bytes ->
      Binary (bytes, "application/octet-stream")

    | :? Array as arr ->
      [ for i in 0..arr.Length-1 do
          let v = arr.GetValue i
          if v <> null then yield create v ]
      |> Array

    | :? IEnumerable<KeyValuePair<string, obj>> as dict ->
      dict
      |> Seq.choose (function
        | KeyValue (k, v) when v <> null ->
          Some (k, create v)

        | otherwise ->
          None)

      |> Map.ofSeq
      |> Object

    //| :? Map<string, obj> as map -> Map.map (fun _ v -> fromObject v) map |> Object
    | :? IEnumerable<obj> as ie ->
      ie
      |> Seq.filter ((<>) null)
      |> Seq.map create
      |> Seq.toList
      |> Array

    | du when FSharpType.IsUnion (du.GetType()) ->
      let uci, fields = FSharpValue.GetUnionFields(du, du.GetType())
      match fields with
      | [||] ->
        String uci.Name
      | [|field|] ->
        Object <| Map [uci.Name, create field]
      | fields ->
        Object <| Map [uci.Name, create fields]

    // POCOs
    | a when a <> null ->
      a
      |> Map.ofObject
      |> Map.map (fun _ v -> create v)
      |> Object

    | otherwise ->
      failwithf "Cannot convert %A to a Value" otherwise

  /// Do an approximate conversion of the value to double
  let toDouble = function
    | String _ ->
      1.

    | Bool true ->
      1.

    | Bool false ->
      0.

    | Float f ->
      f

    | Int64 i64 ->
      float i64

    | BigInt bi ->
      float bi

    | Binary _ ->
      0.

    | Fraction (n, d) ->
      float n / float d

    | Object _ ->
      0.

    | Array _ ->
      0.

  /// Do an approximate conversion of the value to string
  let rec toString = function
    | String str ->
      str

    | Bool true ->
      "true"

    | Bool false ->
      "false"

    | Float f ->
      string f

    | Int64 i64 ->
      string i64

    | BigInt bi ->
      string bi

    | Binary (bytes, ct) ->
      sprintf "content-type:%s,base64:%s" ct (Convert.ToBase64String bytes)

    | Fraction (n, d) ->
      string (float n / float d)

    | Object _ ->
      "" // TO CONSIDER: do something with objects

    | Array values ->
      String.concat "," (values |> List.map toString)

type Units =
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
  | Other of units:string
  // E.g. to denote nano-seconds since epoch;
  // 1474139353507070000 would be Scaled(Seconds, 10.**9.) since year 1970
  // so to get back to seconds, you'd divide the value by 10.**9.
  // E.g. an op that takes 5ms would be represented as
  // Gauge(5000000, Scaled(Seconds, 10.**9.)) (ns) OR:
  // Gauge(50000, Scaled(Seconds, 10**7.)) (ticks):
  | Scaled of units:Units * scale:float
  | Offset of units:Units * offset:float
  | Mul of Units * Units
  | Pow of Units * Units
  | Div of Units * Units
  | Root of Units
  | Log10 of Units // Log of base:float * BaseUnit

  /// E.g. 5 degrees celsius is (5 + 273.15) K
  static member Celsius =
    Offset (Kelvins, +273.15)

  static member symbol = function
    | Bits -> "bit"
    | Bytes -> "B"
    | Seconds -> "s"
    | Metres -> "m"
    | Scalar -> ""
    | Amperes -> "A"
    | Kelvins -> "K"
    | Moles -> "mol"
    | Candelas -> "cd"
    | Percent -> "%"
    | Watts -> "W"
    | Hertz -> "Hz"
    | Other other -> other
    | Scaled (units, scale) -> sprintf "%s / %f" (Units.symbol units) scale
    | Offset (units, offset) ->
      sprintf "%s %s %f" (Units.symbol units)
                         (if offset < 0. then "-" else "+")
                         offset
    | Mul (a, b) -> String.Concat [ "("; Units.symbol a; "*"; Units.symbol b; ")" ]
    | Pow (a, b) -> String.Concat [ Units.symbol a; "^("; Units.symbol b; ")" ]
    | Div (a, b) -> String.Concat [ Units.symbol a; "/"; Units.symbol b ]
    | Root a -> String.Concat [ "sqrt("; Units.symbol a; ")" ]
    | Log10 a -> String.Concat [ "log10("; Units.symbol a; ")" ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Duration =
  open NodaTime
  let hours (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerHour

  let minutes (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerMinute

  let seconds (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerSecond

  let milliseconds (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerMillisecond

  let microseconds =
    ((*) 1000.) << milliseconds
  let ticks (dur : Duration) =
    float dur.Ticks
  let nanoseconds =
    ((*) 1000.) << microseconds

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

type PointName =
  PointName of hierarchy:string[]
with
  member x.isEmpty =
    match x with
    | PointName xs when xs.Length = 0 -> true
    | _ -> false

  override x.ToString() =
    let (PointName hiera) = x in String.concat "." hiera

  static member hierarchy_ =
    (fun (PointName h) -> h),
    fun v x -> PointName v

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PointName =

  let empty = PointName Array.empty

  [<CompiledName "OfSingle">]
  let ofSingle (segment : string) =
    PointName [| segment |]

  [<CompiledName "OfList">]
  let ofList (hiera : string list) =
    PointName (Array.ofList hiera)

  /// Creates a point name of the array. As a performance optimisation, Logary
  /// assumes that you do not change the array contents afterwards, and will
  /// treat this value like an immutable value.
  [<CompiledName "OfArray">]
  let ofArray (hiera : string[]) =
    PointName hiera

  [<CompiledName "Parse">]
  let parse (s: string) =
    String.splita '.' s |> ofArray

  [<CompiledName "Format">]
  let format (pn : PointName) =
    pn.ToString()

  [<CompiledName "SetEnding">]
  let setEnding (nameEnding : string) (PointName segments as original) =
    if nameEnding = null then original else
    PointName (Array.append segments [| nameEnding |])

type PointValue =
  /// Value at point in time
  | Gauge of Value * Units
  /// Any sort of derived measure
  | Derived of Value * Units
  /// All simple-valued fields' values can be templated into the template string
  /// when outputting the value in the target.
  | Event of template:string

/// Extensions for C#
type PointValue with
  /// Tries to get the gauge value
  member x.TryGetGauge([<Out>] gauge:byref<Tuple<Value, Units>>) =
    match x with
    | Gauge (value, units) ->
      gauge <- new Tuple<Value, Units>(value, units)
      true
    | _ ->
      false

  /// Tries to get the derived value
  member x.TryGetDerived([<Out>] derived:byref<Tuple<Value, Units>>) =
    match x with
    | Derived (value, units) ->
      derived <- new Tuple<Value, Units>(value, units)
      true
    | _ ->
      false

  /// Tries to get the Event template value (log message)
  member x.TryGetEvent([<Out>] template:byref<string>) =
    match x with
    | Event value ->
      template <- value
      true
    | _ ->
      false

type Field =
  Field of Value * Units option // move outside this module

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // remove when field moved outside
module Field =

  let inline initWithUnit value units =
    failwith "TODO"

  let inline init (value : ^a) =
    failwith "TODO"

  module Optic =

    let value_ : Lens<Field, Value> =
      (fun (Field (value, mUnits)) -> value),
      fun v (Field (value, mUnits)) -> Field (v, mUnits)

    let units_ : Prism<Field, Units> =
      (fun (Field (_, mUnits)) -> mUnits),
      fun units (Field (value, _)) -> Field (value, Some units)

/// This is record that is logged. It's capable of representing both metrics
/// (gauges) and events.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse template in
    /// (Event template) = message.value
    ///
    name      : PointName
    /// The main value for this metric or event. Either a Gauge, a Derived or an
    /// Event. (A discriminated union type)
    value     : PointValue
    /// The semantic-logging data.
    fields    : Map<PointName, Field>
    /// Where in the code? Who did the operation? What tenant did the principal
    /// who did it belong to? Put things your normally do 'GROUP BY' on in this
    /// Map.
    context   : Map<string, Value>
    /// How important? See the docs on the LogLevel type for details.
    level     : LogLevel
    /// When? nanoseconds since UNIX epoch.
    timestamp : EpochNanoSeconds }

    /// Gets the timestamp as NodaTime ticks (100 ns per tick). If you're getting
    /// for DateTime and/or DateTimeOffset, remember that those start at
    /// 0001-01-01.
    member x.timestampTicks : int64 =
      x.timestamp / Constants.NanosPerTick

/// Extensions to facilitate converting DateTime and DateTimeOffset to EpochNanoSeconds.
[<AutoOpen; Extension>]
module SystemDateEx =

  type DateTimeOffset with
    /// Gets the EpochNanoSeconds from the DateTimeOffset.
    [<Extension>]
    member x.timestamp : EpochNanoSeconds =
      (x.Ticks - DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks)
      * Constants.NanosPerTick

/// Helper functions for transforming DateTimeOffset to timestamps in unix epoch.
module DateTimeOffset =

  let private ticksAt1970 =
    DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks

  /// Get the DateTimeOffset ticks from EpochNanoSeconds
  let ticksUTC (epoch : EpochNanoSeconds) : int64 =
    epoch / Constants.NanosPerTick
    + ticksAt1970

  /// Get the DateTimeOffset from EpochNanoSeconds
  let ofEpoch (epoch : EpochNanoSeconds) : DateTimeOffset =
    DateTimeOffset(ticksUTC epoch, TimeSpan.Zero)

[<AutoOpen; Extension>]
module DurationEx =
  open NodaTime

  type Duration with
    [<Extension; CompiledName "ToGauge">]
    member dur.toGauge () =
      Int64 (dur.Ticks * Constants.NanosPerTick),
      Scaled (Seconds, float Constants.NanosPerSecond)

/// Extensions to facilitate reading Diagnostics.Stopwatch as a value that
/// suits Logary
[<AutoOpen; Extension>]
module StopwatchEx =
  open System.Diagnostics

  type Stopwatch with
    /// Convert the current value of the Stopwatch to a gauge's value and unit.
    [<Extension; CompiledName "ToGauge">]
    member sw.toGauge() : (Value * Units) =
      Int64 (sw.ElapsedTicks * Constants.NanosPerTick),
      Scaled (Seconds, float Constants.NanosPerSecond)

    [<Extension; CompiledName "Time">]
    static member time (fn : unit -> 'res) : 'res * (Value * Units) =
      let sw = Stopwatch.StartNew()
      let res = fn ()
      sw.Stop()
      res, sw.toGauge()

/// Open this module to log in a more succinct way.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
  open Hopac
  open NodaTime
  open System.Threading.Tasks
  open System.Diagnostics
  open Logary.Internals

  module Optic =

    let name_ : Lens<Message, PointName> =
      (fun (x : Message) -> x.name),
      fun v (x : Message) -> { x with name = v }

    let value_ : Lens<Message, PointValue> =
      (fun (x : Message) -> x.value),
      fun v (x : Message) -> { x with value = v }

    let fields_ : Lens<Message, _> =
      (fun (x : Message) -> x.fields),
      (fun v (x : Message) -> { x with fields = v })

    let context_ : Lens<Message, _> =
      (fun (x : Message) -> x.context),
      (fun v (x : Message) -> { x with context = v })

    let level_ : Lens<Message, LogLevel> =
      (fun (x : Message) -> x.level),
      (fun v (x : Message) -> { x with level = v })

    let timestamp_ : Lens<Message, EpochNanoSeconds> =
      (fun (x : Message) -> x.timestamp),
      (fun v (x : Message) -> { x with timestamp = v })

    let field_ name : Prism<Message, Field> =
      fields_ >-> Map.key_ name

    let fieldString_ name : Prism<Message, Field> =
      fields_ >-> Map.key_ (PointName.ofSingle name)

    let contextValue_ name : Prism<Message, _> =
      context_ >-> Map.key_ name

    /// Lens you can use to get the list of errors in this message.
    /// Also see Logary errors: https://gist.github.com/haf/1a5152b77ec64bf10fe8583a081dbbbf
    let errors_ =
      fieldString_ KnownLiterals.ErrorsFieldName
      >?> Field.Optic.value_
      >?> Value.Optic.Array_

    /// Lens to the context field 'service'.
    let service_ =
      contextValue_ KnownLiterals.ServiceContextName
      >?> Value.Optic.String_

  ///////////////// FIELDS ////////////////////

  /// Get a partial setter lens to a field
  [<CompiledName "SetField">]
  let inline setField name value message =
    Optic.set (Optic.fieldString_ name) (Field.init value) message

  /// Get a partial setter lens to a field with an unit
  [<CompiledName "SetFieldUnit">]
  let inline setFieldUnit name value units message =
    Optic.set (Optic.fieldString_ name) (Field.initWithUnit value units) message

  /// You can also choose to construct a Field yourself, using the object model
  /// that Logary has for its data. That way you don't have to rely on having
  /// static ToValue methods on your data objects.
  [<CompiledName "SetFieldValue">]
  let setFieldValue (name : string) (field : Field) message =
    Optic.set (Optic.fieldString_ name) field message

  [<CompiledName "SetFieldValues">]
  let setFieldValues (fields : (string * Field) seq) message =
    fields |> Seq.fold (fun m (name, value) -> setFieldValue name value m) message

  [<CompiledName "SetFieldValuesArray">]
  let setFieldValuesArray (fields : (string * Field)[]) message =
    fields |> Array.fold (fun m (name, value) -> setFieldValue name value m) message

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromMap (m : Map<string, obj>) message =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> k, Field (Value.create v, None))
    |> flip setFieldValues message

  [<CompiledName "SetFieldFromObject">]
  let setFieldFromObject name (data : obj) message =
    setFieldValue name (Field (Value.create data, None)) message

  /// Reflects over the object and sets the appropriate fields.
  [<CompiledName "SetFieldsFromObject">]
  let setFieldsFromObject (data : obj) message =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> k, Field (Value.create v, None))
    |> flip setFieldValues message

  /// Get a partial getter lens to a field
  [<CompiledName "TryGetField">]
  let tryGetField name message =
    Optic.get (Optic.fieldString_ name) message

  /// Tag the message
  [<CompiledName "Tag">]
  let tag (tag : string) (message : Message) =
    let key = KnownLiterals.TagsContextName
    match message.context |> Map.tryFind key with
    | Some (Array tags) when List.contains (String tag) tags ->
      message
    | Some (Array tags) ->
      let tags' = Array (String tag :: tags)
      { message with context = message.context |> Map.add key tags' }
    | Some _ ->
      message
    | None ->
      { message with context = message.context |> Map.add key (Array [ String tag ] ) }

  /// Check if the Message has a tag
  [<CompiledName "HasTag">]
  let hasTag (tag : string) (message : Message) =
    match message.context |> Map.tryFind KnownLiterals.TagsContextName with
    | Some (Array tags) when List.contains (String tag) tags -> true
    | _ -> false

  ///////////////// CONTEXT ////////////////////

  /// Sets a context value by trying to find the ToValue method on the type
  /// passed.
  [<CompiledName "SetContext">]
  let setContext name value message =
    Optic.set (Optic.contextValue_ name) (Value.create value) message

  /// Sets a context value.
  [<CompiledName "SetContextValue">]
  let setContextValue name value message =
    Optic.set (Optic.contextValue_ name) value message

  [<CompiledName "SetContextValues">]
  let setContextValues (values : (string * Value) seq) message =
    values |> Seq.fold (fun m (name, value) -> setContextValue name value m) message

  [<CompiledName "SetContextFromMap">]
  let setContextFromMap (m : Map<string, obj>) message =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.create v)
    |> List.ofSeq
    |> fun fields -> setContextValues fields message

  /// Uses reflection to set all
  [<CompiledName "SetContextFromObject">]
  let setContextFromObject (data : obj) message =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.create v)
    |> List.ofSeq
    |> fun values -> setContextValues values message

  /// Tries to get a context value
  [<CompiledName "TryGetContext">]
  let inline tryGetContext name message =
    Optic.get (Optic.contextValue_ name) message

  ///////////////// CTORS ////////////////////

  /// Creates a new event message template with level
  [<CompiledName "Event">]
  let event level template =
    { name      = PointName.empty
      value     = Event template
      fields    = Map.empty
      context   = Map.empty
      level     = level
      timestamp = Date.timestamp() }

  /// Creates a new event message template with level. Compared to `event`,
  /// this function has its parameters' order flipped.
  [<CompiledName "Event">]
  let eventX template level =
    event level template

  /// Creates a new gauge message with data point name, unit and value
  [<CompiledName "Gauge">]
  let gaugeWithUnit dp units value =
    { name      = dp
      value     = Gauge (value, units)
      fields    = Map.empty
      context   = Map.empty
      level     = LogLevel.Debug
      timestamp = Date.timestamp() }

  /// Creates a new gauge message with data point name and scalar value
  [<CompiledName "Gauge">]
  let gauge dp value =
    { name      = dp
      value     = Gauge (value, Units.Scalar)
      fields    = Map.empty
      context   = Map.empty
      level     = LogLevel.Debug
      timestamp = Date.timestamp() }

  [<CompiledName "Derived">]
  let derivedWithUnit dp units value =
    { name      = dp
      value     = Derived (value, units)
      fields    = Map.empty
      context   = Map.empty
      level     = LogLevel.Debug
      timestamp = Date.timestamp() }

  [<CompiledName "Derived">]
  let derived dp value =
    { name      = dp
      value     = Derived (value, Units.Scalar)
      fields    = Map.empty
      context   = Map.empty
      level     = LogLevel.Debug
      timestamp = Date.timestamp() }

  /// Create a verbose event message
  [<CompiledName "EventVerbose">]
  let eventVerbose = event Verbose

  /// Create a verbose event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventVerboseFormat">]
  let eventVerbosef fmt = Printf.kprintf (event Verbose) fmt

  /// Create a debug event message
  [<CompiledName "EventDebug">]
  let eventDebug = event LogLevel.Debug

  /// Create a debug event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventDebugFormat">]
  let eventDebugf fmt = Printf.kprintf (event LogLevel.Debug) fmt

  /// Create an info event message
  [<CompiledName "EventInfo">]
  let eventInfo = event Info

  /// Create a info event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventInfoFormat">]
  let eventInfof fmt = Printf.kprintf (event Info) fmt

  /// Create an warn event message
  [<CompiledName "EventWarn">]
  let eventWarn = event LogLevel.Warn

  /// Create a warn event message for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventWarnFormat">]
  let eventWarnf fmt = Printf.kprintf (event Warn) fmt

  /// Create an error event message
  [<CompiledName "EventError">]
  let eventError = event LogLevel.Error

  /// Write a error event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventErrorFormat">]
  let eventErrorf fmt = Printf.kprintf (event LogLevel.Error) fmt

  /// Create a fatal event message
  [<CompiledName "EventFatal">]
  let eventFatal = event Fatal

  /// Create a fatal event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventFatalFormat">]
  let eventFatalf fmt = Printf.kprintf (event Fatal) fmt

  open Logary.Internals.FsMessageTemplates

  /// A destructuring strategy for FsMessageTemplates which simply treats
  /// everything as a 'Scalar' object which can later be handled by Logary
  /// as `Field (Value.create o, None)`
  let internal destructureAllAsScalar : Destructurer =
    fun request -> TemplatePropertyValue.ScalarValue request.Value

  let internal captureNamesAndValuesAsScalars (t: Template) (args: obj[]) =
    Capturing.capturePropertiesWith ignore destructureAllAsScalar 1 t args

  let internal convertToNameAndField (pnv : PropertyNameAndValue) : string * Field =
    match pnv.Value with
    | ScalarValue v ->
      pnv.Name, Field (Value.create v, None)
    | _ ->
      failwith "In Logary we extract all properties as Scalar values. File a bug report with the parameter values that you called the function with."

  let internal extractFields formatTemplate args =
    let parsedTemplate = Parser.parse formatTemplate
    captureNamesAndValuesAsScalars parsedTemplate args
    |> Array.map convertToNameAndField

  /// Creates a new event with given level, format and arguments. Format may
  /// contain String.Format-esque format placeholders.
  [<CompiledName "EventFormat">]
  let eventFormat (level, formatTemplate, [<ParamArray>] args : obj[]) : Message =
    let fields = extractFields formatTemplate args
    event level formatTemplate |> setFieldValuesArray fields

  /// Converts a String.Format-style format string and an array of arguments into
  /// a message template and a set of fields.
  [<CompiledName "EventFormat">]
  let templateFormat (format : string, [<ParamArray>] args : obj[]) =
    eventFormat (LogLevel.Debug, format, args)

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds.
  [<CompiledName "Time">]
  let time pointName (f : 'input -> 'res) : 'input -> 'res * Message =
    fun input ->
      let sw = Stopwatch.StartNew()
      let res = f input
      sw.Stop()

      let value, units = sw.toGauge()
      let message = gaugeWithUnit pointName units value

      res, message

  [<CompiledName "TimeAsync">]
  let timeAsync pointName (fn : 'input -> Async<'res>) : 'input -> Async<'res * Message> =
    fun input ->
      async {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        sw.Stop()

        let value, units = sw.toGauge()
        return res, gaugeWithUnit pointName units value
      }


  [<CompiledName "TimeJob">]
  let timeJob pointName (fn : 'input -> Job<'res>) : 'input -> Job<'res * Message> =
    fun input ->
      job {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        let value, units =
          sw.Stop()
          sw.toGauge()
        return res, gaugeWithUnit pointName units value
      }

  [<CompiledName "TimeAlt">]
  let timeAlt pointName (fn : 'input -> Alt<'res>) : 'input -> Alt<'res * Message> =
    fun input ->
    Alt.prepareFun (fun () ->
      let sw = Stopwatch.StartNew()
      fn input |> Alt.afterFun (fun res ->
      sw.Stop()

      let value, units = sw.toGauge()

      res, gaugeWithUnit pointName units value
    ))

  [<CompiledName "TimeTask">]
  let timeTask pointName (fn : 'input -> Task<'res>) : 'input -> Task<'res * Message> =
    fun input ->
      let sw = Stopwatch.StartNew()
      // http://stackoverflow.com/questions/21520869/proper-way-of-handling-exception-in-task-continuewith
      (fn input).ContinueWith((fun (task : Task<'res>) ->
        sw.Stop()
        let value, units = sw.toGauge()
        task.Result, // will rethrow if needed
        gaugeWithUnit pointName units value
      ), TaskContinuationOptions.ExecuteSynchronously) // stopping SW is quick

  ///////////////// PROPS ////////////////////

  /// Sets the name of the message to a PointName
  [<CompiledName "SetName">]
  let setName name (msg : Message) =
    { msg with name = name }

  /// Sets the name of the message from a string.
  [<CompiledName "SetName">]
  let setSimpleName name (msg : Message) =
    { msg with name = PointName.parse name }

  /// Sets the last bit of the Message name value to the given `nameEnding`.
  /// This is useful when you have functions calling a static logger, but you
  /// want to make the Message say what function it was created from.
  /// Note: lastBitName MAY BE NULL!
  [<CompiledName "SetNameEnding">]
  let setNameEnding (nameEnding : string) : Message -> Message = function
    | { name = pn } as m
      when not (nameEnding = null)
        && not (String.isEmpty nameEnding) ->
      { m with name = pn |> PointName.setEnding nameEnding }
    | m ->
      m

  /// Sets the message's level.
  [<CompiledName "SetLevel">]
  let setLevel lvl msg = { msg with level = lvl}

  /// Sets the number of nanoseconds since epoch.
  [<CompiledName "SetNanoEpoch">]
  let setNanoEpoch (ts : EpochNanoSeconds) msg =
    { msg with timestamp = ts }

  [<CompiledName "SetTimestamp">]
  let setTimestamp (instant : Instant) msg =
    { msg with timestamp = instant.Ticks * Constants.NanosPerTick }

  /// Sets the number of ticks since epoch. There are 10 ticks per micro-second,
  /// so a tick is a 1/10th microsecond, so it's 100 nanoseconds long.
  [<CompiledName "SetTicksEpoch">]
  let setTicksEpoch (ticks : int64) msg =
    { msg with timestamp = ticks * Constants.NanosPerTick }

  /// Sets the number of ticks as specified by DateTime and DateTimeOffset,
  /// which starts at zero the 0001-01-01 00:00:00 instant.
  [<CompiledName "SetUTCTicks">]
  let setUTCTicks (ticks : int64) msg =
    setTicksEpoch (ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks) msg

  /// Update the message with the current timestamp.
  [<CompiledName "UpdateTimestamp">]
  let updateTimestamp message =
    { message with timestamp = Date.timestamp () }

  /// Replaces the value of the message with a new Event with the supplied format
  [<CompiledName "SetEvent">]
  let setEvent format message =
    { message with value = Event format }

  [<CompiledName "SetGauge">]
  let setGauge (value, units) message =
    { message with value = Gauge (value, units) }

  [<CompiledName "SetDerived">]
  let setDerived (value, units) message =
    { message with value = Derived (value, units) }

  /// Adds a new exception to the "errors" field in the message.
  /// AggregateExceptions are automatically expanded.
  [<CompiledName "AddException">]
  let addExn (e : exn) msg =
    let flattenedExns =
      match e with
      | :? AggregateException as ae ->
        ae.InnerExceptions |> Seq.map toValue |> Seq.toList
      | _ ->
        toValue e :: []

    let exnsNext =
      let exns = Optic.get Optic.errors_ [] msg
      exns @ flattenedExns

    // If there's no "errors" field, add it
    let msg =
      match Lens.getPartial Optic.errors_ msg with
      | Some x ->
        msg

      | None ->
        setFieldValue KnownLiterals.ErrorsFieldName (Field (Array [], None)) msg

    Optic.set Optic.errors_ exnsNext msg
