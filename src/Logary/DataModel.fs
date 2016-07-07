namespace Logary

open System
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.Reflection
open Logary
open Logary.Utils.Chiron
open Logary.Utils.Chiron.Operators
open Logary.Utils.Aether
open Logary.Utils.Aether.Operators

[<AutoOpen>]
module Json =
  let inline maybeWrite key value =
    match value with
    | Some v -> Json.write key v
    | None -> fun json -> Value (), json
  let (|Val|_|) = Map.tryFind
  let (|Only|_|) name m =
    match m with
    | Val name value when m |> Seq.length = 1 ->
      Some value
        | _ -> None

type ContentType = string

type EpochNanoSeconds = int64

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

  (* Isomorphisms *)

  static member String__ : PIso<Value, string> =
    (function | String x -> Some x
              | _ -> None), String

  static member Bool__ : PIso<Value, bool> =
    (function | Bool x -> Some x
              | _ -> None), Bool

  static member Float__ : PIso<Value, float> =
    (function | Float x -> Some x
              | _ -> None), Float

  static member Int64__ : PIso<Value, int64> =
    (function | Int64 x -> Some x
              | _ -> None), Int64

  static member BigInt__ : PIso<Value, bigint> =
    (function | BigInt x -> Some x
              | _ -> None), BigInt

  static member Binary__ : PIso<Value, byte [] * ContentType> =
    (function | Binary (bs, ct) -> Some (bs, ct)
              | _ -> None), Binary

  static member Fraction__ : PIso<Value, int64 * int64> =
    (function | Fraction (n, d) -> Some (n, d)
              | _ -> None), Fraction

  static member Array__ : PIso<Value, Value list> =
    (function | Array x -> Some x
              | _ -> None), Array

  static member Object__ : PIso<Value, Map<string, Value>> =
    (function | Object x -> Some x
              | _ -> None), Object

  (* Lenses *)

  static member String_ : PLens<Value, string> =
    id_ <-?> Value.String__

  static member Bool_ : PLens<Value, bool> =
      id_ <-?> Value.Bool__

  static member Float_ : PLens<Value, float> =
    id_ <-?> Value.Float__

  static member Int64_ : PLens<Value, int64> =
    id_ <-?> Value.Int64__

  static member BigInt_ : PLens<Value, bigint> =
    id_ <-?> Value.BigInt__

  static member Binary_ : PLens<Value, byte[] * ContentType> =
    id_ <-?> Value.Binary__

  static member Fraction_ : PLens<Value, int64 * int64> =
    id_ <-?> Value.Fraction__

  static member Array_ : PLens<Value, Value list> =
    id_ <-?> Value.Array__

  static member Object_ : PLens<Value, Map<string, Value>> =
    id_ <-?> Value.Object__

  static member ToJson (v : Value) =
    match v with
    | String s ->
      Json.Lens.setPartial Json.String_ s

    | Bool b ->
      Json.Lens.setPartial Json.Bool_ b

    | Float f ->
      Json.Lens.setPartial Json.Number_ (decimal f)

    | Int64 i ->
      Json.write "Int64" (decimal i)

    | BigInt bi ->
      Json.write "BigInt" (decimal bi)

    | Binary (bs, contentType) ->
      Json.write "mime" contentType
      *> Json.write "data" ("base64:" + Convert.ToBase64String bs)

    | Fraction (n, d) ->
      Json.write "fraction" (Json.Array [Json.Number (decimal n); Json.Number (decimal d)])

    | Array values ->
      Json.Lens.setPartial Json.Array_ (values |> List.map Json.serialize)

    | Object o ->
      Json.Lens.setPartial Json.Object_ (o |> Map.map (fun k v -> Json.serialize v))

  static member FromJson (_ : Value) =
    fun json ->
      match json with
      | Json.String str ->
        JsonResult.Value (String str), json

      | Json.Bool b ->
        JsonResult.Value (Bool b), json

      | Json.Number f ->
        JsonResult.Value (Float (float f)), json

      | Json.Array arr ->
        match arr |> List.traverseChoiceA Json.tryDeserialize with
        | Choice1Of2 values ->
          JsonResult.Value (Array values), json

        | Choice2Of2 err ->
          JsonResult.Error err, json

      | Json.Object o ->

        match o with
        | Only "Int64" (Json.Number i) -> JsonResult.Value (Int64 (int64 i)), json
        | Only "BigInt" (Json.Number i) -> JsonResult.Value (BigInt (bigint i)), json
        | Val "mime" (Json.String mimeType) & Val "data" (Json.String data)
            when data.StartsWith "base64:" ->
          let bytes = Convert.FromBase64String (data.Substring(7))
          JsonResult.Value (Binary (bytes, mimeType)), json
        | Only "fraction" (Json.Array [Json.Number n; Json.Number d]) ->
          JsonResult.Value (Fraction (int64 n, int64 d)), json
        | _ ->
          (o |> Map.map (fun k v -> Json.deserialize v) |> Object) |> JsonResult.Value, json

      | Json.Null () ->
        JsonResult.Error "Cannot handle Null json values", json


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Value =
  open Logary.Internals
  open System.Collections.Generic

  [<CompiledName "OfObject">]
  let rec ofObject : obj -> Value = function
    
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

    // Collections
    | :? (byte array) as bytes ->
      Binary (bytes, "application/octet-stream")

    | :? Array as arr ->
      [ for i in 0..arr.Length-1 do
          let v = arr.GetValue i
          if v <> null then yield ofObject v ]
      |> Array

    | :? IEnumerable<KeyValuePair<string, obj>> as dict ->
      dict
      |> Seq.choose (function
        | KeyValue (k, v) when v <> null ->
          Some (k, ofObject v)

        | otherwise ->
          None)

      |> Map.ofSeq
      |> Object

    //| :? Map<string, obj> as map -> Map.map (fun _ v -> fromObject v) map |> Object
    | :? IEnumerable<obj> as ie ->
      ie
      |> Seq.filter ((<>) null)
      |> Seq.map ofObject
      |> Seq.toList
      |> Array

    | du when FSharpType.IsUnion (du.GetType()) ->
      let uci, _ = FSharpValue.GetUnionFields(du, du.GetType())
      String uci.Name

    // POCOs
    | a when a <> null ->
      a
      |> Map.ofObject
      |> Map.map (fun _ v -> ofObject v)
      |> Object

    | otherwise ->
      failwithf "Cannot convert %A to a Value" otherwise

[<AutoOpen>]
module Capture =

  // TODO: this structure is both a Reader and Writer monad, but only needs
  // to be a writer monad
  type Value<'a> =
    Value -> ValueResult<'a> * Value

  and ValueResult<'a> =
    | ValueResult of 'a
    | ValueError of string

  [<RequireQualifiedAccess>]
  module Value =

    let inline init (a: 'a) : Value<'a> =
      fun value ->
        ValueResult a, value

    let inline error (e: string) : Value<'a> =
      fun value ->
        ValueError e, value

    let inline internal ofResult result =
      fun value ->
        result, value

    let inline bind (m: Value<'a>) (f: 'a -> Value<'b>) : Value<'b> =
      fun json ->
        match m json with
        | ValueResult a, json -> (f a) json
        | ValueError e, json -> ValueError e, json

    let inline apply (f: Value<'a -> 'b>) (m: Value<'a>) : Value<'b> =
      bind f (fun f' ->
        bind m (fun m' ->
          init (f' m')))

    let inline map (f: 'a -> 'b) (m: Value<'a>) : Value<'b> =
      bind m (fun m' ->
        init (f m'))

    let inline map2 (f: 'a -> 'b -> 'c) (m1: Value<'a>) (m2: Value<'b>) : Value<'c> =
      apply (apply (init f) m1) m2

(* Operators

   Symbolic operators for working with Value<'a> functions, providing
   an operator based concise alternative to the primitive Json<'a> combinators
   given as part of Functional.

   This module is not opened by default, as symbolic operators are a matter
   of taste and may also clash with other operators from other libraries. *)

module Operators =

  let inline (>>=) m f =
    Value.bind m f

  let inline (=<<) f m =
    Value.bind m f

  let inline (<*>) f m =
    Value.apply f m

  let inline (<!>) f m =
    Value.map f m

  let inline (>>.) m f =
    Value.bind m (fun _ -> f)

  let inline (.>>) m f =
    Value.bind (fun _ -> m) f

  let inline ( *>) m1 m2 =
    Value.map2 (fun _ x -> x) m1 m2

  let inline ( <*) m1 m2 =
    Value.map2 (fun x _ -> x) m1 m2

  let inline (>=>) m1 m2 =
    Value.bind (fun x -> m1 x) m2

  let inline (<=<) m1 m2 =
    Value.bind (fun x -> m2 x) m1

(* Lens

   Functional lens based access to nested Json data strcutures,
   using Aether format lenses. Uses Value<'a> based functions, so
   can be used monadicly. *)

[<AutoOpen>]
module Lens =

  (* Functions *)

  [<RequireQualifiedAccess>]
  module Value =

    let getLens l : Value<_> =
      fun value ->
        ValueResult (Lens.get l value), value

    let getLensPartial l : Value<_> =
      fun value ->
        match Lens.getPartial l value with
        | Some x -> ValueResult x, value
        | _ -> ValueError (sprintf "couldn't use lens %A on value '%A'" l value), value

    let tryGetLensPartial l : Value<_> =
      fun value ->
        ValueResult (Lens.getPartial l value), value

    let setLens l v : Value<_> =
      fun value ->
        ValueResult (), Lens.set l v value

    let setLensPartial l v : Value<_> =
      fun value ->
        ValueResult (), Lens.setPartial l v value

    let mapLens l f : Value<_> =
      fun value ->
        ValueResult (), Lens.map l f value

    let mapLensPartial l f : Value<_> =
      fun value ->
        ValueResult (), Lens.mapPartial l f value

[<AutoOpen>]
module Mapping =

  open Operators

  (* To

      *)

  (* Defaults *)

  type ToValueDefaults = ToValueDefaults with

    (* Basic Types *)

    static member inline ToValue (x: bool) =
      Value.setLensPartial Value.Bool_ x

    static member inline ToValue (x: decimal) =
      Value.setLensPartial Value.Float_ (float x)

    static member inline ToValue (x: float) =
      Value.setLensPartial Value.Float_ x

    static member inline ToValue (x: int) =
      Value.setLensPartial Value.Float_ (float x)

    static member inline ToValue (x: int16) =
      Value.setLensPartial Value.Int64_ (int64 x)

    static member inline ToValue (x: int64) =
      Value.setLensPartial Value.Int64_ x

    static member inline ToValue (x: single) =
      Value.setLensPartial Value.Float_ (float x)

    static member inline ToValue (x: string) =
      Value.setLensPartial Value.String_ x

    static member inline ToValue (x: uint16) =
      Value.setLensPartial Value.Int64_ (int64 x)

    static member inline ToValue (x: uint32) =
      Value.setLensPartial Value.Int64_ (int64 x)

    static member inline ToValue (x: uint64) =
      Value.setLensPartial Value.Float_ (float x)

    (* Common Types *)

    static member inline ToValue (x: DateTime) =
      Value.setLensPartial Value.String_ (x.ToUniversalTime().ToString("o"))

    static member inline ToValue (x: DateTimeOffset) =
      Value.setLensPartial Value.String_ (x.ToString("o"))

    static member inline ToValue (x: Guid) : Value<unit> =
      Value.setLensPartial Value.String_ (string x)

    static member inline ToValue (x: Uri) =
      Value.setLensPartial Value.String_ (x.ToString())

    static member inline ToValue (e: exn) : Value<unit> =
      let rec serialise (e : exn) =
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
              yield "data", Value.ofObject e.Data ]

        Map.ofSeq <|
          if e.InnerException <> null then
            ("inner", Object <| serialise e.InnerException) :: fields
          else
            fields

      Value.setLensPartial Value.Object_ (serialise e)

    (* Json Type *)

    static member inline ToValue (x: Value) =
      Value.setLens id_ x

  (* Mapping Functions

     Functions for applying the ToJson function to data structures to produce
     new Json instances. *)

  let inline internal toValueDefaults (a: ^a, _: ^b) =
    ((^a or ^b) : (static member ToValue: ^a -> unit Value) a)

  let inline internal toValue (x: 'a) =
    snd (toValueDefaults (x, ToValueDefaults) (Object (Map.empty)))

  (* Defaults *)

  type ToValueDefaults with

    (* Arrays *)

    static member inline ToValue (x: 'a array) =
      Value.setLens id_ (Array ((Array.toList >> List.map toValue) x))

    (* Lists *)

    static member inline ToValue (x: 'a list) =
      Value.setLens id_ (Array (List.map toValue x))

    (* Maps *)

    static member inline ToValue (x: Map<string,'a>) =
      Value.setLens id_ (Object (Map.map (fun _ a -> toValue a) x))

    (* Options *)

    static member inline ToValue (x: 'a option) =
      match x with | None -> Value.init ()
                   | Some a -> Value.setLens id_ (toValue a)

    (* Sets *)

    static member inline ToValue (x: Set<'a>) =
      Value.setLens id_ (Array ((Set.toList >> List.map toValue) x))

    (* Tuples *)

    static member inline ToValue ((a, b)) =
      Value.setLens id_ (Array [ toValue a; toValue b ])

    static member inline ToValue ((a, b, c)) =
      Value.setLens id_ (Array [ toValue a; toValue b; toValue c ])

  [<RequireQualifiedAccess>]
  module Value =

    let inline write key value =
      Value.setLensPartial (Value.Object_ >??> key_ key) (toValue value)

    let inline serialize (a : ^a) : Value =
      toValue a

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
  | Mul of Units * Units
  | Pow of Units * Units
  | Div of Units * Units
  | Root of Units
  | Log10 of Units // Log of base:float * BaseUnit

  static member symbol = function
    | Bits -> "b"
    | Bytes -> "B"
    | Seconds -> "s"
    | Metres -> "m"
    | Scalar -> ""
    | Amperes -> "A"
    | Kelvins -> "K"
    | Moles -> "mol"
    | Candelas -> "cd"
    | Mul (a, b) -> String.Concat [ "("; Units.symbol a; "*"; Units.symbol b; ")" ]
    | Pow (a, b) -> String.Concat [ Units.symbol a; "^("; Units.symbol b; ")" ]
    | Div (a, b) -> String.Concat [ "("; Units.symbol a; "/"; Units.symbol b; ")" ]
    | Root a -> String.Concat [ "sqrt("; Units.symbol a; ")" ]
    | Log10 a -> String.Concat [ "log10("; Units.symbol a; ")" ]

  static member ToJson (u : Units) =
    match u with
    | Bits
    | Bytes
    | Seconds
    | Metres
    | Scalar
    | Amperes
    | Kelvins
    | Moles
    | Candelas ->
      Json.Lens.setPartial Json.String_ (u |> Units.symbol)
    | Mul (a, b) ->
      Json.write "multipleA" a
      *> Json.write "multipleB" b
    | Pow (a, b) ->
      Json.write "base" a
      *> Json.write "exponent" b
    | Div (a, b) ->
      Json.write "dividend" a
      *> Json.write "divider" b
    | Root a ->
      Json.write "root" a
    | Log10 a ->
      Json.write "log10" a

  static member FromJson (_ : Units) =
    fun json ->
      match json with
      | Json.String s ->

        match s with
        | "b" -> Bits |> JsonResult.Value, json
        | "B" -> Bytes |> JsonResult.Value, json
        | "s" -> Seconds |> JsonResult.Value, json
        | "m" -> Metres |> JsonResult.Value, json
        | "" -> Scalar |> JsonResult.Value, json
        | "A" -> Amperes |> JsonResult.Value, json
        | "K" -> Kelvins |> JsonResult.Value, json
        | "mol" -> Moles |> JsonResult.Value, json
        | "cd" -> Candelas |> JsonResult.Value, json
        | _ -> JsonResult.Error "Unknown unit type represented as string", json
      | Json.Object o ->
        match o with
        | Val "multipleA" a & Val "multipleB" b ->
          Mul(Json.deserialize a, Json.deserialize b) |> JsonResult.Value, json
        | Val "base" b & Val "exponent" e ->
          Pow(Json.deserialize b, Json.deserialize e) |> JsonResult.Value, json
        | Val "dividend" a & Val "divider" b ->
          Div(Json.deserialize a, Json.deserialize b) |> JsonResult.Value, json
        | Val "root" a ->
          Root (Json.deserialize a) |> JsonResult.Value, json
        | Val "log10" a ->
          Log10 (Json.deserialize a) |> JsonResult.Value, json
        | _ -> JsonResult.Error "Unknown unit type represented as object", json
      | _ -> JsonResult.Error "Unknown unit", json


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

  let scaleSeconds = int64 >> ((*) 1000000000000L) >> function
    | value when value < 1000L -> 1L
    | value when value < 1000000L -> 1000L
    | value when value < 1000000000L -> 1000000L
    | value when value < 1000000000000L -> 1000000000L
    | value when value < 60L * 1000000000000L -> 1000000000000L
    | value when value < 3600L * 1000000000000L -> 3600L * 1000000000000L
    | value -> 86400L * 1000000000000L

  // TODO: re-enable scaling
  (*
  // grafana/public/app/kbn.js:374@g20d5d0e
  let doScale (fFactor : decimal -> int64) (scaledUnits : string list) =
    fun (decimals : uint16) (scaledDecimals : uint16) (value : decimal) ->
      (value : decimal), "KiB"

  let scale units : uint16 -> uint16 -> decimal -> float*string =
    match units with
    | Bits -> (fun _ -> 1000L), ["b"; "Kib"; "Mib"; "Gib"; "Tib"; "Pib"; "Eib"; "Zib"; "Yib"]
    | Bytes -> (fun _ -> 1024L), ["B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]
    | Seconds -> scaleSeconds, ["ns"; "µs"; "ms"; "s"; "min"; "h"; "days"]
    ||> doScale
   *)

  let formatWithUnit orient un value =
    match orient with
    | Prefix ->
      sprintf "%s %s" (Units.symbol un) (formatValue value)
    | Suffix ->
      sprintf "%s %s" (formatValue value) (Units.symbol un)

type PointName =
  PointName of hierarchy:string[]
with
  override x.ToString() =
    let (PointName hiera) = x in String.concat "." hiera

  static member hierarchy_ : Lens<PointName, string[]> =
    (fun (PointName h) -> h),
    fun v x -> PointName v

  static member FromJson(_ : PointName) : Json<PointName> =
    fun json ->
      Json.tryDeserialize json
      |> function
      | Choice1Of2 xs -> Json.init (PointName xs) json
      | Choice2Of2 err -> Json.error err json

  static member ToJson (PointName xs) : Json<unit> =
    Json.Lens.setPartial Json.Array_ (xs |> Array.map Json.String |> List.ofArray)

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

module Chiron =
  let inline internal (|PropertyWith|) (fromJson : Json< ^a>) key =
       Lens.getPartial (Json.Object_ >??> key_ key)
    >> Option.bind (fromJson >> function | Value a, _ -> Some (a : 'a)
                                         | _, _ -> None)

type PointValue =
  /// Value at point in time
  | Gauge of Value * Units
  /// Any sort of derived measure
  | Derived of Value * Units
  /// All simple-valued fields' values can be templated into the template string
  /// when outputting the value in the target.
  | Event of template:string
with
  static member private valueUnitsToJson (value : Value, units : Units) : Json<unit> =
    Json.write "value" value
    *> Json.write "units" units

  static member private valueUnitsFromJson : Json<Value * Units> =
    (fun value units -> value, units)
    <!> Json.read "value"
    <*> Json.read "units"

  static member ToJson (pv : PointValue) : Json<unit> =
    let inJsonObject writer =
      writer (Json.Object Map.empty) |> snd

    match pv with
    | Gauge (value, units) ->
      Json.writeWith (PointValue.valueUnitsToJson >> inJsonObject) "gauge" (value, units)

    | Derived (value, units) ->
      Json.writeWith (PointValue.valueUnitsToJson >> inJsonObject) "derived" (value, units)

    | Event template ->
      Json.write "event" template

  static member FromJson (_ : PointValue) : Json<PointValue> =
    fun json ->
      match json with
      // TODO: why does the compiler require Some (..) here??
      | Chiron.PropertyWith PointValue.valueUnitsFromJson "gauge" (Some (value, units)) ->
        JsonResult.Value (Gauge (value, units)), json

      // TODO: why does the compiler require Some (..) here??
      | Chiron.PropertyWith PointValue.valueUnitsFromJson "derived" (Some (value, units)) ->
        JsonResult.Value (Derived (value, units)), json

      // TODO: as opposed to here...
      | Property "event" event ->
        JsonResult.Value (Event event), json

      | json ->
        Json.error (sprintf "Cannot convert JSON %A to PointValue" json) json

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
with
  static member value_ : Lens<Field, Value> =
    (fun (Field (value, mUnits)) -> value),
    fun v (Field (value, mUnits)) -> Field (v, mUnits)

  static member units_ : PLens<Field, Units> =
    (fun (Field (_, mUnits)) -> mUnits),
    fun units (Field (value, _)) -> Field (value, Some units)

  static member ToJson (Field (value, maybeUnit)) : Json<unit> =
    Json.write "value" value
    *> Json.maybeWrite "units" maybeUnit

  static member FromJson (_ : Field) : Json<Field> =
    // not all fields have units; be flexible in format
    // for those that don't to make it easier for javascript
    // users
    fun json ->
      match json with
      | Json.Object o ->
        match o with
        | Only "value" value ->
          Field (Json.deserialize value, None)
          |> JsonResult.Value, json
        | Val "value" value & Val "units" units ->
          Field (Json.deserialize value, Some (Json.deserialize units))
          |> JsonResult.Value, json
        | _ ->
          Field (Json.deserialize json, None)
          |> JsonResult.Value, json
      | _ ->
        Field (Json.deserialize json, None)
        |> JsonResult.Value, json

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // remove when field moved outside
module Field =

  let inline initWithUnit value units =
    Field (Value.serialize value, Some units)

  let inline init (value : ^a) =
    Field (Value.serialize value, None)

  /// Converts a String.Format-style format string and an array of arguments into
  /// a message template and a set of fields.
  let templateFromFormat (format : string) (args : obj[]) =
    let fields =
      args
      |> Array.mapi (fun i v ->
        sprintf "arg%i" i,
        Field (Value.ofObject v, None))
      |> List.ofArray

    // Replace {0}..{n} with {arg0}..{argn}
    let template = Seq.fold (fun acc i -> String.replace (sprintf "{%i}" i) (sprintf "{arg%i}" i) acc) format [0..args.Length]
    (template, fields)


/// This is the main value type of Logary. It spans both logging and metrics.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse template in
    /// (Event template) = message.value
    ///
    name      : PointName
    /// The main value for this metric or event. Either a Gauge, a Derived or an
    /// Event. (A discriminated union type)
    value     : PointValue
    /// The semantic-logging data. This corresponds to the 'data' field in other
    /// logging abstractions.
    fields    : Map<PointName, Field>
    /// Where in the code? Who did the operation? What tenant did the principal
    /// who did it belong to? Put things your normally do 'GROUP BY' on in this
    /// Map.
    context   : Map<string, Value>
    /// What urgency? For events you're interested in tracking in some metrics
    /// interface, like Grafana or logs you normally expose in your dashboards
    /// in Kibana (like 'Event "User logged in'), you want LogLevel.Info. Debug
    /// is what you put in so that you can see what goes awry when things do go
    /// awry, and finally you can use Verbose on things that run every single
    /// request; for when you really need to do 'sanity checking' in production.
    level     : LogLevel
    /// When? nanoseconds since UNIX epoch.
    timestamp : EpochNanoSeconds }

    /// Gets the timestamp as NodaTime ticks (100 ns per tick). If you're getting
    /// for DateTime and/or DateTimeOffset, remember that those start at
    /// 0001-01-01.
    member x.timestampTicks : int64 =
      x.timestamp / 100L

    static member ToJson (m : Message) =
      let fields' =
        m.fields
        |> Seq.map (function KeyValue(key, value) -> PointName.format key, value)
        |> Map.ofSeq

      Json.write "name" m.name
      *> Json.write "value" m.value
      *> Json.write "fields" fields'
      *> Json.write "context" m.context
      *> Json.write "level" m.level
      *> Json.write "timestamp" m.timestamp

    static member FromJson (_ : Message) =
      (fun name value (fields : Map<string, _>) context level ts ->
        let fields =
          fields
          |> Seq.map (fun kv -> PointName (kv.Key |> String.splita '.'), kv.Value)
          |> Map.ofSeq

        { name      = name
          value     = value
          fields    = fields
          context   = context
          level     = level
          timestamp = ts })
      <!> Json.read "name"
      <*> Json.read "value"
      <*> Json.read "fields"
      <*> Json.read "context"
      <*> Json.read "level"
      <*> Json.read "timestamp"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
  open NodaTime
  open System.Diagnostics
  open Logary.Internals

  [<Literal>]
  let ErrorsFieldName = "errors"

  [<Literal>]
  let ServiceFieldName = "service"

  module Lenses =

    let name_ : Lens<Message, PointName> =
      (fun x -> x.name),
      fun v x -> { x with name = v }

    let value_ : Lens<Message, PointValue> =
      (fun x -> x.value),
      fun v x -> { x with value = v }

    let fields_ : Lens<Message, Map<PointName, Field>> =
      (fun x -> x.fields),
      (fun v x -> { x with fields = v })

    let context_ : Lens<Message, Map<string, Value>> =
      (fun x -> x.context),
      (fun v x -> { x with context = v })

    let field_ name : PLens<Message, Field> =
      fields_ >-?> key_ (PointName.ofSingle name)

    let contextValue_ name : PLens<Message, Value> =
      context_ >-?> key_ name

    /// Lens to the context field 'service'
    let service_ : PLens<Message, string> =
      contextValue_ ServiceFieldName >??> Value.String_

    /// Lens you can use to get the list of errors in this message.
    let errors_ : PLens<Message, Value list> =
      field_ ErrorsFieldName >?-> Field.value_ >??> Value.Array_

  ///////////////// FIELDS ////////////////////

  /// Get a partial setter lens to a field
  [<CompiledName "SetField">]
  let inline setField name value =
    Lens.setPartial (Lenses.field_ name) (Field.init value)

  /// Get a partial setter lens to a field with an unit
  [<CompiledName "SetFieldUnit">]
  let inline setFieldUnit name value units =
    Lens.setPartial (Lenses.field_ name) (Field.initWithUnit value units)

  /// You can also choose to construct a Field yourself, using the object model
  /// that Logary has for its data. That way you don't have to rely on having
  /// static ToValue methods on your data objects.
  [<CompiledName "SetFieldValue">]
  let setFieldValue (name : string) (field : Field) msg =
    Lens.setPartial (Lenses.field_ name) field msg

  [<CompiledName "SetFieldValues">]
  let setFieldValues (fields : (string * Field) seq) msg =
    fields |> Seq.fold (fun m (name, value) -> setFieldValue name value m) msg

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromMap (m : Map<string, obj>) msg =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> k, Field (Value.ofObject v, None))
    |> List.ofSeq
    |> fun fields -> setFieldValues fields msg

  [<CompiledName "SetFieldFromObject">]
  let setFieldFromObject name (data : obj) msg =
    setFieldValue name (Field (Value.ofObject data, None)) msg

  /// Reflects over the object and sets the appropriate fields.
  [<CompiledName "SetFieldsFromObject">]
  let setFieldsFromObject (data : obj) msg =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> k, Field (Value.ofObject v, None))
    |> List.ofSeq
    |> fun fields -> setFieldValues fields msg

  /// Get a partial getter lens to a field
  [<CompiledName "TryGetField">]
  let tryGetField name =
    Lens.getPartial (Lenses.field_ name)

  ///////////////// CONTEXT ////////////////////

  /// Sets a context value by trying to find the ToValue method on the type
  /// passed.
  [<CompiledName "SetContext">]
  let inline setContext name value msg =
    Lens.setPartial (Lenses.contextValue_ name) (Value.serialize value) msg

  /// Sets a context value.
  [<CompiledName "SetContextValue">]
  let setContextValue name value msg =
    Lens.setPartial (Lenses.contextValue_ name) value msg

  [<CompiledName "SetContextValues">]
  let setContextValues (values : (string * Value) seq) msg =
    values |> Seq.fold (fun m (name, value) -> setContextValue name value m) msg

  [<CompiledName "SetContextFromMap">]
  let setContextFromMap (m : Map<string, obj>) msg =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.ofObject v)
    |> List.ofSeq
    |> fun fields -> setContextValues fields msg

  /// Uses reflection to set all
  [<CompiledName "SetContextFromObject">]
  let setContextFromObject (data : obj) msg =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.ofObject v)
    |> List.ofSeq
    |> fun values -> setContextValues values msg

  /// Tries to get a context value
  [<CompiledName "TryGetContext">]
  let inline tryGetContext name msg =
    Lens.getPartial (Lenses.contextValue_ name) msg

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

  /// Creates a new gauge message with data point name, unit and value
  [<CompiledName "Gauge">]
  let gaugeWithUnit dp units value =
    { name      = dp
      value     = Gauge (value, units)
      fields    = Map.empty
      context   = Map.empty
      level     = LogLevel.Debug
      timestamp = Date.timestamp() }

  [<Obsolete "Use gaugeWithUnit (C#: Gauge)"; CompiledName "Metric">]
  let metricWithUnit dp units value =
    gaugeWithUnit dp units value

  /// Creates a new gauge message with data point name and scalar value
  [<CompiledName "Gauge">]
  let gauge dp value =
    { name      = dp
      value     = Gauge (value, Units.Scalar)
      fields    = Map.empty
      context   = Map.empty
      level     = LogLevel.Debug
      timestamp = Date.timestamp() }

  [<Obsolete "Use gauge (C#: Gauge)"; CompiledName "Metric">]
  let metric dp value =
    gauge dp value

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
  [<CompiledName "EventVerboseFormat">]
  let eventVerbosef fmt = Printf.kprintf (event Verbose) fmt

  /// Create a debug event message
  [<CompiledName "EventDebug">]
  let eventDebug = event LogLevel.Debug

  /// Create a debug event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "EventDebugFormat">]
  let eventDebugf fmt = Printf.kprintf (event LogLevel.Debug) fmt

  /// Create an info event message
  [<CompiledName "EventInfo">]
  let eventInfo = event Info

  /// Create a info event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "EventInfoFormat">]
  let eventInfof fmt = Printf.kprintf (event LogLevel.Info) fmt

  /// Create an warn event message
  [<CompiledName "EventWarn">]
  let eventWarn = event LogLevel.Warn

  /// Create a warn event message for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "EventWarnFormat">]
  let eventWarnf fmt = Printf.kprintf (event LogLevel.Warn) fmt

  /// Create an error event message
  [<CompiledName "EventError">]
  let eventError = event LogLevel.Error

  /// Write a error event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "EventErrorFormat">]
  let eventErrorf fmt = Printf.kprintf (event LogLevel.Error) fmt

  /// Create a fatal event message
  [<CompiledName "EventFatal">]
  let eventFatal = event Fatal

  /// Create a fatal event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "EventFatalFormat">]
  let eventFatalf fmt = Printf.kprintf (event LogLevel.Fatal) fmt

  open Logary.Utils.FsMessageTemplates

  /// A destructuring strategy for FsMessageTemplates which simply treats
  /// everything as a 'Scalar' object which can later be handled by Logary
  /// as `Field (Value.ofObject o, None)`
  let internal destructureAllAsScalar : Destructurer =
    fun request -> TemplatePropertyValue.ScalarValue request.Value

  let internal captureNamesAndValuesAsScalars (t: Template) (args: obj[]) =
    Capturing.capturePropertiesWith ignore destructureAllAsScalar 1 t args

  let internal convertToNameAndField (pnv : PropertyNameAndValue) : string * Field =
    match pnv.Value with
    | ScalarValue v -> pnv.Name, Field (Value.ofObject v, None)
    | _ -> failwith "This should never happen. In Logary we extract all properties as Scalar"

  /// Converts a String.Format-style format string and an array of arguments into
  /// a message template and a set of fields.
  let templateFromFormat (format : string) (args : obj[]) =
    let parsedTemplate = Parser.parse format
    let scalarNamesAndValues = captureNamesAndValuesAsScalars parsedTemplate args
    let fields = scalarNamesAndValues |> Seq.map (convertToNameAndField) |> List.ofSeq
    (format, fields)

  /// Creates a new event with given level, format and arguments. Format may
  /// contain String.Format-esque format placeholders.
  [<CompiledName "EventFormat">]
  let eventFormat (level, format, [<ParamArray>] args : obj[]) =
    let (template, fields) = Field.templateFromFormat format args
    event level template |> setFieldValues fields

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds.
  [<CompiledName "Time";>]
  let time pointName f =
    let sw = Stopwatch.StartNew()
    let res = f ()
    sw.Stop()

    let value =
      Float (float sw.ElapsedTicks / float NodaConstants.TicksPerSecond)

    let message =
      gaugeWithUnit pointName Units.Seconds value

    res, message

  ///////////////// PROPS ////////////////////

  /// Sets the messages's name
  [<CompiledName "SetName">]
  let setName name (msg : Message) =
    { msg with name = name }

  /// Sets the message's level.
  [<CompiledName "SetLevel">]
  let setLevel lvl msg = { msg with level = lvl}

  /// Sets the number of nanoseconds since epoch.
  [<CompiledName "SetNanoEpoch">]
  let setNanoEpoch (ts : EpochNanoSeconds) msg =
    { msg with timestamp = ts }

  /// Sets the number of ticks since epoch. There are 10 ticks per micro-second,
  /// so a tick is a 1/10th microsecond, so it's 100 nanoseconds long.
  [<CompiledName "SetTicksEpoch">]
  let setTicksEpoch (ticks : int64) msg =
    { msg with timestamp = ticks * 100L }

  /// Sets the number of ticks as specified by DateTime and DateTimeOffset,
  /// which starts at zero the 0001-01-01 00:00:00 instant.
  [<CompiledName "SetUTCTicks">]
  let setUTCTicks (ticks : int64) msg =
    setTicksEpoch (ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks) msg

  /// Update the message with the current timestamp.
  [<CompiledName "UpdateTimestamp">]
  let updateTimestamp msg =
    { msg with timestamp = Date.timestamp () }

  /// Replaces the value of the message with a new Event with the supplied format
  [<CompiledName "SetEvent">]
  let setEvent format msg =
    { msg with value = Event format}

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
      let exns = Lens.getPartialOrElse Lenses.errors_ [] msg
      exns @ flattenedExns

    // If there's no "errors" field, add it
    let msg =
      match Lens.getPartial Lenses.errors_ msg with
      | Some x ->
        msg

      | None ->
        setFieldValue "errors" (Field (Array [], None)) msg

    Lens.setPartial Lenses.errors_ exnsNext msg
