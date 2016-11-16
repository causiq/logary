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

    | _ ->
      None

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
  
  let rec exceptionToStringValueMap (valueOfObject : obj->Value) (e : exn) =
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
    | :? TimeSpan as ts        -> String (ts.ToString())
    | :? NodaTime.Duration as d-> String (d.ToString())
    | :? exn as e              -> Object (exceptionToStringValueMap ofObject e)

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
      Value.setLensPartial Value.Object_ (Value.exceptionToStringValueMap Value.ofObject e)

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
    | Bits -> "b"
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
    | Candelas
    | Percent
    | Watts
    | Hertz ->
      Json.Lens.setPartial Json.String_ (u |> Units.symbol)
    | Other other ->
      Json.write "other" other
    | Scaled (units, scale) ->
      Json.write "units" units
      *> Json.write "scale" scale
    | Offset (units, offset) ->
      Json.write "units" units
      *> Json.write "offset" offset
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
        | "%" -> Percent |> JsonResult.Value, json
        | "W" -> Watts |> JsonResult.Value, json
        | "Hz" -> Hertz |> JsonResult.Value, json
        | unknown ->
          let msg = sprintf "Unknown unit type represented as string '%s'" unknown
          JsonResult.Error msg, json

      | Json.Object o ->
        match o with
        | Val "other" other ->
          Other (Json.deserialize other) |> JsonResult.Value, json
        | Val "scale" scale & Val "units" units ->
          Scaled (Json.deserialize units, Json.deserialize scale) |> JsonResult.Value, json
        | Val "offset" offset & Val "units" units ->
          Offset (Json.deserialize units, Json.deserialize offset) |> JsonResult.Value, json
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

  let scaleBits : float -> float * string =
    let prefix = [| ""; "k"; "M"; "G"; "T"; "P" |]
    fun value ->
      let index = min (int (log10 value) / 3) (prefix.Length - 1)
      1. / 10.**(float index * float 3), sprintf "%sbit" prefix.[index]

  // grafana/public/app/kbn.js:374@g20d5d0e
  let calculate (calcFactor : float -> float * string) =
    fun (value : float) ->
      let factor, unitStr = calcFactor value
      value * factor, unitStr

  // Given a Unit, returns the scaling function and the list of units available.
  let scale units value : float * string =
    let scale2to10 _ = 1024., "TODO"
    match units with
    | Bits ->
      calculate scaleBits value
    | Bytes ->
      calculate scaleBits value
    | Seconds ->
      calculate scaleSeconds value
    | x ->
      calculate scale2to10 value
        //["ns"; "µs"; "ms"; "s"; "min"; "h"; "days"]
        //["B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]

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

  static member ToValue (PointName xs) : Value<unit> =
    Value.setLensPartial Value.Array_ (xs |> Array.map Value.String |> List.ofArray)

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
  open Hopac.Infixes
  open NodaTime
  open System.Threading.Tasks
  open System.Diagnostics
  open Logary.Internals
  open Logary.KnownLiterals

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

    /// Lens you can use to get the list of errors in this message.
    /// Also see Logary errors: https://gist.github.com/haf/1a5152b77ec64bf10fe8583a081dbbbf
    let errors_ : PLens<Message, Value list> =
      field_ ErrorsFieldName >?-> Field.value_ >??> Value.Array_

    /// Lens to the context field 'service'
    let service_ : PLens<Message, string> =
      contextValue_ ServiceContextName >??> Value.String_

    let tags_ : Lens<Message, Set<string>> =
      let array : Lens<Message, Value list> =
        let tagsL = context_ >-?> key_ TagsContextName
        let tagsLA = tagsL >??> Value.Array_
        (fun x -> Lens.getPartialOrElse tagsLA [] x),
        (fun v x ->
          if x.context |> Map.containsKey TagsContextName then
            x |> Lens.setPartial tagsLA v
          else
            { x with context = x.context |> Map.add TagsContextName (Array []) }
            |> Lens.setPartial tagsLA v)

      let strings : Lens<Value list, Set<string>> =
        let read =
          List.choose (function String s -> Some s | _ -> None)
          >> Set.ofList
        let write xs existing =
          xs |> Set.map String |> List.ofSeq |> List.append existing
        read, write

      array >--> strings

  ///////////////// FIELDS ////////////////////

  /// Get a partial setter lens to a field
  [<CompiledName "SetField">]
  let inline setField name value message =
    Lens.setPartial (Lenses.field_ name) (Field.init value) message

  /// Get a partial setter lens to a field with an unit
  [<CompiledName "SetFieldUnit">]
  let inline setFieldUnit name value units message =
    Lens.setPartial (Lenses.field_ name) (Field.initWithUnit value units) message

  /// You can also choose to construct a Field yourself, using the object model
  /// that Logary has for its data. That way you don't have to rely on having
  /// static ToValue methods on your data objects.
  [<CompiledName "SetFieldValue">]
  let setFieldValue (name : string) (field : Field) message =
    Lens.setPartial (Lenses.field_ name) field message

  [<CompiledName "SetFieldValues">]
  let setFieldValues (fields : (string * Field) seq) message =
    fields |> Seq.fold (fun m (name, value) -> setFieldValue name value m) message

  [<CompiledName "SetFieldValuesArray">]
  let setFieldValuesArray (fields : (string * Field)[]) message =
    fields |> Array.fold (fun m (name, value) -> setFieldValue name value m) message

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromMap (m : Map<string, obj>) message =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> k, Field (Value.ofObject v, None))
    |> flip setFieldValues message

  [<CompiledName "SetFieldFromObject">]
  let setFieldFromObject name (data : obj) message =
    setFieldValue name (Field (Value.ofObject data, None)) message

  /// Reflects over the object and sets the appropriate fields.
  [<CompiledName "SetFieldsFromObject">]
  let setFieldsFromObject (data : obj) message =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> k, Field (Value.ofObject v, None))
    |> flip setFieldValues message

  /// Get a partial getter lens to a field
  [<CompiledName "TryGetField">]
  let tryGetField name message =
    Lens.getPartial (Lenses.field_ name) message

  /// Tag the message
  [<CompiledName "Tag">]
  let tag (tag : string) (message : Message) =
    let tags =
      Lens.get Lenses.tags_ message
      |> Set.add tag

    Lens.set Lenses.tags_ tags message

  /// Check if the Message has a tag
  [<CompiledName "HasTag">]
  let hasTag (tag : string) (message : Message) =
    Lens.get Lenses.tags_ message
    |> Set.contains tag

  ///////////////// CONTEXT ////////////////////

  /// Sets a context value by trying to find the ToValue method on the type
  /// passed.
  [<CompiledName "SetContext">]
  let inline setContext name value message =
    Lens.setPartial (Lenses.contextValue_ name) (Value.serialize value) message

  /// Sets a context value.
  [<CompiledName "SetContextValue">]
  let setContextValue name value message =
    Lens.setPartial (Lenses.contextValue_ name) value message

  [<CompiledName "SetContextValues">]
  let setContextValues (values : (string * Value) seq) message =
    values |> Seq.fold (fun m (name, value) -> setContextValue name value m) message

  [<CompiledName "SetContextFromMap">]
  let setContextFromMap (m : Map<string, obj>) message =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.ofObject v)
    |> List.ofSeq
    |> fun fields -> setContextValues fields message

  /// Uses reflection to set all
  [<CompiledName "SetContextFromObject">]
  let setContextFromObject (data : obj) message =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.ofObject v)
    |> List.ofSeq
    |> fun values -> setContextValues values message

  /// Tries to get a context value
  [<CompiledName "TryGetContext">]
  let inline tryGetContext name message =
    Lens.getPartial (Lenses.contextValue_ name) message

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
    | ScalarValue v ->
      pnv.Name, Field (Value.ofObject v, None)
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
      fn input ^-> fun res ->
      sw.Stop()

      let value, units = sw.toGauge()

      res, gaugeWithUnit pointName units value
    )

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
      let exns = Lens.getPartialOrElse Lenses.errors_ [] msg
      exns @ flattenedExns

    // If there's no "errors" field, add it
    let msg =
      match Lens.getPartial Lenses.errors_ msg with
      | Some x ->
        msg

      | None ->
        setFieldValue ErrorsFieldName (Field (Array [], None)) msg

    Lens.setPartial Lenses.errors_ exnsNext msg