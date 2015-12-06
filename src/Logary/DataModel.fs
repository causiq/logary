namespace Logary

open System
open System.Reflection
open Logary
open Logary.Utils.Chiron
open Logary.Utils.Chiron.Operators
open Logary.Utils.Aether
open Logary.Utils.Aether.Operators

type ContentType = string

type Value =
  | String of string
  | Bool of bool
  | Float of decimal
  | Int64 of int64
  | BigInt of bigint
  | Binary of byte [] * ContentType
  | Fraction of int64 * int64
  | Object of Map<string, Value> // NOTE: moved from ComplexValue
  | Array of Value list // NOTE: moved from ComplexValue

  (* Isomorphisms *)

  static member String__ : PIso<Value, string> =
    (function | String x -> Some x
              | _ -> None), String

  static member Bool__ : PIso<Value, bool> =
    (function | Bool x -> Some x
              | _ -> None), Bool

  static member Float__ : PIso<Value, decimal> =
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

  static member Float_ : PLens<Value, decimal> =
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
      Json.Lens.setPartial Json.Number_ f

    | Int64 i ->
      Json.Lens.setPartial Json.Number_ (decimal i)

    | BigInt bi ->
      Json.Lens.setPartial Json.Number_ (decimal bi)

    | Binary (bs, contentType) ->
      Json.write "mime" contentType
      *> Json.write "data" ("base64:" + Convert.ToBase64String bs)

    | Fraction (n, d) ->
      Json.write "fraction" (n, d)

    | Array values ->
      Json.write "array" (values |> List.map Json.serialize)

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
        JsonResult.Value (Float f), json

      | Json.Array arr ->
        match arr |> List.traverseChoiceA Json.tryDeserialize with
        | Choice1Of2 values ->
          JsonResult.Value (Array values), json

        | Choice2Of2 err ->
          JsonResult.Error err, json

      | Json.Object o ->
        o
        |> Seq.map (fun kv -> kv.Key, kv.Value)
        |> Seq.toList
        |> List.traverseChoiceA (fun (key, jValue) ->
          match Json.tryDeserialize jValue with
          | Choice1Of2 (vValue : Value) ->
            Choice1Of2 (key, vValue)

          | Choice2Of2 err ->
            Choice2Of2 err)
        |> Choice.map Map.ofList
        |> function
        | Choice1Of2 (result : Map<string, Value>) ->
          JsonResult.Value (Object result), json

        | Choice2Of2 err ->
          JsonResult.Error err, json

      | Json.Null () ->
        JsonResult.Error "Cannot handle Null json values", json

module Escaping =
    let private unescaped i =
         i >= 0x20 && i <= 0x21
      || i >= 0x23 && i <= 0x5b
      || i >= 0x5d && i <= 0x10ffff

    let escape (s: string) =
      let rec escape r =
        function | [] -> r
                 | h :: t when (unescaped (int h)) ->
                    escape (r @ [ h ]) t
                 | h :: t ->
                    let n =
                      match h with
                      | '"' -> [ '\\'; '"' ]
                      | '\\' -> [ '\\'; '\\' ]
                      | '\b' -> [ '\\'; 'b' ]
                      | '\f' -> [ '\\'; 'f' ]
                      | '\n' -> [ '\\'; 'n' ]
                      | '\r' -> [ '\\'; 'r' ]
                      | '\t' -> [ '\\'; 't' ]
                      | x -> [ '\\'; 'u' ] @ [ for c in ((int x).ToString ("X4")) -> unbox c ]

                    escape (r @ n) t

      new string (List.toArray (escape [] [ for c in s -> unbox c ]))

module Conversions =
  let asDecimal = function
  | Float f -> Some f
  | Int64 i -> Some (decimal i)
  | BigInt bi -> Some (decimal bi)
  | _ -> None

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
      Value.setLensPartial Value.Float_ x

    static member inline ToValue (x: float) =
      Value.setLensPartial Value.Float_ (decimal x)

    static member inline ToValue (x: int) =
      Value.setLensPartial Value.Float_ (decimal x)

    static member inline ToValue (x: int16) =
      Value.setLensPartial Value.Int64_ (int64 x)

    static member inline ToValue (x: int64) =
      Value.setLensPartial Value.Int64_ x

    static member inline ToValue (x: single) =
      Value.setLensPartial Value.Float_ (decimal x)

    static member inline ToValue (x: string) =
      Value.setLensPartial Value.String_ x

    static member inline ToValue (x: uint16) =
      Value.setLensPartial Value.Int64_ (int64 x)

    static member inline ToValue (x: uint32) =
      Value.setLensPartial Value.Int64_ (int64 x)

    static member inline ToValue (x: uint64) =
        Value.setLensPartial Value.Float_ (decimal x)

    (* Common Types *)

    static member inline ToValue (x: DateTime) =
      Value.setLensPartial Value.String_ (x.ToUniversalTime().ToString("o"))

    static member inline ToValue (x: DateTimeOffset) =
      Value.setLensPartial Value.String_ (x.ToString("o"))

    static member inline ToValue (x: Guid) =
      Value.setLensPartial Value.String_ (string x)

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

    let inline serialize a =
      toValue a

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Value =
    open Logary.Internals
    open System.Collections.Generic

    let rec fromObject : obj -> Value = function

    // Built-in types
    | :? bool as b    -> Bool b
    | :? int8 as i    -> Int64 (int64 i)
    | :? uint8 as i   -> Int64 (int64 i)
    | :? int16 as i   -> Int64 (int64 i)
    | :? uint16 as i  -> Int64 (int64 i)
    | :? int32 as i   -> Int64 (int64 i)
    | :? uint32 as i  -> Int64 (int64 i)
    | :? int64 as i   -> Int64 (int64 i)
    | :? uint64 as i  -> Float (decimal i)
    | :? bigint as i  -> BigInt i
    | :? decimal as d -> Float d
    | :? float32 as f -> Float (decimal f)
    | :? float as f   -> Float (decimal f)
    | :? char as c    -> String (string c)
    | :? string as s  -> String s

    // Common BCL types
    | :? Guid as g              -> String (string g)
    | :? DateTime as dt         -> String (dt.ToUniversalTime().ToString("o"))
    | :? DateTimeOffset as dto  -> String (dto.ToString("o"))

    // Collections
    | :? (byte array) as bytes -> Binary (bytes, "application/octet-stream")
    | :? IEnumerable<KeyValuePair<string, obj>> as dict ->
      Seq.map (fun (KeyValue (k, v)) -> (k, fromObject v)) dict
      |> Map |> Object
    //| :? Map<string, obj> as map -> Map.map (fun _ v -> fromObject v) map |> Object
    | :? IEnumerable<obj> as ie ->
      Seq.map fromObject ie
      |> Seq.toList |> Array

    // POCOs
    | a ->
      Map.fromObj a
      |> Map.map (fun _ v -> fromObject v)
      |> Object

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
    Json.Lens.setPartial Json.String_ (u |> Units.symbol)

  static member FromJson (_ : Units) =
        function
        | "b" -> Bits
        | "B" -> Bytes
        | "s" -> Seconds
        | "m" -> Metres
        | "" -> Scalar
        | "A" -> Amperes
        | "K" -> Kelvins
        | "mol" -> Moles
        | "cd" -> Candelas
        | x -> failwith "TODO: implement units parser"
    <!> Json.Lens.getPartial Json.String_

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
  PointName of hierarchy:string list
with
  override x.ToString() =
    let (PointName hiera) = x in String.concat "." hiera

  static member hierarchy_ : Lens<PointName, string list> =
    (fun (PointName h) -> h),
    fun v x -> PointName v

  static member FromJson(_ : PointName) : Json<PointName> =
    fun json ->
      Json.tryDeserialize json
      |> function
      | Choice1Of2 xs -> Json.init (PointName xs) json
      | Choice2Of2 err -> Json.error err json

  static member ToJson (PointName xs) : Json<unit> =
    Json.Lens.setPartial Json.Array_ (xs |> List.map Json.String)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PointName =

  let empty = PointName []

  let ofSingle (segment : string) =
    PointName [ segment ]

  let ofList (hiera : string list) =
    PointName hiera

  [<CompiledName "Joined">]
  let joined (pn : PointName) =
    pn.ToString()

  [<CompiledName "FromString">]
  let parse (s: string) =
    String.split '.' s |> ofList

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
    *> Json.write "units" maybeUnit

  static member FromJson (_ : Field) : Json<Field> =
    (fun value units -> Field (value, units))
    <!> Json.read "value"
    <*> Json.tryRead "units"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // remove when field moved outside
module Field =

  let inline initWithUnit value units =
    Field (Value.serialize value, Some units)

  let inline init value =
    Field (Value.serialize value, None)

type Message =
  { name      : PointName
    value     : PointValue
    /// the semantic-logging data
    fields    : Map<PointName, Field>
    /// the principal/actor/user/tenant/act-as/oauth-id data
    session   : Value
    /// where in the code?
    context   : Map<string, Value>
    /// what urgency?
    level     : LogLevel
    /// when?
    timestamp : int64 }

    static member name_ : Lens<Message, PointName> =
      (fun x -> x.name),
      fun v x -> { x with name = v }

    static member value_ : Lens<Message, PointValue> =
      (fun x -> x.value),
      fun v x -> { x with value = v }

    static member fields_ : Lens<Message, Map<PointName, Field>> =
      (fun x -> x.fields),
      (fun v x -> { x with fields = v })

    static member session_ : Lens<Message, Value> =
      (fun x -> x.session),
      fun v x -> { x with session = v }

    static member context_ : Lens<Message, Map<string, Value>> =
      (fun x -> x.context),
      (fun v x -> { x with context = v })

    static member ToJson (m : Message) =
      Json.write "name" m.name
      *> Json.write "value" m.value
      *> Json.write "fields" (m.fields |> Seq.map (fun kv -> kv.Key.ToString(), kv.Value) |> Map.ofSeq)
      *> Json.write "session" m.session
      *> Json.write "context" m.context
      *> Json.write "level" m.level
      *> Json.write "timestamp" m.timestamp

    static member FromJson (_ : Message) =
      (fun name value (fields : Map<string, _>) session context level ts ->
        let fields =
          fields
          |> Seq.map (fun kv -> PointName (kv.Key |> String.split '.'), kv.Value)
          |> Map.ofSeq

        { name    = name
          value   = value
          fields  = fields
          session = session
          context = context
          level   = level
          timestamp = ts })
      <!> Json.read "name"
      <*> Json.read "value"
      <*> Json.read "fields"
      <*> Json.read "session"
      <*> Json.read "context"
      <*> Json.read "level"
      <*> Json.read "timestamp"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
  open NodaTime
  open Logary.Internals

  let field_ name : PLens<Message, Field> =
    Message.fields_ >-?> key_ (PointName.ofSingle name)

  /// Get a partial setter lens to a field
  let inline field name value =
    Lens.setPartial (field_ name) (Field.init value)

  /// Get a partial setter lens to a field with an unit
  let inline fieldUnit name value units =
    Lens.setPartial (field_ name) (Field.initWithUnit value units)

  /// Get a partial getter lens to a field
  let inline tryGetField name =
    Lens.getPartial (field_ name)

  let contextValue_ name : PLens<Message, Value> =
    Message.context_ >-?> key_ name

  let inline contextValue name value =
    Lens.setPartial (contextValue_ name) (value)

  let inline tryGetContextValue name =
    Lens.getPartial (contextValue_ name)

  /// Contains lenses and functions for manipulating message fields.
  module Fields =

    let errors_ : PLens<Message, Value list> =
      field_ "errors" >?-> Field.value_ >??> Value.Array_

  /// Contains lenses and functions for manipulating message contexts.
  module Context =

    /// Lens to the context field 'service'
    let service_ : PLens<Message, string> =
      contextValue_ "service" >??> Value.String_

  /// Creates a new event message with level
  [<CompiledName "CreateEvent">]
  let event level msg =
    { name      = PointName.empty
      value     = Event msg
      fields    = Map.empty
      session   = Object Map.empty
      context   = Map.empty
      level     = level
      timestamp = SystemClock.Instance.Now.Ticks }

  /// Creates a new metric message with data point name, unit and value
  [<CompiledName "CreateMetric">]
  let metricWithUnit dp unit value =
    { name = dp
      value = Gauge (value, unit)
      fields = Map.empty
      session = Object Map.empty
      context = Map.empty
      level = LogLevel.Info
      timestamp = SystemClock.Instance.Now.Ticks }

  /// Creates a new metric message with data point name and scalar value
  [<CompiledName "CreateMetric">]
  let metric dp value =
    { name = dp
      value = Gauge (value, Units.Scalar)
      fields = Map.empty
      session = Object Map.empty
      context = Map.empty
      level = LogLevel.Info
      timestamp = SystemClock.Instance.Now.Ticks }

  /// Create a verbose event message
  [<CompiledName "Verbose">]
  let verbose = event Verbose

  /// Create a verbose event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "VerboseFormat">]
  let verbosef fmt = Printf.kprintf (event Verbose) fmt

  /// Create a debug event message
  [<CompiledName "Debug">]
  let debug = event Debug

  /// Create a debug event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "DebugFormat">]
  let debugf fmt = Printf.kprintf (event LogLevel.Debug) fmt

  /// Create an info event message
  [<CompiledName "Info">]
  let info = event Info

  /// Create a info event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "InfoFormat">]
  let infof fmt = Printf.kprintf (event LogLevel.Info) fmt

  /// Create an warn event message
  [<CompiledName "Warn">]
  let warn = event LogLevel.Warn

  /// Create a warn event message for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "WarnFormat">]
  let warnf fmt = Printf.kprintf (event LogLevel.Warn) fmt

  /// Create an error event message
  [<CompiledName "Error">]
  let error = event LogLevel.Error

  /// Write a error event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "ErrorFormat">]
  let errorf fmt = Printf.kprintf (event LogLevel.Error) fmt

  /// Create a fatal event message
  [<CompiledName "Fatal">]
  let fatal = event Fatal

  /// Create a fatal event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "FatalFormat">]
  let fatalf fmt = Printf.kprintf (event Fatal) fmt

  [<CompiledName "SetName">]
  let setName name (msg : Message) = { msg with name = name }

  [<CompiledName "SetLevel">]
  let setLevel lvl msg = { msg with level = lvl}

  [<CompiledName "SetTimestamp">]
  let setTimestamp ts msg = { msg with timestamp = ts}

  /// Replaces the value of the message with a new Event with the supplied format
  [<CompiledName "SetEvent">]
  let setEvent format msg = { msg with value = Event format}

  [<CompiledName "AddField">]
  let addField ((name, field) : (PointName * Field)) msg =
    {msg with fields = Map.add name field msg.fields}

  [<CompiledName "AddFields">]
  let addFields (fields: (PointName * Field) seq) msg =
    {msg with fields = Map.fold (fun acc k v -> Map.add k v acc) msg.fields (Map fields)}

  // TODO: this data should be structured on the F# side of things, not obj
  [<CompiledName "AddData">]
  let addData (data: obj) msg =
    let fields =
      Map.fromObj data
      |> Seq.map (fun (KeyValue (k, v)) ->
          PointName.ofSingle k,
          Field (Value.fromObject v, None))

    addFields fields msg

  let rec private exnToFields (e : exn) =
    let fields =
      [("type", String (e.GetType ()).FullName);
       ("message", String e.Message)] @
      (if e.TargetSite <> null then [("targetsite", String <| e.TargetSite.ToString ())] else []) @
      (if e.StackTrace <> null then [("backtrace", String e.StackTrace)] else [])

    Map <|
      if e.InnerException <> null then
        ("inner", Object <| exnToFields e.InnerException) :: fields
      else
        fields

  /// Adds a new exception to the "errors" field in the message.
  /// AggregateExceptions are automatically expanded.
  [<CompiledName "AddException">]
  let addExn (e : exn) msg =
    let flattenedExns =
      match e with
      | :? AggregateException as ae ->
        ae.InnerExceptions |> Seq.map exnToFields |> Seq.toList
      | _ ->
        exnToFields e :: []

    let exnsNext =
      let exns = Lens.getPartialOrElse Fields.errors_ [] msg
      exns @ (flattenedExns |> List.map Object)

    Lens.setPartial Fields.errors_ exnsNext msg

  /// Converts a String.Format-style format string and an array of arguments into
  /// a message template and a set of fields.
  [<CompiledName "TemplateFromFormat">]
  let private templateFromFormat (format : string) (args : obj[]) =
    let fields =
      args
      |> Seq.mapi (fun i v ->
        PointName.ofSingle (sprintf "arg%i" i),
        Field (Value.fromObject v, None))
      |> Seq.toList

    // Replace {0}..{n} with {arg0}..{argn}
    let template = Seq.fold (fun acc i -> String.replace (sprintf "{%i}" i) (sprintf "{arg%i}" i) acc) format [0..args.Length]
    (template, fields)

  /// Creates a new event with given level, format and arguments.
  /// Format may contain String.Format-esque format placeholders.
  [<CompiledName "CreateFormattedEvent">]
  let eventf (level, format, [<ParamArray>] args : obj[]) =
    let (template, fields) = templateFromFormat format args

    event level template
    |> addFields fields