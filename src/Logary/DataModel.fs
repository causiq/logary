namespace Logary.DataModel

open System
open Logary
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

  static member StringPIso : PIso<Value, string> =
      (function | String x -> Some x
                | _ -> None), String

  static member BoolPIso : PIso<Value, bool> =
      (function | Bool x -> Some x
                | _ -> None), Bool

  static member FloatPIso : PIso<Value, decimal> =
      (function | Float x -> Some x
                | _ -> None), Float

  static member IntPIso : PIso<Value, int64> =
      (function | Int64 x -> Some x
                | _ -> None), Int64

  static member BigIntPIso : PIso<Value, bigint> =
      (function | BigInt x -> Some x
                | _ -> None), BigInt

  static member BinaryPIso : PIso<Value, byte [] * ContentType> =
      (function | Binary (bs, ct) -> Some (bs, ct)
                | _ -> None), Binary

  static member FractionPIso : PIso<Value, int64 * int64> =
      (function | Fraction (n, d) -> Some (n, d)
                | _ -> None), Fraction

  static member ArrayPIso : PIso<Value, Value list> =
      (function | Array x -> Some x
                | _ -> None), Array

  static member ObjectPIso : PIso<Value, Map<string, Value>> =
      (function | Object x -> Some x
                | _ -> None), Object

  (* Lenses *)

  static member StringPLens : PLens<Value, string> =
      idLens <-?> Value.StringPIso

  static member BoolPLens : PLens<Value, bool> =
      idLens <-?> Value.BoolPIso

  static member FloatPLens : PLens<Value, decimal> =
      idLens <-?> Value.FloatPIso

  static member IntPLens : PLens<Value, int64> =
      idLens <-?> Value.IntPIso

  static member BigIntPLens : PLens<Value, bigint> =
      idLens <-?> Value.BigIntPIso

  static member BinaryPLens : PLens<Value, byte[] * ContentType> =
      idLens <-?> Value.BinaryPIso

  static member FractionPLens : PLens<Value, int64 * int64> =
      idLens <-?> Value.FractionPIso

  static member ArrayPLens : PLens<Value, Value list> =
      idLens <-?> Value.ArrayPIso

  static member ObjectPLens : PLens<Value, Map<string, Value>> =
      idLens <-?> Value.ObjectPIso

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

[<AutoOpen>]
module Capture =

  // TODO: this structure is both a Reader and Writer monad, but only needs
  // to be a writer monad
  type Value<'a> =
    Value -> ValueResult<'a> * Value

  and ValueResult<'a> =
    | ValueResult of 'a
    | Error of string

  [<RequireQualifiedAccess>]
  module Value =

    let inline init (a: 'a) : Value<'a> =
      fun value ->
        ValueResult a, value

    let inline error (e: string) : Value<'a> =
      fun value ->
        Error e, value

    let inline internal ofResult result =
      fun value ->
        result, value

    let inline bind (m: Value<'a>) (f: 'a -> Value<'b>) : Value<'b> =
      fun json ->
        match m json with
        | ValueResult a, json -> (f a) json
        | Error e, json -> Error e, json

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
        | _ -> Error (sprintf "couldn't use lens %A on value '%A'" l value), value

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
      Value.setLensPartial Value.BoolPLens x

    static member inline ToValue (x: decimal) =
      Value.setLensPartial Value.FloatPLens x

    static member inline ToValue (x: float) =
      Value.setLensPartial Value.FloatPLens (decimal x)

    static member inline ToValue (x: int) =
      Value.setLensPartial Value.FloatPLens (decimal x)

    static member inline ToValue (x: int16) =
      Value.setLensPartial Value.IntPLens (int64 x)

    static member inline ToValue (x: int64) =
      Value.setLensPartial Value.IntPLens x

    static member inline ToValue (x: single) =
      Value.setLensPartial Value.FloatPLens (decimal x)

    static member inline ToValue (x: string) =
      Value.setLensPartial Value.StringPLens x

    static member inline ToValue (x: uint16) =
      Value.setLensPartial Value.IntPLens (int64 x)

    static member inline ToValue (x: uint32) =
      Value.setLensPartial Value.IntPLens (int64 x)

    static member inline ToValue (x: uint64) =
        Value.setLensPartial Value.FloatPLens (decimal x)

    (* Common Types *)

    static member inline ToValue (x: DateTime) =
      Value.setLensPartial Value.StringPLens (x.ToUniversalTime().ToString("o"))

    static member inline ToValue (x: DateTimeOffset) =
      Value.setLensPartial Value.StringPLens (x.ToString("o"))

    static member inline ToValue (x: Guid) =
      Value.setLensPartial Value.StringPLens (string x)

    (* Json Type *)

    static member inline ToValue (x: Value) =
        Value.setLens idLens x

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
      Value.setLens idLens (Array ((Array.toList >> List.map toValue) x))

    (* Lists *)

    static member inline ToValue (x: 'a list) =
      Value.setLens idLens (Array (List.map toValue x))

    (* Maps *)

    static member inline ToValue (x: Map<string,'a>) =
      Value.setLens idLens (Object (Map.map (fun _ a -> toValue a) x))

    (* Options *)

    static member inline ToValue (x: 'a option) =
      match x with | None -> Value.init ()
                   | Some a -> Value.setLens idLens (toValue a)

    (* Sets *)

    static member inline ToValue (x: Set<'a>) =
      Value.setLens idLens (Array ((Set.toList >> List.map toValue) x))

    (* Tuples *)

    static member inline ToValue ((a, b)) =
      Value.setLens idLens (Array [ toValue a; toValue b ])

    static member inline ToValue ((a, b, c)) =
      Value.setLens idLens (Array [ toValue a; toValue b; toValue c ])

  [<RequireQualifiedAccess>]
  module Value =

    let inline write key value =
      Value.setLensPartial (Value.ObjectPLens >??> mapPLens key) (toValue value)

    let inline serialize a =
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

  let rec symbol = function
    | Bits -> "b"
    | Bytes -> "B"
    | Seconds -> "s"
    | Metres -> "m"
    | Scalar -> ""
    | Amperes -> "A"
    | Kelvins -> "K"
    | Moles -> "mol"
    | Candelas -> "cd"
    | Mul (a, b) -> String.Concat [ "("; symbol a; "*"; symbol b; ")" ]
    | Pow (a, b) -> String.Concat [ symbol a; "^("; symbol b; ")" ]
    | Div (a, b) -> String.Concat [ "("; symbol a; "/"; symbol b; ")" ]
    | Root a -> String.Concat [ "sqrt("; symbol a; ")" ]
    | Log10 a -> String.Concat [ "log10("; symbol a; ")" ]

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

  // grafana/public/app/kbn.js:374@g20d5d0e
  let doScale (fFactor : float -> int64) (scaledUnits : string list) =
    fun (decimals : uint16) (scaledDecimals : uint16) (value : float) ->
      (value : float), "KiB"

  let scale units : uint16 -> uint16 -> float -> float*string =
    match units with
    | Bits -> (fun _ -> 1000L), ["b"; "Kib"; "Mib"; "Gib"; "Tib"; "Pib"; "Eib"; "Zib"; "Yib"]
    | Bytes -> (fun _ -> 1024L), ["B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB"]
    | Seconds -> scaleSeconds, ["ns"; "µs"; "ms"; "s"; "min"; "h"; "days"]
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

type PointName = string list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PointName =
  let joined name = String.concat "." name

type PointValue =
  /// Value at point in time
  | Gauge of Value * Units
  /// Any sort of derived measure
  | Derived of Value * Units
  /// All simple-valued fields' values can be templated into the template string
  /// when outputting the value in the target.
  | Event of template:string

type Field = Field of Value * Units option // move outside this module

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
    session   : Value // NOTE: changed from Map<PointName, Field>
    /// where in the code?
    context   : Map<string, Value>
    /// what urgency?
    level     : LogLevel
    /// when?
    timestamp : int64 }

    static member fields_ : Lens<Message, Map<PointName, Field>> =
      (fun x -> x.fields),
      (fun v x -> { x with fields = v })

    static member contextFields_ : Lens<Message, Map<string, Value>> =
      (fun x -> x.context),
      (fun v x -> { x with context = v})

    static member field_ name : PLens<Message, Field> =
      Message.fields_ >-?> (Logary.Utils.Aether.mapPLens [name])

    static member contextField_ name : PLens<Message, Value> =
      Message.contextFields_ >-?> (Logary.Utils.Aether.mapPLens name)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
  open NodaTime

  /// Get a partial setter lens to a field
  let inline field name value =
    Lens.setPartial (Message.field_ name) (Field.init value)

  /// Get a partial setter lens to a field with an unit
  let inline fieldUnit name value units =
    Lens.setPartial (Message.field_ name) (Field.initWithUnit value units)

  /// Get a partial getter lens to a field
  let inline tryGetField name =
    Lens.getPartial (Message.field_ name)

  let inline contextField name value =
    Lens.setPartial (Message.contextField_ name) (value)

  let inline tryGetContextField name =
    Lens.getPartial (Message.contextField_ name)

  module Fields =
    let errors = Message.field_ "errors"
    let errorsGet =
      Lens.getPartial errors
      >> Option.bind (function Field (Array a, None) -> Some a | _  -> None)

    let errorsSet (value: Value list) =
      Lens.setPartial errors (Field (Array value, None))

  module Context =
    /// Gets the context field 'service', or an empty string if the field doesn't exist or is of the wrong type.
    let service = Message.contextField_ "service"
    let serviceGet =
      Lens.getPartial service
      >> Option.bind (function String s -> Some s | _ -> None)
      >> (fun x -> defaultArg x "")

    let serviceSet = String >> Lens.setPartial service

  /// Creates a new event message with level
  let event level msg =
    { name = [] // check in logger
      value = Event msg
      fields = Map.empty
      session = Object Map.empty // default, hoist to static
      context = Map.empty // fix, take from logger
      level   = level // fix
      timestamp = SystemClock.Instance.Now.Ticks }

  /// Creates a new metric message with data point name, unit and value
  let metric dp unit value =
    { name = dp
      value = Gauge (value, unit)
      fields = Map.empty
      session = Object Map.empty // TODO
      context = Map.empty // TODO
      level = LogLevel.Info
      timestamp = SystemClock.Instance.Now.Ticks
    }

  /// Creates a new metric message with data point name and scalar value
  let metric' dp value =
    { name = dp
      value = Gauge (value, Units.Scalar)
      fields = Map.empty
      session = Object Map.empty // TODO
      context = Map.empty // TODO
      level = LogLevel.Info
      timestamp = SystemClock.Instance.Now.Ticks
    }


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

  let setLevel lvl msg = {msg with level = lvl}
  let setTimestamp ts msg = {msg with timestamp = ts}

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
  /// AggregateExceptions are automatically expanded into multiple different exceptions.
  let addExn (e : exn) msg =
    let errors = defaultArg (Fields.errorsGet msg) []

    let newErrors =
      List.map Object <|
        match e with
        | :? AggregateException as ae ->
          Seq.map exnToFields ae.InnerExceptions |> Seq.toList
        | _ ->
          [exnToFields e]

    Fields.errorsSet (errors @ newErrors) msg
