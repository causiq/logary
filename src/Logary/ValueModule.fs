namespace Logary

open System
open Logary.Internals.Aether

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

  // TODO: move out of this file,
  // TODO: use TypeShape,
  // TODO: configurable

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