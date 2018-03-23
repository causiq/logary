namespace Logary.Formatting

module internal JsonHelper =

  open System.Reflection
  open System.Collections
  open System.Collections.Generic
  open Logary.Internals
  open Logary.Internals.Chiron
  open Logary.Internals.TypeShape

  module E = Chiron.Serialization.Json.Encode
  module EI = Chiron.Inference.Json.Encode

  type ICustomJsonEncoderRegistry =
    abstract TryGetRegistration: System.Type -> CustomJsonEncoderFactory option

  and CustomJsonEncoderFactory = JsonEncoder<obj> -> JsonEncoder<obj>

  and CustomJsonEncoderFactory<'t> = JsonEncoder<obj> -> JsonEncoder<'t>

  let emptyJsonEncoderRegistry =
    {
      new ICustomJsonEncoderRegistry with
        member __.TryGetRegistration _ = None
    }

  module internal Shape =

    let (|KeyStringValuePairSeq|_|) (shape: TypeShape) =
      match shape with
      | Shape.Enumerable ie ->
        match ie.Element with
        | Shape.KeyValuePair kv when kv.Key.Type = typeof<string> -> Some kv
        | _ -> None
      | _ -> None

  let private chironDefaultsType = typeof<Chiron.Inference.Internal.ChironDefaults>

  /// try some basic type test for make a better decision to transform to json type
  /// since chiron use monad and static type resolver style,
  /// implementation here avoid using reflection as much as possible.
  /// so record, union, poco... are not support by default.
  /// container type like list, array, dictionary... are support by testing IEnumerable
  /// tuple in dotnet core or framework 4.7.1 are support ITuple, https://docs.microsoft.com/zh-cn/dotnet/api/system.runtime.compilerservices.ituple?view=netframework-4.7.1
  /// so implement by typeshape reflection first, then migrate by test ITuple interface.
  /// if user want to support union, record, poco... they can offer their own encoderFac.
  let rec internal toJsonTypeShape (registry: ICustomJsonEncoderRegistry) (data: obj) =
    let resolver: JsonEncoder<obj> = toJsonTypeShape registry
    let (|CustomFactory|_|) (shape: TypeShape) = registry.TryGetRegistration shape.Type
    let wrap (k: 'a -> Json) = fun inp -> k (unbox inp)

    let inline mkFieldPrinter (shape: IShapeMember<'DeclaringType>) =
      shape.Accept
        { new IMemberVisitor<'DeclaringType, string * ('DeclaringType -> Json)> with
            member __.Visit (field: ShapeMember<'DeclaringType, 'Field>) =
              field.Label, resolver << field.Project }

    if isNull data then Json.Null
    else
    match TypeShape.FromValue data with
    | CustomFactory factory ->
      factory resolver data
    | Shape.Unit ->
      Json.Null
    | Shape.String ->
      wrap E.string data
    | Shape.Bool ->
      wrap (fun (b: bool) -> E.bool b) data
    | Shape.Double ->
      wrap (fun (f: float) -> E.number (f.ToString())) data
    | Shape.Single ->
      wrap (fun (s: single) -> E.number (s.ToString())) data
    | Shape.BigInt ->
      wrap (fun (bi: bigint) -> E.number (bi.ToString())) data
    | Shape.Int16 ->
      wrap (fun (u: int16) -> E.number (u.ToString())) data
    | Shape.Int32 ->
      wrap (fun (u: int32) -> E.number (u.ToString())) data
    | Shape.Int64 ->
      wrap (fun (u: int64) -> E.number (u.ToString())) data
    | Shape.UInt16 ->
      wrap (fun (u: uint16) -> E.number (u.ToString())) data
    | Shape.UInt32 ->
      wrap (fun (u: uint32) -> E.number (u.ToString())) data
    | Shape.UInt64 ->
      wrap (fun (u: uint64) -> E.number (u.ToString())) data
    | Shape.DateTime ->
      wrap (fun (dt: System.DateTime) -> E.string (dt.ToString "o")) data
    | Shape.DateTimeOffset ->
      wrap (fun (dto: System.DateTimeOffset) -> E.string (dto.ToString "o")) data

    | Shape.FSharpOption s ->
      s.Accept
        { new IFSharpOptionVisitor<obj -> Json> with
            member __.Visit<'a> () =
              wrap (function
                | None ->
                  Json.Null
                | Some x ->
                  E.mapWith resolver x)
        }
        data

    | Shape.ByteArray ->
      wrap (fun (bs: byte[]) -> E.bytes bs) data

    | Shape.KeyValuePair kv ->
      kv.Accept {
        new IKeyValuePairVisitor<Json> with
          member __.Visit<'k,'v> () =
            if typeof<string> = typeof<'k> then
              let kv = data :?> KeyValuePair<string,'v>
              Map [kv.Key, kv.Value] |> E.mapWith resolver
            else
              let kv = data :?> KeyValuePair<'k,'v>
              Map [ "Key", box kv.Key; "Value", box kv.Value]
              |> E.mapWith resolver
      }

    | Shape.Tuple s ->
      s.Accept {
        new ITupleVisitor<Json> with
          member __.Visit shapeTuple =
            shapeTuple.Elements
            |> List.ofArray
            |> List.map (fun el ->
               el.Accept {
                 new IMemberVisitor<'tupleType,Json> with
                   member __.Visit shapeMember =
                     shapeMember.Project (data :?> 'tupleType)
                     |> resolver
               })
            |> Json.Array
      }

    | Shape.KeyStringValuePairSeq kv ->
      kv.Accept {
        new IKeyValuePairVisitor<Json> with
          member __.Visit<'k,'v> () =
            data :?> seq<KeyValuePair<string,'v>>
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq
            |> E.mapWith resolver
      }

    | Shape.Enumerable s ->
      wrap (fun (ie: IEnumerable) ->
        let enumerator = ie.GetEnumerator ()
        [| while enumerator.MoveNext () do yield resolver enumerator.Current |]
        |> E.array)
        data

    | Shape.FSharpRecord (:? ShapeFSharpRecord<obj> as s) ->
      s.Fields
        |> Array.map mkFieldPrinter
        |> Seq.fold (fun s (label, fp) -> s |> JsonObject.add label (fp data)) JsonObject.empty
        |> Json.Object

    | Shape.Poco (:? ShapePoco<obj> as s) ->
      s.Properties
        |> Array.map mkFieldPrinter
        |> Seq.fold (fun s (label, fp) -> s |> JsonObject.add label (fp data)) JsonObject.empty
        |> Json.Object

    | shape ->
      tryToJsonWithDefault data shape.Type

  and private tryToJsonWithDefault (data: obj) dataType =
    // can do some dataType toJson methodinfo cache here
    let m = dataType.GetMethod("ToJson", BindingFlags.Public|||BindingFlags.Static,null,CallingConventions.Any,[|dataType|],null)
    // maybe chiron default can hard code use type test above, avoid reflection here
    let mOrDefault =
      if isNull m then
        chironDefaultsType.GetMethod("ToJson", BindingFlags.Public|||BindingFlags.Static,null, CallingConventions.Any,[|dataType|],null)
      else m
    if not <| isNull mOrDefault && mOrDefault.ReturnType = typeof<Json> then
      let json = mOrDefault.Invoke(null,[|data|])
      unbox json
    else
      E.string (string data)