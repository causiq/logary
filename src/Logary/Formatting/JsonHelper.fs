namespace Logary.Formatting

module internal JsonHelper =

  open Chiron
  open System.Reflection
  open System.Collections
  open System.Collections.Generic
  open Logary.Internals.TypeShape

  module E = Chiron.Serialization.Json.Encode
  module EI = Chiron.Inference.Json.Encode

  type ICustomJsonEncoderRegistry =
    abstract TryGetRegistration : System.Type -> CustomJsonEncoderFactory option
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
  let rec internal toJsonTypeShape (registry : ICustomJsonEncoderRegistry) (data : obj) =
    let resolver : JsonEncoder<obj> = toJsonTypeShape registry
    let (|CustomFactory|_|) (shape: TypeShape) = registry.TryGetRegistration shape.Type

    if isNull data then Json.Null
    else
      let shape = TypeShape.FromValue data
      match data, shape with
      | _, CustomFactory factory -> factory resolver data
      | :? string as str , _ ->  E.string str
      | :? array<byte> as bytes , _ ->  E.bytes bytes
      | _, Shape.KeyValuePair kv ->
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
      | :? DictionaryEntry as de, _ ->
        Map [ "Key", de.Key; "Value",  de.Value]
        |> E.mapWith resolver
      | _, Shape.Tuple s ->
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
      | _, Shape.FSharpOption s ->
        s.Accept {
          new IFSharpOptionVisitor<Json> with
            member __.Visit<'t> () =
              Json.Encode.optionWith resolver (data :?> Option<'t>)
        }
      | _, Shape.KeyStringValuePairSeq kv ->
        kv.Accept {
          new IKeyValuePairVisitor<Json> with
            member __.Visit<'k,'v> () =
              data :?> seq<KeyValuePair<string,'v>>
              |> Seq.map (|KeyValue|)
              |> Map.ofSeq
              |> E.mapWith resolver
        }
      | :? IEnumerable as ie , _ ->
        let enumerator = ie.GetEnumerator ()
        let jsonList = [
          while enumerator.MoveNext () do
            yield resolver enumerator.Current
        ]
        Json.Array jsonList
      | _ -> tryToJsonWithDefault data shape.Type

  and private tryToJsonWithDefault (data: obj) dataType =
    // can do some dataType toJson methodinfo cache here
    let m = dataType.GetMethod("ToJson",BindingFlags.Public|||BindingFlags.Static,null,CallingConventions.Any,[|dataType|],null)
    // maybe chiron default can hard code use type test above, avoid reflection here
    let mOrDefault =
      if isNull m then
        chironDefaultsType.GetMethod("ToJson",BindingFlags.Public|||BindingFlags.Static,null,CallingConventions.Any,[|dataType|],null)
      else m
    if not <| isNull mOrDefault && mOrDefault.ReturnType = typeof<Json> then
      let json = mOrDefault.Invoke(null,[|data|])
      unbox json
    else
      E.string (string data)
