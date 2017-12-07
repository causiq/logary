namespace Logary.Formatting

open Chiron
open System.Reflection
open System.Collections
open System.Collections.Generic
open Logary.Internals.TypeShape

module Json =

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
  let rec private toJsonTypeShape (encoderFac : obj -> Json option) (data : obj) =
    let toJsonWithFac : obj -> Json = toJsonTypeShape encoderFac
    if isNull data then Json.Null
    else
      match encoderFac data with
      | Some json -> json
      | _ ->
        let shape = TypeShape.FromValue data
        match data, shape with
        | :? string as str , _ ->  Json.Encode.string str
        | :? array<byte> as bytes , _ ->  Json.Encode.bytes bytes
        | _, Shape.KeyValuePair kv ->
          kv.Accept {
            new IKeyValuePairVisitor<Json> with
              member __.Visit<'k,'v> () =
                let kv = data :?> KeyValuePair<'k,'v>
                Json.Array [toJsonWithFac kv.Key; toJsonWithFac kv.Value]
          }  
        | :? DictionaryEntry as de, _ ->
          [de.Key; de.Value] |> Json.Encode.listWith toJsonWithFac
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
                         |> toJsonWithFac
                   })
                |> Json.Array
          }
        | _, Shape.FSharpOption s ->
          s.Accept {
            new IFSharpOptionVisitor<Json> with
              member __.Visit<'t> () = 
                Json.Encode.optionWith toJsonWithFac (data :?> Option<'t>)
          }
        | _, Shape.KeyStringValuePairSeq kv ->
          kv.Accept {
            new IKeyValuePairVisitor<Json> with
              member __.Visit<'k,'v> () =
                data :?> seq<KeyValuePair<string,'v>>
                |> Seq.map (|KeyValue|)
                |> Map.ofSeq
                |> Json.Encode.mapWith toJsonWithFac
          }    
        | :? IEnumerable as ie , _ ->
          let enumerator = ie.GetEnumerator ()
          let jsonList = [
            while enumerator.MoveNext () do
              yield toJsonWithFac enumerator.Current
          ]
          Json.Array jsonList
        | _ -> tryToJsonWithDefault data shape.Type
  and private tryToJsonWithDefault (data : obj) dataType =
    // can do some dataType toJson methodinfo cache here
    let m = dataType.GetMethod("ToJson",BindingFlags.Public|||BindingFlags.Static,null,CallingConventions.Any,[|dataType|],null) 
    // maybe chiron default can hard code use type test above, avoid reflection here
    let mOrDefault = 
      if isNull m then 
        chironDefaultsType.GetMethod("ToJson",BindingFlags.Public|||BindingFlags.Static,null,CallingConventions.Any,[|dataType|],null) 
      else m
    if not <| isNull mOrDefault then 
      let json = mOrDefault.Invoke(null,[|data|])
      unbox json
    else
      Json.Encode.string (string data)

  let format (data : obj) = 
    data |> toJsonTypeShape (fun _ -> None) |> Json.format

  let formatWith options encoderFac (data : obj) = 
    data |> toJsonTypeShape encoderFac |> Json.formatWith options
    