namespace Logary.Formatting

open System.Collections.Concurrent
open Logary.Internals.TypeShape.Core
open Logary.Internals.Chiron

module Json =

  module E = Serialization.Json.Encode
  module EI = Inference.Json.Encode

  let private customJsonEncoderDic = ConcurrentDictionary<TypeShape, JsonEncoderFactory>()

  let configureEncoder<'t> (factory: JsonEncoderFactory<'t>) =
    let untypedEncoder (resolver: JsonEncoder<obj>) (data: obj) =
      match data with
      | :? 't as typedData ->
        factory resolver typedData
      | _ ->
        let message = sprintf "Could not down-cast value to '%s'. Value is: %O" typeof<'t>.FullName data
        Json.String message
    customJsonEncoderDic.[shapeof<'t>] <- untypedEncoder

  let customJsonEncoderRegistry = lazy (
    // Example:
    //configureEncoder<Gauge>(fun resolve (Gauge (v, u)) ->
    //  let (vs, us) = Units.scale u (v.toFloat())
    //  E.string (sprintf "%s %s" (vs.ToString()) us))

    { new JsonEncoderRegistry with
        member __.tryGet (shape: TypeShape) =
          match customJsonEncoderDic.TryGetValue shape with
          | true, factory ->
            Some factory
          | false , _ ->
            customJsonEncoderDic.Keys
            |> Seq.tryFind (fun baseType -> baseType.Type.IsAssignableFrom shape.Type)
            |> Option.bind (fun key ->
               match customJsonEncoderDic.TryGetValue key with
               | true, factory -> Some factory
               | false , _ -> None)
    })
