namespace Logary.Formatting

module internal JsonHelper =

  open System
  open System.Reflection
  open System.Collections.Generic
  open Logary.Internals
  open Logary.Internals.Chiron
  open Logary.Internals.TypeShape

  module E = Chiron.Serialization.Json.Encode
  module EI = Chiron.Inference.Json.Encode

  type ICustomJsonEncoderRegistry =
    abstract TryGetRegistration: System.Type -> CustomJsonEncoderFactory option

  and BoxedLambda =
    abstract InputType: System.Type
    abstract InvokeBoxed: obj -> Json

  and Wrap<'T>(f: 'T -> Json) =
    interface BoxedLambda with
     member __.InputType = typeof<'T>
     member __.InvokeBoxed o = f (o :?> 'T)

  and CustomJsonEncoderFactory =
    JsonEncoder<obj> -> JsonEncoder<obj>

  and CustomJsonEncoderFactory<'t> =
    JsonEncoder<obj> -> JsonEncoder<'t>

  let emptyJsonEncoderRegistry =
    { new ICustomJsonEncoderRegistry with
        member __.TryGetRegistration _ = None }

  let private chironDefaultsType = typeof<Chiron.Inference.Internal.ChironDefaults>

  /// try some basic type test for make a better decision to transform to json type
  /// since chiron use monad and static type resolver style,
  /// implementation here avoid using reflection as much as possible.
  /// container type like list, array, dictionary... are support by testing IEnumerable
  /// tuple in dotnet core or framework 4.7.1 are support ITuple, https://docs.microsoft.com/zh-cn/dotnet/api/system.runtime.compilerservices.ituple?view=netframework-4.7.1
  /// so implement by typeshape reflection first, then migrate by test ITuple interface.
  /// if user want to support union, record, poco... they can offer their own encoderFac.
  let rec internal toJson (registry: ICustomJsonEncoderRegistry) (t: System.Type): BoxedLambda =
    let wrap f = Wrap f :> BoxedLambda
    let encode value = toJson registry (value.GetType())

    let (|FromTypeRegistry|_|) (shape: TypeShape) =
      registry.TryGetRegistration shape.Type

    let inline mkFieldPrinter (shape: IShapeMember<'DeclaringType>) =
      shape.Accept
        { new IMemberVisitor<'DeclaringType, string * ('DeclaringType -> BoxedLambda)> with
            member __.Visit (field: ShapeMember<'DeclaringType, 'Field>) =
              field.Label, encode << field.Project
        }

    match TypeShape.Create t with
    | FromTypeRegistry factory ->
      factory encode
    | Shape.Unit ->
      wrap (fun () -> Json.Null)
    | Shape.String ->
      wrap E.string
    | Shape.Bool ->
      wrap (fun (b: bool) -> E.bool b)
    | Shape.Double ->
      wrap (fun (f: float) -> E.number (f.ToString()))
    | Shape.Single ->
      wrap (fun (s: single) -> E.number (s.ToString()))
    | Shape.BigInt ->
      wrap (fun (bi: bigint) -> E.number (bi.ToString()))
    | Shape.Int16 ->
      wrap (fun (u: int16) -> E.number (u.ToString()))
    | Shape.Int32 ->
      wrap (fun (u: int32) -> E.number (u.ToString()))
    | Shape.Int64 ->
      wrap (fun (u: int64) -> E.number (u.ToString()))
    | Shape.UInt16 ->
      wrap (fun (u: uint16) -> E.number (u.ToString()))
    | Shape.UInt32 ->
      wrap (fun (u: uint32) -> E.number (u.ToString()))
    | Shape.UInt64 ->
      wrap (fun (u: uint64) -> E.number (u.ToString()))
    | Shape.DateTime ->
      wrap (fun (dt: System.DateTime) -> E.string (dt.ToString "o"))
    | Shape.DateTimeOffset ->
      wrap (fun (dto: System.DateTimeOffset) -> E.string (dto.ToString "o"))

    | Shape.FSharpOption s ->
      s.Accept
        { new IFSharpOptionVisitor<BoxedLambda> with
            member __.Visit<'a> () =
              wrap (function
                | None ->
                  Json.Null
                | Some x ->
                  E.mapWith encode x)
        }

    | Shape.ByteArray ->
      wrap (fun (bs: byte[]) -> E.bytes bs)

    | Shape.Enumerable s ->
      match s.Element with
      | Shape.KeyValuePair ks when ks.Key = shapeof<string> ->
        s.Accept
          { new IEnumerableVisitor<BoxedLambda> with
              member __.Visit<'e, 't when 'e :> seq<'t>> () =
                let fold =
                  Seq.fold (fun (s: JsonObject) (KeyValue (key, value)) ->
                    let json = encode value
                    s |> JsonObject.add key json)
                    JsonObject.empty
                wrap (fold >> Json.Object)
          }

      | _ ->
        s.Accept
          { new IEnumerableVisitor<BoxedLambda> with
              member __.Visit<'e, 't when 'e :> seq<'t>> () =
                let map =
                  Seq.map encode
                  >> Seq.toList
                wrap (map >> Json.Array)
          }

    | Shape.Exception s ->
      let iff b value = if b then Some value else None

      let field (pred: exn -> (string * 'x) option) (e: exn): (JsonObject -> _) -> JsonObject -> _ =
        match pred e with
        | Some (name, field) ->
          fun next state ->
            next (state |> JsonObject.add name (encode field))
        | None ->
          fun next state ->
            next state

      let data = field (fun e -> iff (not (isNull e.Data) && e.Data.Count > 0) ("data", e.Data))
      let helpLink = field (fun e -> iff (not (isNull e.HelpLink)) ("helpLink", e.HelpLink))
      let hresult = field (fun e -> iff (not (Unchecked.defaultof<int> = e.HResult)) ("hresult", e.HResult))
      let innerException = field (fun e -> iff (not (isNull e.InnerException)) ("innerException", e.InnerException))
      let message = field (fun e -> Some ("message", e.Message))
      let source = field (fun e -> Some ("source", e.Source))
      let stackTrace = field (fun e -> iff (not (String.IsNullOrWhiteSpace e.StackTrace)) ("stackTrace", e.StackTrace))
      let targetSite = field (fun e -> iff (not (isNull e.TargetSite)) ("targetSite", e.TargetSite))

      let composed e =
        data e // ('s -> _) -> ('s -> _)
        >> helpLink e
        >> hresult e
        >> innerException e
        >> message e
        >> source e
        >> stackTrace e
        >> targetSite e

      s.Accept
        { new IExceptionVisitor<BoxedLambda> with
            member __.Visit () =
              wrap (fun e -> composed e Json.Object JsonObject.empty)
        }

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as s) ->
      let fps = s.Fields |> Array.map mkFieldPrinter
      wrap (fun data ->
        Seq.fold (fun s (label, fp) -> s |> JsonObject.add label (fp data)) JsonObject.empty
        |> Json.Object)

    | Shape.Poco (:? ShapePoco<'T> as s) ->
      let fps = s.Properties |> Array.map mkFieldPrinter
      wrap (fun data ->
        Seq.fold (fun s (label, fp) -> s |> JsonObject.add label (fp data)) JsonObject.empty
        |> Json.Object)

    | shape ->
      printfn "Shape %O triggered default! %O" shape shape.Type
      wrap (fun data -> tryToJsonWithDefault shape.Type data)

  and private tryToJsonWithDefault dataType (data: obj) =
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