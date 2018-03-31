namespace Logary.Formatting

open System
open System.Reflection
open System.Collections.Generic
open Logary
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Internals.TypeShape.Core
open Logary.Internals.TypeShape.Core.Utils

// Logary.HashMap

type IHashMapVisitor<'R> =
  abstract Visit<'K, 'V when 'K : equality> : unit -> 'R

type IShapeHashMap =
  abstract Key: TypeShape
  abstract Value: TypeShape
  abstract Accept: IHashMapVisitor<'R> -> 'R

// System.Collections.Generic.IDictionary`2

type IIDictionaryVisitor<'R> =
  abstract Visit<'K, 'V when 'K : equality> : unit -> 'R

type IShapeIDictionary =
  abstract Key: TypeShape
  abstract Value: TypeShape
  abstract Accept: IIDictionaryVisitor<'R> -> 'R

module Shape =
  let private SomeU = Some() // avoid allocating all the time
  let inline private test<'T> (s : TypeShape) =
    match s with
    | :? TypeShape<'T> -> SomeU
    | _ -> None

  // System.Collections.Generic.IDictionary`2

  type private ShapeIDictionary<'K, 'V when 'K : equality> () =
    interface IShapeIDictionary with
      member __.Key = shapeof<'K>
      member __.Value = shapeof<'V>
      member __.Accept v = v.Visit<'K, 'V> ()

  let (|IDictionary|_|) (shape: TypeShape) =
    match shape.ShapeInfo with
    | Generic(td, ta) when td = typedefof<System.Collections.Generic.IDictionary<_, _>> ->
      Activator.CreateInstanceGeneric<ShapeIDictionary<_, _>>(ta)
      :?> IShapeIDictionary
      |> Some
    | _ ->
      None

  // Logary.HashMap`2

  type private ShapeHashMap<'K, 'V when 'K : equality> () =
    interface IShapeHashMap with
      member __.Key = shapeof<'K>
      member __.Value = shapeof<'V>
      member __.Accept v = v.Visit<'K, 'V> ()

  let (|HashMap|_|) (shape: TypeShape) =
    match shape.ShapeInfo with
    // I could not make 'k when 'k :> IEquatable<'k> work with typedefof<HashMap<_,_>>
    | Generic(td, ta) when td.FullName = "Logary.HashMap`2" ->
      Activator.CreateInstanceGeneric<ShapeHashMap<_,_>>(ta)
      :?> IShapeHashMap
      |> Some
    | _ ->
      None

  let (|LogLevel|_|) (shape: TypeShape) = test<Logary.LogLevel> shape
  let (|PointName|_|) (shape: TypeShape) = test<Logary.PointName> shape
  //let (|Units|_|) (shape: TypeShape) = test<Logary.Units> shape

module JsonHelper =
  module E = Chiron.Serialization.Json.Encode

  type ICustomJsonEncoderRegistry =
    abstract TryGetRegistration: System.Type -> CustomJsonEncoderFactory option

  and CustomJsonEncoderFactory =
    JsonEncoder<obj> -> JsonEncoder<obj>

  and CustomJsonEncoderFactory<'t> =
    JsonEncoder<obj> -> JsonEncoder<'t>

  let emptyJsonEncoderRegistry =
    { new ICustomJsonEncoderRegistry with
        member __.TryGetRegistration _ = None }

  let private chironDefaultsType = typeof<Logary.Internals.Chiron.Inference.Internal.ChironDefaults>

  let private getKey (valueType: Type) =
    let prop = valueType.GetProperty("Key")
    fun (instance: obj) -> prop.GetValue(instance, null)

  let private getValue (valueType: Type) =
    let prop = valueType.GetProperty("Value")
    fun (instance: obj) -> prop.GetValue(instance, null)

  // TO CONSIDER https://github.com/eiriktsarpalis/TypeShape/blob/master/src/TypeShape/Utils.fs
  // let rec internal toJson (registry: ICustomJsonEncoderRegistry) (t: System.Type): obj -> Json =
  let rec toJson<'T>(): 'T -> Json =
//    printfn "toJson %O" typeof<'T>
    use ctx = new TypeGenerationContext()
    toJsonCached<'T> ctx

  and private toJsonCached<'T> (ctx: TypeGenerationContext): 'T -> Json =
//    printfn "toJsonCached %O" typeof<'T>
    if typeof<'T> = typeof<obj> then
      toJsonAux<'T> ctx
    else
      match ctx.InitOrGetCachedValue<'T -> Json>(fun c t -> c.Value t) with
      | Cached (value = r) ->
        r
      | NotCached t ->
        let p = toJsonAux<'T> ctx
        ctx.Commit t p

  and private toJsonAux<'T> (ctx: TypeGenerationContext): 'T -> Json =
//    printfn "toJsonAux %O" typeof<'T>
    let wrap (f: 'a -> Json) = unbox f
    let inline enc (a: 'a) = Inference.Json.encode a
    let mkFieldPrinter (field: IShapeMember<'DeclaringType>) =
      field.Accept {
        new IMemberVisitor<'DeclaringType, string * ('DeclaringType -> Json)> with
          member __.Visit(field: ShapeMember<'DeclaringType, 'Field>) =
            let fp = toJsonCached<'Field> ctx

            let project declaringValue =
              match box declaringValue with
              | null ->
                Json.Null
              | _ ->
                field.Project declaringValue |> fp

            let project declaringValue =
              try
                project declaringValue
              with e ->
                let message = Reflection.memberAccessExn field.Label e
                Json.String message

            field.Label, project
      }

    match shapeof<'T> with
    | Shape.Unit ->
      wrap (fun () -> Json.Null)

    | Shape.Char ->
      wrap (fun (c: char) -> E.string (string c))

    | Shape.String ->
      wrap (fun (s: string) -> Inference.Json.encode s)

    | Shape.Bool ->
      wrap (fun (x: bool) -> Inference.Json.encode x)

    | Shape.Double ->
      wrap (fun (x: float) -> Inference.Json.encode x)

    | Shape.Single ->
      wrap (fun (x: Single) -> Inference.Json.encode x)

    | Shape.BigInt ->
      wrap (fun (x: bigint) -> Inference.Json.encode x)

    | Shape.UInt16 ->
      wrap (fun (x: uint16) -> Inference.Json.encode x)

    | Shape.UInt32 ->
      wrap (fun (x: uint32) -> Inference.Json.encode x)

    | Shape.UInt64 ->
      wrap (fun (x: uint64) -> Inference.Json.encode x)

    | Shape.Int16 ->
      wrap (fun (x: int16) -> Inference.Json.encode x)

    | Shape.Int32 ->
      wrap (fun (x: int) -> Inference.Json.encode x)

    | Shape.Int64 ->
      wrap (fun (x: int64) -> Inference.Json.encode x)

    | Shape.Guid ->
      wrap (fun (x: Guid) -> Inference.Json.encode x)

    | Shape.TimeSpan ->
      wrap (fun (x: TimeSpan) -> E.string (x.ToString()))

    | Shape.DateTime ->
      wrap (fun (x: DateTime) -> Inference.Json.encode x)

    | Shape.DateTimeOffset ->
      wrap (fun (x: DateTimeOffset) -> Inference.Json.encode x)

    | Shape.LogLevel ->
      wrap (fun (level: LogLevel) -> Json.String (level.ToString()))

    | Shape.PointName ->
      wrap (fun (PointName xs) -> E.arrayWith Json.String xs)

    | meta when meta.Type.Namespace = "System.Reflection" ->
      fun x -> Json.String (x.ToString())

    | Shape.Exception s ->
      wrap (fun (e: exn) ->
        let fields = ref JsonObject.empty

        if not (isNull e.Data) && e.Data.Count > 0 then
          fields := !fields |> JsonObject.add "data" (toJsonCached<System.Collections.IDictionary> ctx e.Data)

        if not (isNull e.HelpLink) then
          fields := !fields |> JsonObject.add "helpLink" (Json.String e.HelpLink)

        if not (isNull e.InnerException) then
          fields := !fields |> JsonObject.add "inner" (toJsonCached<exn> ctx e.InnerException)

        if e.HResult <> Unchecked.defaultof<int> then
          fields := !fields |> JsonObject.add "hresult" (toJsonCached<_> ctx e.HResult)

        fields := !fields |> JsonObject.add "message" (Json.String e.Message)
        fields := !fields |> JsonObject.add "source" (Json.String e.Source)

        if not (String.IsNullOrWhiteSpace e.StackTrace) then
          let lines = DotNetStacktrace.parse e.StackTrace
          fields := !fields |> JsonObject.add "stacktrace" (toJsonCached<StacktraceLine[]> ctx lines)

        if not (isNull e.TargetSite) then
          fields := !fields |> JsonObject.add "targetSite" (Json.String e.TargetSite.Name)

        Json.Object !fields)

    | Shape.FSharpMap s when s.Key = shapeof<string> ->
      s.Accept
        { new IFSharpMapVisitor<'T -> Json> with
            member x.Visit<'k, 'v when 'k : comparison> () =
              let fp = toJsonCached<'v> ctx
              wrap (fun (m: Map<string, 'v>) -> E.mapWith fp m)
        }

    | Shape.FSharpMap s ->
      s.Accept
        { new IFSharpMapVisitor<'T -> Json> with
            member x.Visit<'k, 'v when 'k : comparison> () =
              let fp = toJsonCached<'v> ctx
              wrap (fun (m: Map<'k, 'v>) -> E.mapWithCustomKey (sprintf "%O") fp m)
        }

    | Shape.HashMap s when s.Key = shapeof<string> ->
      s.Accept
        { new IHashMapVisitor<'T -> Json> with
            member x.Visit<'k, 'v when 'k : equality> () =
              let fp = toJsonCached<'v> ctx
              wrap (fun (m: HashMap<string, 'v>) ->
                m
                |> HashMap.toSeqPair
                |> Seq.fold (fun s (KeyValue (k, v)) -> s |> JsonObject.add (sprintf "%O" k) (fp v)) JsonObject.empty
                |> Json.Object)
        }

    | Shape.HashMap s ->
      fun (o: 'T) ->
        let e = box o :?> System.Collections.IEnumerable
        toJsonCached<_> ctx e

    | Shape.Dictionary s ->
      s.Accept
        { new IDictionaryVisitor<'T -> Json> with
            member __.Visit<'k, 'a when 'k: equality> () =
              let ap = toJsonCached<'a> ctx
              wrap (fun (d: Dictionary<'k, 'a>) ->
                d
                |> Seq.fold (fun s (KeyValue (k, v)) -> s |> JsonObject.add (sprintf "%O" k) (ap v)) JsonObject.empty
                |> Json.Object)
        }

    | Shape.IDictionary s ->
      s.Accept
        { new IIDictionaryVisitor<'T -> Json> with
            member __.Visit<'k, 'a when 'k: equality> () =
              let ap = toJsonCached<'a> ctx
              wrap (fun (d: IDictionary<'k, 'a>) ->
                d
                |> Seq.fold (fun s (KeyValue (k, v)) -> s |> JsonObject.add (sprintf "%O" k) (ap v)) JsonObject.empty
                |> Json.Object)
        }

    | Shape.FSharpOption s ->
      s.Accept
        { new IFSharpOptionVisitor<'T -> Json> with
            member __.Visit<'a> () = // 'T = 'a option
              let toJ = toJsonCached<'a> ctx
              wrap (Option.fold (fun _ -> toJ) Json.Null)
        }

    | Shape.ByteArray ->
      wrap E.bytes

    | Shape.FSharpList s ->
      s.Accept
        { new IFSharpListVisitor<'T -> Json> with
            member __.Visit<'a> () = //  'T = 'a list
              //printfn ">>>> list case"
              let ap = toJsonCached<'a> ctx
              wrap (E.listWith ap)
        }

    | Shape.FSharpSet s ->
      s.Accept
        { new IFSharpSetVisitor<'T -> Json> with
            member __.Visit<'a when 'a : comparison> () = //  'T = Set<'a>
              //printfn ">>>> set case"
              let ap = toJsonCached<'a> ctx
              wrap (E.setWith ap)
        }

    | Shape.Array s when s.Rank = 1 ->
      s.Accept
        { new IArrayVisitor<'T -> Json> with
            member __.Visit<'a> rank = //  'T = 'a[]
              //printfn ">>>> array case"
              let ap = toJsonCached<'a> ctx
              wrap (E.arrayWith ap)
        }

    | Shape.Tuple (:? ShapeTuple<'T> as shape) ->
      let elemPrinters = shape.Elements |> Array.map mkFieldPrinter
      fun (t: 'T) ->
        elemPrinters
        |> Array.map (fun (_, ep) -> ep t)
        |> E.array

    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
      (* {
        "type": "CaseName",
        "user": { // case name field
          "id": 2,
          ...
        },
        "companyId": 1234 // case name field
      } *)
      let mkUnionCasePrinter (s: ShapeFSharpUnionCase<'T>) =
        let fieldPrinters = s.Fields |> Array.map mkFieldPrinter
        fun (u: 'T) ->
          Map [
            yield "type", String s.CaseInfo.Name
            yield!
              match fieldPrinters with
              | [||] ->
                [||]
              | [| label, fp |] ->
                [| label, fp u |]
              | fps ->
                fps |> Array.map (fun (label, fp) -> label, fp u)
          ]
          |> JsonObject.ofMap
          |> Json.Object

      let casePrinters =
        shape.UnionCases |> Array.map mkUnionCasePrinter

      fun (u: 'T) ->
        let printer = casePrinters.[shape.GetTag u]
        printer u

    | Shape.Enumerable s ->
      //printfn "JsonHelper: encode enumerable %O" s
      // TO CONSIDER: seq<KeyValue<string, 'a>> with reflection on the Value property
      match s.Element with
      | Shape.KeyValuePair kvps ->
        fun (o: 'T) ->
          match box o with
          | :? System.Collections.IEnumerable as e ->
            let m = ref JsonObject.empty
            let en = e.GetEnumerator()
            while en.MoveNext () do
              let valueType = en.Current.GetType()
              let key = string (getKey valueType en.Current)
              let value = getValue valueType en.Current
              let json = toJsonCached<obj> ctx value
              m := !m |> JsonObject.add key json
            Json.Object !m
          | _ ->
            failwithf "Unexpected shape %O" s

      | _ ->
        s.Accept
          { new IEnumerableVisitor<'T -> Json> with
              member __.Visit<'T, 'a when 'T :> seq<'a>> () =
                //printfn "enumerable shape generating for %O" typeof<'a>
                let ap = toJsonCached<'a> ctx
                wrap (fun (xs: seq<'a>) ->
                  xs |> Seq.fold (fun s x -> ap x :: s) []
                     |> Seq.toArray
                     |> E.array)
          }

    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
      let fps = shape.Fields |> Array.map (fun f -> lazy (mkFieldPrinter f))
      fun (input: 'T) ->
        fps
        |> Seq.fold (fun s x ->
            let label, fp = x.Value
            s |> JsonObject.add label (fp input)) JsonObject.empty
        |> Json.Object

    | Shape.Poco (:? ShapePoco<'T> as shape) when shape.Properties.Length <> 0 ->
      let fps = shape.Properties |> Array.map (fun p -> lazy (mkFieldPrinter p))
      fun (input: 'T) ->
        fps
        |> Seq.fold (fun s x ->
            let label, fp = x.Value
            s |> JsonObject.add label (fp input)) JsonObject.empty
        |> Json.Object

    | other when other = shapeof<obj> ->
//      printfn "JsonHelper: encode other %O" other
      let toJsonTDef =
        Type.GetType("Logary.Formatting.JsonHelper, Logary")
          .GetMethod("toJsonCached", BindingFlags.Static ||| BindingFlags.NonPublic)
      fun value ->
        match box value with
        | null ->
          Json.Null
        | _ ->
          let typ = value.GetType() // string
          if typ = typeof<obj> then Json.Object JsonObject.empty else
          let toJsonT = toJsonTDef.MakeGenericMethod(typ) // toJsonCached<string>(): string -> Json: MethodInfo
          let resFuncT = typedefof<FSharpFunc<_, _>>.MakeGenericType(typ, typeof<Json>) // string -> Json:
          let resFuncInvoker = resFuncT.GetMethod("Invoke") // (string -> Json).Invoke: MethodInfo
          let refFunc = toJsonT.Invoke(null, [| box ctx |]) // : toJson: string -> Json
          resFuncInvoker.Invoke(refFunc, [| value |]) :?> Json // (toJson value): Json

    | other when typeof<System.Collections.IEnumerable>.IsAssignableFrom other.Type ->
      fun (o: 'T) ->
        let vals = ResizeArray<_>()
        let e = box o :?> System.Collections.IEnumerable
        for value in e do
          let json = toJson<obj>() value
          vals.Add json
        E.array (vals.ToArray())

    | other ->
      //failwithf "JsonHelper: encode got shape %A" other
      fun x -> Json.String (x.ToString())

