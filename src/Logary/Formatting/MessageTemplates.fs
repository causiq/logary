namespace Logary

module MessageTemplates =

  open System
  open System.Globalization
  open System.Collections.Generic
  open System.Runtime.Serialization
  open Logary.Internals.FsMtParserFull

  type DestrHint = Default | Structure  | Stringify

  [<AutoOpen>]
  module FsMtParserFullExtensions =
    type CaptureHint with
      member __.ToDestrHint () =
        match __ with
        | CaptureHint.Stringify -> DestrHint.Stringify
        | CaptureHint.Structure -> DestrHint.Structure
        | _ -> DestrHint.Default

    type Property with
      member __.Pos =
         match Int32.TryParse (__.name, NumberStyles.None, NumberFormatInfo.CurrentInfo) with
           | true, value -> value
           | false, _ -> Int32.MaxValue
      member __.IsPositional () =
        let pos = __.Pos
        pos >= 0 && pos < Int32.MaxValue

  type Template =
    { raw : string
      tokens : TemplateToken []}
  with
    member __.Properties =
      __.tokens
      |> Array.choose (function | Text _ -> None | Property prop -> Some prop)

    member __.IsAllPositional =
      __.Properties |> Array.forall (fun prop -> prop.IsPositional ())

  and TemplateToken =
  | Text of string
  | Property of Property

  /// refId in ref count depend on the order of the obj send to calculate, e.g:
  /// A { userInfo : { some info }, users : [ userInfo] } ,
  /// if send userInfo first then A { userInfo : $1 { some info} , users : [ $1 ]}
  /// if send users first then A { userInfo : $1 , users : [ $1 { some info} ]}
  /// things may get weird : check "cycle reference" test in Formatting.fs -> Logary.Test.fsproj
  /// :(
  /// consider show all refId (this can make format verbose)
  /// or no need refId in TemplatePropertyValue, when cycle reference show null
  type TemplatePropertyValue =
    | RefId of int64
    | ScalarValue of obj
    | SequenceValue of refId:int64 * TemplatePropertyValue list
    | StructureValue of refId:int64 * typeTag:string * values:PropertyNameAndValue list
    | DictionaryValue of refId:int64 * data: (TemplatePropertyValue * TemplatePropertyValue) list
    
  and PropertyNameAndValue =
    { Name:string; Value:TemplatePropertyValue }

  type RefIdManager () =
    let showAsIdSet = new HashSet<int64> ()
    let idGen = new ObjectIDGenerator ()

    /// need invoke when data is complex template property value
    /// since a complex data may destructure to sacalar value
    member __.TryShowAsRefId (data : obj) =
      match idGen.GetId data with
      | refId, false ->
        do showAsIdSet.Add refId |> ignore
        refId, RefId refId |> Some
      | refId, true -> refId, None

    /// consider only cover cycle reference, not multi reference, cycle reference show null, then no need refId
    member __.IsShowRefId id = showAsIdSet.Contains id

  let parse raw =
    let tokens = ResizeArray<TemplateToken>()
    let foundText text = tokens.Add(TemplateToken.Text text)
    let foundProp prop = tokens.Add(TemplateToken.Property prop)
    parseParts raw foundText foundProp

    {
      raw = raw
      tokens = tokens.ToArray()
    }

  [<AutoOpen>]
  module Capturing =

    let inline private capturePositionals (props:Property[]) (args:obj[]) =
      props
      |> Array.map (fun prop ->
         let pos = prop.Pos
         let arg = if pos < args.Length then Some args.[pos] else None
         (prop, arg))

    let inline private captureNamed (props:Property[]) (args:obj[]) =
      props
      |> Array.mapi(fun i prop ->
         let arg = if i < args.Length then Some (args.[i]) else None
         (prop, arg))

    let capture (tpl: Template) (args: obj[]) : (Property * obj option)[]  =
      let cap = if tpl.IsAllPositional then capturePositionals else captureNamed
      cap tpl.Properties args

  module Destructure =

    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations.Patterns

    type DestructureRequest (value: obj, hint: DestrHint, idManager: RefIdManager) =
      member __.Value = value
      member __.Hint = hint
      member __.IdManager = idManager
      member __.WithNewValue (value: obj) =
        DestructureRequest (value, __.Hint, __.IdManager)
      member __.TryDownCast<'t> () =
        match __.Value with
        | :? 't as typedValue -> DestructureRequest<'t>(typedValue, __.Hint, __.IdManager) |> Some
        | _ -> None

    and DestructureResolver = DestructureRequest -> TemplatePropertyValue
    and DestructureStrategy = DestructureRequest -> TemplatePropertyValue option
    and DestructureRequest<'t> (value: 't, hint: DestrHint, idManager: RefIdManager) =
      inherit DestructureRequest (value,hint,idManager)
      member __.Value = value
    and DestructureResolver<'t> = DestructureRequest<'t> -> TemplatePropertyValue
    and CustomDestructureFactory = DestructureResolver -> DestructureResolver
    and CustomDestructureFactory<'t> = DestructureResolver -> DestructureResolver<'t>
    and ICustomDestructureRegistry =
      abstract TryGetRegistration : Type -> CustomDestructureFactory option

    let emptyDestructureRegistry =
      {
        new ICustomDestructureRegistry with
          member __.TryGetRegistration _ = None
      }

    /// maybe we can add more projection strategy: only(whenNotNull,proj)
    let only<'t> (proj: 't -> obj[]) = ()
    let except<'t> (proj: 't -> obj[]) = ()

    module Projection =

      type Projection =
      | Projection of Type * How
      | NotSupport
      and How =
      | Only of string list list
      | Except of string list list
      and ProjectionStrategy = Type -> How option

      (*
        Projection
          ("Exception",
           Only
             [["Message"]; ["StackTrace"]; ["Data"]; ["InnerException"]; ["InnerException"; "Message"];
              ["InnerException"; "StackTrace"]]
              )
      *)
      let private topHierarchy (names: string list list) =
          match names with
          | [] -> Map.empty
          | _ ->
            names
            |> List.fold (fun hieraMap eachHiera ->
               match eachHiera with
               | [] -> hieraMap
               | [head] ->
                 match hieraMap |> Map.tryFind head with
                 | None -> hieraMap |> Map.add head []
                 | _ -> hieraMap
               | head :: rests  ->
                 match hieraMap |> Map.tryFind head with
                 | Some others -> hieraMap |> Map.add head (rests :: others)
                 | _ -> hieraMap |> Map.add head [rests]
               ) (Map.empty)

      let emptyProjectionStrategy _ = None

      let toTopMap how =
        match how with
        | Only names ->
          names |> topHierarchy |> Map.map (fun _ v -> Only v)
        | Except names ->
          names |> topHierarchy |> Map.map (fun _ v -> Except v)

      let rec byExpr expr =
        match expr with
        | Call (_, method, [Lambda( d, NewArray(_, props))]) -> Projection (d.Type, generateHow method.Name props)
        | _ -> NotSupport
      and private generateHow methodName exprs =
        exprs
        |> List.map (fun expr ->
           match expr with
           | Coerce (PropertyGet(Some(outterExpr), prop,_), _) -> generateOutter outterExpr [prop.Name]
           | _ -> [])
        |> fun projs ->
           match methodName with
           | "only" -> Only projs
           | "except" -> Except projs
           | _ -> Only []
      and private generateOutter outterExpr innerProjs =
        match outterExpr with
        | PropertyGet (Some (expr),prop,_) ->
          generateOutter expr (prop.Name :: innerProjs)
        | _ -> innerProjs

    let scalarTypes =
        [ typeof<bool>;      typeof<char>;       typeof<byte>;              typeof<int16>
          typeof<uint16>;    typeof<int32>;      typeof<uint32>;            typeof<int64>
          typeof<uint64>;    typeof<single>;     typeof<double>;            typeof<decimal>
          typeof<string>;    typeof<DateTime>;   typeof<DateTimeOffset>;    typeof<TimeSpan>
          typeof<Guid>;      typeof<Uri>; ]

    let scalarTypeHash = HashSet(scalarTypes)

    let inline private (|?) (f: DestructureStrategy) (g: DestructureStrategy) : DestructureStrategy =
      fun req ->
        match f req with
        | Some _ as d -> d
        | None -> g req

    let someScalar = ScalarValue >> Some
    let someSequence = SequenceValue >> Some
    let someStructure = StructureValue >> Some
    let someDictionary = DictionaryValue >> Some

    let inline tryNull (req : DestructureRequest) =
      if isNull req.Value then someScalar null
      else None

    let inline tryBuiltInTypes (req : DestructureRequest)  =
      if scalarTypeHash.Contains(req.Value.GetType()) then
        someScalar req.Value
      else None

    let inline tryEnum (req : DestructureRequest)  =
      match req.Value with
      | :? Enum as e -> someScalar e
      | _ -> None

    let inline tryByteArrayMaxBytes maxBytes (req : DestructureRequest)  =
      match req.Value with
      | :? array<Byte> as bytes ->
        if bytes.Length <= maxBytes then someScalar bytes
        else
          let inline toHexString (b:byte) = b.ToString("X2")
          let start = bytes |> Seq.take maxBytes |> Seq.map toHexString |> String.Concat
          let description = start + "... (" + string bytes.Length + " bytes)"
          someScalar description
      | _ -> None

    let inline tryByteArray (req : DestructureRequest)  = tryByteArrayMaxBytes 1024 req

    let inline tryReflectionTypes (req : DestructureRequest)  =
        match req.Value with
        | :? Type as t -> someScalar t
        | :? MemberInfo as m -> someScalar m
        | _ -> None

    let inline tryScalar (req : DestructureRequest)  =
      tryNull |? tryBuiltInTypes |? tryEnum |? tryByteArray |? tryReflectionTypes
      <| req

    let inline private isDictionable (t: Type) =
      t.GetInterfaces()
      |> Seq.exists (fun iface ->
         if iface.IsGenericType && iface.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> then
           let elementType = iface.GetGenericArguments().[0]
           elementType.IsGenericType && elementType.GetGenericTypeDefinition() = typedefof<KeyValuePair<_,_>>
         else iface = typeof<System.Collections.IDictionary>)

    let inline private isFSharpList (t: Type) =
      t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Microsoft.FSharp.Collections.List<_>>

    let inline tryContainerTypes (resolver : DestructureResolver) (req : DestructureRequest) =
      let resolverWithValue value = resolver <| req.WithNewValue(value)
      let instance = req.Value
      let refCount = req.IdManager

      match instance.GetType() with
      | t when FSharpType.IsTuple t ->
        match refCount.TryShowAsRefId instance with
        | _, Some pv  -> Some pv
        | refId, None ->
          let tupleValues =
              instance
              |> FSharpValue.GetTupleFields
              |> Seq.map resolverWithValue
              |> Seq.toList
          someSequence (refId, tupleValues)

      | t when isDictionable t ->
        match refCount.TryShowAsRefId instance with
        | _, Some pv  -> Some pv
        | refId, None ->
          let keyProp, valueProp = ref Unchecked.defaultof<PropertyInfo>, ref Unchecked.defaultof<PropertyInfo>
          let getKey o = if isNull !keyProp  then do keyProp := o.GetType().GetRuntimeProperty("Key")
                         (!keyProp).GetValue(o)
          let getValue o = if isNull !valueProp then do valueProp := o.GetType().GetRuntimeProperty("Value")
                           (!valueProp).GetValue(o)
          let objEnumerable = instance :?> System.Collections.IEnumerable |> Seq.cast<obj>
          let skvps = objEnumerable
                      |> Seq.map (fun o ->  resolverWithValue (getKey o),  resolverWithValue (getValue o))
                      |> Seq.toList
          someDictionary (refId, skvps)

      | t when FSharpType.IsUnion t && not <| isFSharpList t ->

        let case, fields = FSharpValue.GetUnionFields(instance, t)
        let caseName = ScalarValue case.Name
        match fields with
        | [||] -> Some caseName
        | [| oneField |] ->
          match refCount.TryShowAsRefId instance with
          | _, Some pv  -> Some pv
          | refId, None ->
            someDictionary (refId, [caseName, resolverWithValue oneField])
        | _ ->
          match refCount.TryShowAsRefId instance with
          | _, Some pv  -> Some pv
          | refId, None ->
            someDictionary (refId, [caseName, resolverWithValue fields])
      | _ ->
        match instance with
        | :? System.Collections.IEnumerable as e ->
          match refCount.TryShowAsRefId instance with
          | _, Some pv  -> Some pv
          | refId, None ->
            let eles = e |> Seq.cast<obj> |> Seq.map resolverWithValue |> Seq.toList
            someSequence (refId, eles)
        | _ -> None

    let lookupPropFlags = BindingFlags.Public ||| BindingFlags.Instance

    let inline private tryGetValueWithProp (propInfo : PropertyInfo) (instance : obj) =
      match propInfo with
      | p when (not <| isNull p)
               && p.CanRead
               && p.GetIndexParameters().Length = 0 ->
        let propValue =
          try p.GetValue(instance)
          with
          | :? TargetInvocationException as ex ->
            (sprintf "The property (%s) accessor threw an (TargetInvocationException): %s" p.Name ex.InnerException.Message) |> box
          | ex ->
            (sprintf "The property (%s) accessor threw an (%A) : %s" p.Name (ex.GetType())  ex.Message) |> box
        Some propValue
      | _ -> None

    let inline private tryGetValueWithType (t : Type) name (instance : obj) =
      t.GetProperty(name,lookupPropFlags,null,null,Array.empty,null) |> tryGetValueWithProp <| instance

    let rec reflectionProperties (tryGetProjection : Projection.ProjectionStrategy) (resolver : DestructureResolver) (req : DestructureRequest) =
      let ty = req.Value.GetType()
      match tryGetProjection ty with
      | None -> reflectionByProjection resolver (Projection.Except []) req // try reflection all
      | Some how -> reflectionByProjection resolver how req
    and private reflectionByProjection (resolver : DestructureResolver) (how : Projection.How) (req : DestructureRequest) =
      let resolverChild childHow childInstance=
        match childHow with
        | Projection.Only ([])
        | Projection.Except ([]) ->
          // means no children names, so try with destr passed in, this will use custom  type projection
          resolver <| req.WithNewValue(childInstance)
        | Projection.Only _
        | Projection.Except _ ->
          // use childHow ignore custom type projection
          reflectionByProjection resolver childHow (req.WithNewValue(childInstance))

      let instance = req.Value
      let refCount = req.IdManager

      if not <| isNull instance then
        match refCount.TryShowAsRefId instance with
        | _, Some pv  -> pv
        | refId, None ->
          let ty = instance.GetType()
          let typeTag = ty.Name

          match how with
          | Projection.Except _ ->
            let topHieraMap = Projection.toTopMap how
            let isInclude name =
              // in except hieraMap and has no childNames
              match topHieraMap |> Map.tryFind name with
              | Some (Projection.Except []) -> false
              | _ -> true
            let nvs =
              ty.GetProperties(lookupPropFlags)
              |> Array.filter (fun p -> not <| isNull p && (isInclude p.Name))
              |> Array.sortBy (fun p -> p.Name)
              |> Array.fold (fun nvs p ->
                 let name = p.Name
                 match tryGetValueWithProp p instance with
                 | None -> nvs
                 | Some propValue ->
                   let childHow =
                     match topHieraMap |> Map.tryFind name with
                     | Some childHow -> childHow
                     | None -> Projection.Except []
                   let tpv = if isNull propValue then ScalarValue null else resolverChild childHow propValue
                   { Name = name; Value = tpv } :: nvs
                 ) List.empty

            StructureValue (refId, typeTag, nvs)

          | Projection.Only _ ->
            let nvs =
              how
              |> Projection.toTopMap
              |> Map.toList
              |> List.fold (fun nvs (name, childHow) ->
                 match tryGetValueWithType ty name instance with
                 | None -> nvs
                 | Some propValue ->
                   let tpv = if isNull propValue then ScalarValue null else resolverChild childHow propValue
                   { Name = name; Value = tpv } :: nvs
                 ) List.empty

            StructureValue (refId, typeTag, nvs)
      else ScalarValue null

    let rec destructure (registry : ICustomDestructureRegistry) (tryGetProjection : Projection.ProjectionStrategy) (req : DestructureRequest) : TemplatePropertyValue =
      let resolver = destructure registry tryGetProjection
      let tryCustom (req : DestructureRequest) =
        match registry.TryGetRegistration (req.Value.GetType()) with
        | Some customFactory -> customFactory resolver req |> Some
        | None -> None
      let tryDefault = tryNull |? tryCustom |? tryScalar |? (tryContainerTypes resolver)
      let catchByReflection (req : DestructureRequest) = reflectionProperties tryGetProjection resolver req
      let catchByScalarOrigin (req : DestructureRequest) = ScalarValue req.Value

      match tryNull req with
      | Some tpv -> tpv
      | _ ->
        match req.Hint with
        | DestrHint.Stringify -> ScalarValue (req.Value.ToString())
        | DestrHint.Structure ->
          match req |> tryDefault with
          | Some tpv -> tpv
          | None -> catchByReflection req
        | DestrHint.Default ->
          match req |> tryDefault with
          | Some tpv -> tpv
          | None -> catchByScalarOrigin req

  module Formatting =

    module Utils =
      let formatWithCustom (provider : IFormatProvider) (arg : obj) format =
        let customFormatter = provider.GetFormat(typeof<System.ICustomFormatter>) :?> System.ICustomFormatter
        match customFormatter with
        | cf when not (isNull cf) ->
          (cf.Format(format, arg, provider))
        | _ ->
          match arg with
          | :? System.IFormattable as f -> f.ToString(format, provider)
          | _ -> arg.ToString()

      let escapeNewlineAndQuote (str : string) =
        if isNull str then String.Empty
        else
          let escape = str.Replace("\"","\\\"").Replace("\r\n",@"\r\n").Replace("\n",@"\n")
          "\"" + escape + "\""

    type WriteState =
      {
        provider : IFormatProvider
        idManager : RefIdManager
      }

    module Literate =

      /// The output tokens, which can be potentially coloured.
      type LiterateToken =
        | Text | Subtext
        | Punctuation
        | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
        | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
        | MissingTemplateField

      let tokeniseRefId refId = (sprintf "$%d " refId), KeywordSymbol

      let tokeniseScalarValue (provider : IFormatProvider) (sv: obj) (format: string) =
        match sv with
        | null -> "null", StringSymbol
        | :? string as s ->
            if format = "l" then s, Subtext
            else (Utils.escapeNewlineAndQuote s), StringSymbol
        | _ ->
          // build in scalar write directly
          // else escape newline and double quote
          let formated = Utils.formatWithCustom provider sv format
          if Destructure.scalarTypeHash.Contains(sv.GetType()) then
            let token =
              match sv with
               | :? bool ->
                  KeywordSymbol
                | :? int16 | :? int32 | :? int64 | :? decimal | :? float | :? double
                | :? uint16 | :? uint32 | :? uint64 ->
                  NumericSymbol
                | :? string | :? char ->
                  StringSymbol
                | _ ->
                  OtherSymbol
            (formated, token)
            // yield formated
          else (Utils.escapeNewlineAndQuote formated), StringSymbol

      let tokeniseSequenceValueCompact (svs : TemplatePropertyValue list) (recurse : TemplatePropertyValue -> seq<string * LiterateToken>) =
        let mutable isFirst = true
        let valueTokens =
          svs
          |> List.map (fun sv -> seq {
             if not isFirst then yield ", ", Punctuation
             isFirst <- false
             yield! recurse sv
             })
          |> Seq.concat

        seq {
          yield "[", Punctuation
          yield! valueTokens
          yield "]", Punctuation
        }

      // use for formatting message template
      let rec tokenisePropValueCompact (writeState : WriteState) (tpv : TemplatePropertyValue) (format : string) =
        seq {
          match tpv with
          | RefId id ->  yield tokeniseRefId id
          | ScalarValue sv ->
            yield tokeniseScalarValue writeState.provider sv format
          | SequenceValue (refId, svs) ->
            if writeState.idManager.IsShowRefId refId then yield tokeniseRefId refId
            let recurs tpv = tokenisePropValueCompact writeState tpv null
            yield! tokeniseSequenceValueCompact svs recurs
          | StructureValue(refId, typeTag, values) ->
            if writeState.idManager.IsShowRefId refId then yield tokeniseRefId refId
            let hasAnyValues = not values.IsEmpty
            if not <| isNull typeTag then
              yield typeTag, (if hasAnyValues then Subtext else OtherSymbol)

            if hasAnyValues then
              yield " ", Subtext
              yield "{ ", Punctuation

              let lastIndex = values.Length - 1
              for i = 0 to lastIndex do
                let tp = values.[i]
                yield tp.Name, Subtext
                yield ": ", Punctuation
                yield! tokenisePropValueCompact writeState tp.Value null
                yield (if i = lastIndex then (" ", Subtext) else (", ", Punctuation))

              yield "}", Punctuation
          | DictionaryValue(refId, data) ->
            if writeState.idManager.IsShowRefId refId then yield tokeniseRefId refId
            let valueTokens =
              data
              |> List.map (fun (entryKey, entryValue) -> seq {
                 yield "(", Punctuation
                 yield! tokenisePropValueCompact writeState entryKey null
                 yield ": ", Punctuation
                 yield! tokenisePropValueCompact writeState entryValue null
                 yield ")", Punctuation
                 })
              |> Seq.concat

            if data.Length <> 1 then yield "[", Punctuation
            yield! valueTokens
            if data.Length <> 1 then yield "]", Punctuation
        }

      // use for formatting message context
      let rec tokenisePropValueIndent (writeState : WriteState) (tpv : TemplatePropertyValue) (nl : string) (depth : int) =
        // fields/gauges/other use 2 indent, depth start from 0, so 2+2 = 6
        let indent = new String (' ', depth * 2 + 4)
        seq {
          match tpv with
          | RefId id -> yield tokeniseRefId id
          | ScalarValue sv -> yield tokeniseScalarValue writeState.provider sv null
          | SequenceValue (refId, svs) ->
            if writeState.idManager.IsShowRefId refId then yield tokeniseRefId refId
            let isAllScalar = svs |> List.forall (function ScalarValue _ -> true | _ -> false)
            if isAllScalar then
              let recurs = function
                | ScalarValue sv -> tokeniseScalarValue writeState.provider sv null |> Seq.singleton
                | _ -> Seq.empty
              yield! tokeniseSequenceValueCompact svs recurs
            else
              let lastIndex = svs.Length - 1
              for i = 0 to lastIndex do
                let sv = svs.[i]
                yield nl, Text
                yield indent, Text
                yield "- ", Punctuation
                yield! tokenisePropValueIndent writeState sv nl (depth + 1)

          | StructureValue(refId, typeTag, values) ->
            if writeState.idManager.IsShowRefId refId then yield tokeniseRefId refId
            let writeTypeTag = not <| isNull typeTag
            if writeTypeTag then
              yield nl, Text
              yield indent, Text
              yield typeTag, Subtext
              yield " {", Punctuation

            let lastIndex = values.Length - 1
            for i = 0 to lastIndex do
              let nv = values.[i]
              yield nl, Text
              if writeTypeTag then yield "  ", Text
              yield indent, Text
              yield nv.Name, Subtext
              yield " => ", Punctuation
              yield! tokenisePropValueIndent writeState nv.Value nl (if writeTypeTag then depth + 2 else depth + 1)

            if writeTypeTag then yield "}", Punctuation

          | DictionaryValue(refId, kvList) ->
            if writeState.idManager.IsShowRefId refId then yield tokeniseRefId refId
            let lastIndex = kvList.Length - 1
            for i = 0 to lastIndex do
              let (entryKey, entryValue) = kvList.[i]
              match entryKey with
              | ScalarValue sv ->
                yield nl, Text
                yield indent, Text
                yield tokeniseScalarValue writeState.provider sv null
                yield " => ", Punctuation
                yield! tokenisePropValueIndent writeState entryValue nl (depth + 1)
              | _ ->
                // default case will not go to here, unless user define their own DictionaryValue which its entryKey is not ScalarValue
                yield nl, Text
                yield indent, Text
                yield "- key => ", Punctuation
                yield! tokenisePropValueIndent writeState entryKey nl (depth + 2)

                yield nl, Text
                yield indent, Text
                yield "  value => ", Punctuation
                yield! tokenisePropValueIndent writeState entryValue nl (depth + 2)
        }

      let tokeniseProperty (writeState : WriteState) (pt : Property) (pv : TemplatePropertyValue) =
        match pv with
        | ScalarValue sv ->
          let tokenised = tokeniseScalarValue writeState.provider sv pt.format

          if pt.align.isEmpty then tokenised |> Seq.singleton
          else
            let (formated, token) = tokenised
            let padded =
              match pt.align.direction with
              |  AlignDirection.Right -> formated.PadRight(pt.align.width, ' ')
              |  AlignDirection.Left -> formated.PadLeft(pt.align.width, ' ')
              | _ -> formated
            (padded, token) |> Seq.singleton
        | _ -> tokenisePropValueCompact writeState pv null

      let tokeniseTemplate (pvd: IFormatProvider) (t : Template) tryGetPropertyValue =
        t.tokens
        |> Seq.collect (function
           | TemplateToken.Text raw -> (raw, Text) |> Seq.singleton
           | TemplateToken.Property pt  ->
             match tryGetPropertyValue pt with
             | None -> (pt.ToString(), MissingTemplateField) |> Seq.singleton // not found value
             | Some pv ->
               let writeState = { provider = pvd; idManager = RefIdManager () }
               tokeniseProperty writeState pt pv
          )

      let collectAllToString (tokenised: seq<string * LiterateToken>) =
        let sb = System.Text.StringBuilder ()
        tokenised |> Seq.map fst |> Seq.iter (sb.Append >> ignore)
        sb.ToString ()

    let formatWithProvider pvd raw args =
      let tpl = parse raw
      let no _ = None
      let pvMaps = Capturing.capture tpl args |> Array.map (fun (p,v) -> p.name,v) |> Map.ofArray
      let tryGetPropertyValue (pt:Property) =
        match Map.tryFind pt.name pvMaps with
        | Some (Some v) ->

          Destructure.DestructureRequest(v, pt.captureHint.ToDestrHint(), RefIdManager())
          |> Destructure.destructure Destructure.emptyDestructureRegistry Destructure.Projection.emptyProjectionStrategy
          |> Some
        | _ -> None
      Literate.tokeniseTemplate pvd tpl tryGetPropertyValue
      |> Literate.collectAllToString

    let format raw args =
      formatWithProvider CultureInfo.CurrentCulture raw args

  type Formatting =
    static member Format (raw: string, [<ParamArray>] args: obj[]) =
      Formatting.format raw args
