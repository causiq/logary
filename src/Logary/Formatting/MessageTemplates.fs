namespace Logary

module MessageTemplates =

  open System
  open System.Globalization
  open System.Collections.Generic
  open System.Runtime.Serialization
  open Logary.Internals.FsMtParserFull

  type DestrHint = Default | Structure  | Stringify
  module DestrHint =
    let fromCaptureHint = function 
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

  /// consider only cover cycle reference, not multi reference, cycle reference show null, then no need refId
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

    member __.TryShowAsRefId (data : obj) =
      match idGen.GetId data with
      | refId, false -> 
        do showAsIdSet.Add refId |> ignore
        refId, RefId refId |> Some
      | refId, true -> refId, None

    /// consider only cover cycle reference, not multi reference, cycle reference show null, then no need refId
    member __.IsShowRefId id = showAsIdSet.Contains id

  let parseToTemplate raw =
    let tokens = ResizeArray<TemplateToken>()
    let foundText text = tokens.Add(TemplateToken.Text text)
    let foundProp prop = tokens.Add(TemplateToken.Property prop)
    parseParts raw foundText foundProp
    {
      raw = raw
      tokens = tokens.ToArray()
    }

  module Destructure =

    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations.Patterns

    [<NoEquality;NoComparison>]
    type DestructureRequest =
      {
        value : obj
        hint  : DestrHint
        idManager : RefIdManager
      }
    and Destructurer = DestructureRequest -> TemplatePropertyValue
    and DestructurerStrategy = DestructureRequest -> TemplatePropertyValue option

    let only<'t> (proj: 't -> obj[]) = ()
    let except<'t> (proj: 't -> obj[]) = ()

    type Projection =
    | Projection of Type * How
    | NotSupport
    with
      (*
        Projection
          ("Exception",
           Only
             [["Message"]; ["StackTrace"]; ["Data"]; ["InnerException"]; ["InnerException"; "Message"];
              ["InnerException"; "StackTrace"]]
              )
      *)
      static member TopHierarchy (names: string list list) =
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
    and How =
    | Only of string list list
    | Except of string list list
    and ProjectionStrategy = Type -> How option

    module Projection =

      let rec byExpr expr =
        match expr with
        | Call (_, method, [Lambda( d, NewArray(_, props))]) -> Projection (d.Type, generateHow method.Name props)
        | _ -> NotSupport
      and generateHow methodName exprs =
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
      and generateOutter outterExpr innerProjs =
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

    let inline private (|?) (f: DestructurerStrategy) (g: DestructurerStrategy) : DestructurerStrategy =
      fun req ->
        match f req with
        | Some _ as d -> d
        | None -> g req

    let someScalar = ScalarValue >> Some
    let someSequence = SequenceValue >> Some
    let someStructure = StructureValue >> Some
    let someDictionary = DictionaryValue >> Some

    let inline tryNull req =
      if isNull req.value then someScalar null
      else None

    let inline tryBuiltInTypes req =
      if scalarTypeHash.Contains(req.value.GetType()) then
        someScalar req.value
      else None

    let inline tryEnum req =
      match req.value with
      | :? Enum as e -> someScalar e
      | _ -> None

    let inline tryByteArrayMaxBytes maxBytes req =
      match req.value with
      | :? array<Byte> as bytes ->
        if bytes.Length <= maxBytes then someScalar bytes
        else
          let inline toHexString (b:byte) = b.ToString("X2")
          let start = bytes |> Seq.take maxBytes |> Seq.map toHexString |> String.Concat
          let description = start + "... (" + string bytes.Length + " bytes)"
          someScalar description
      | _ -> None

    let inline tryByteArray req = tryByteArrayMaxBytes 1024 req

    let inline tryReflectionTypes req =
        match req.value with
        | :? Type as t -> someScalar t
        | :? MemberInfo as m -> someScalar m
        | _ -> None

    let inline tryScalar req =
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


    let inline tryContainerTypes (destr: Destructurer) req =
      let destr o = destr {req with value = o}
      let instance = req.value
      let refCount = req.idManager
  
      match instance.GetType() with
      | t when FSharpType.IsTuple t ->
        match refCount.TryShowAsRefId instance with
        | _, Some pv  -> Some pv
        | refId, None -> 
          let tupleValues =
              instance
              |> FSharpValue.GetTupleFields
              |> Seq.map destr
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
                      |> Seq.map (fun o ->  destr (getKey o),  destr (getValue o))
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
            someDictionary (refId, [caseName, destr oneField])
        | _ ->
          match refCount.TryShowAsRefId instance with
          | _, Some pv  -> Some pv
          | refId, None -> 
            someDictionary (refId, [caseName, destr fields])
      | _ ->
        match instance with
        | :? System.Collections.IEnumerable as e ->
          match refCount.TryShowAsRefId instance with
          | _, Some pv  -> Some pv
          | refId, None -> 
            let eles = e |> Seq.cast<obj> |> Seq.map destr |> Seq.toList
            someSequence (refId, eles)
        | _ -> None

    let lookupPropFlags = BindingFlags.Public ||| BindingFlags.Instance

    let inline private tryGetValueWithProp (propInfo: PropertyInfo) instance =
      match propInfo with
      | p when (not <| isNull p) 
               && p.CanRead 
               && (p.Name <> "Item" || p.GetIndexParameters().Length = 0) ->
        let propValue = 
          try p.GetValue(instance) 
          with 
          | :? TargetInvocationException as ex ->  ("The property accessor threw an exception: " + ex.InnerException.Message) |> box
          | ex -> ("The property accessor threw an exception: " + ex.Message) |> box
        Some propValue
      | _ -> None

    let inline private tryGetValueWithType (t: Type) name instance =
      t.GetProperty(name,lookupPropFlags,null,null,Array.empty,null) |> tryGetValueWithProp <| instance

    let rec reflectionProperties (tryProjection : ProjectionStrategy) (destr : Destructurer) (req : DestructureRequest) =
      let ty = req.value.GetType()
      match tryProjection ty with
      | None -> reflectionByProjection destr (Except []) req // try reflection all
      | Some how -> reflectionByProjection destr how req
    and reflectionByProjection destr how req =
      let destrChild childHow childInstance=
        match childHow with
        | Only ([])
        | Except ([]) ->
          // means no children names, so try with destr passed in
          destr { req with value = childInstance }
        | Only _
        | Except _ ->
          // use childHow ignore custom type projection
          reflectionByProjection destr childHow { req with value = childInstance }

      let instance = req.value
      let refCount = req.idManager
      
      if not <| isNull instance then 
        match refCount.TryShowAsRefId instance with
        | _, Some pv  -> pv
        | refId, None -> 
          let ty = instance.GetType()
          let typeTag = ty.Name

          match how with
          | Except names -> 
            let topHieraMap = Projection.TopHierarchy names
            let isInclude name =
              // in except hieraMap and has no childNames
              match topHieraMap |> Map.tryFind name with
              | Some [] -> false
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
                     | Some childNames -> Except childNames
                     | None -> Except []
                   let tpv = if isNull propValue then ScalarValue null else destrChild childHow propValue
                   { Name = name; Value = tpv } :: nvs
                 ) List.empty

            StructureValue (refId, typeTag, nvs)

          | Only names ->
            let nvs =
              names
              |> Projection.TopHierarchy
              |> Map.toList
              |> List.fold (fun nvs (name, childNames) ->
                 match tryGetValueWithType ty name instance with
                 | None -> nvs
                 | Some propValue ->
                   let childHow = Only childNames
                   let tpv = if isNull propValue then ScalarValue null else destrChild childHow propValue
                   { Name = name; Value = tpv } :: nvs
                 ) List.empty

            StructureValue (refId, typeTag, nvs)
      else ScalarValue null

    let generatePropValue (tryCustom: DestructurerStrategy)
                          (tryProjection: ProjectionStrategy)
                          (req: DestructureRequest)
                          : TemplatePropertyValue =
      match tryNull req with
      | Some tpv -> tpv
      | _ ->
        match req.hint with
        | DestrHint.Stringify -> ScalarValue (req.value.ToString())
        | DestrHint.Structure ->
          let rec defaultOrReflection req =
            let tryDefault = tryNull |? tryCustom |? tryScalar |? (tryContainerTypes defaultOrReflection)
            match tryDefault req with
            | Some tpv -> tpv
            | _ -> reflectionProperties tryProjection defaultOrReflection req
          defaultOrReflection req
        | DestrHint.Default ->
          let rec defaultOrScalar req =
            let tryDefault = tryNull |? tryCustom |? tryScalar |? (tryContainerTypes defaultOrScalar)
            match tryDefault req with
            | Some tpv -> tpv
            | _ -> ScalarValue (req.value)

          defaultOrScalar req

  module Capturing =

    let inline private capturePositionals (props:Property[]) (args:obj[]) =
      props
      |> Array.mapi (fun i prop ->
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

  module Formatting = 
    open Destructure

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

      let tokeniseSequenceValueCompact (svs: TemplatePropertyValue list) recurse =
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
      let rec tokenisePropValueCompact (writeState : WriteState) (tpv: TemplatePropertyValue) (format: string) =
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
      let rec tokenisePropValueIndent (writeState : WriteState) (tpv: TemplatePropertyValue) (nl: string) (depth: int) =
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

      let tokeniseProperty (writeState : WriteState) (pt: Property) (pv: TemplatePropertyValue) =
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
      let tpl = parseToTemplate raw
      let no _ = None
      let pvMaps = Capturing.capture tpl args |> Array.map (fun (p,v) -> p.name,v) |> Map.ofArray
      let tryGetPropertyValue (pt:Property) =
        match Map.tryFind pt.name pvMaps with
        | Some (Some v) ->
          let req = {value = v; hint = (DestrHint.fromCaptureHint pt.captureHint); idManager = RefIdManager()}
          Destructure.generatePropValue no no req
          |> Some
        | _ -> None
      Literate.tokeniseTemplate pvd tpl tryGetPropertyValue
      |> Literate.collectAllToString

    let format raw args =
      formatWithProvider CultureInfo.CurrentCulture raw args

  type Formatting =
    static member Format (raw: string, [<ParamArray>] args: obj[]) =
      Formatting.format raw args
