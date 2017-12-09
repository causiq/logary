namespace Logary.Formatting

module FsMtParserFull =

  open System.Text

  type CaptureHint = Default = 0 | Structure = 1 | Stringify = 2
  type AlignDirection = Right = 0 | Left = 1

  [<Struct>]
  type AlignInfo =
      new (direction : AlignDirection, width : int) = { _direction=direction; _width=width; }
      new (isValid : bool) = { _direction = AlignDirection.Right; _width = (if isValid then -1 else -2) }
      val private _direction : AlignDirection
      val private _width : int
      member this.direction with get() = this._direction
      member this.width with get() = this._width
      member this.isEmpty = this.width = -1
      member internal this.isValid = this.width <> -2
      static member empty = AlignInfo(isValid=true)
      static member invalid = AlignInfo(isValid=false)

  type Property(name : string, format : string, captureHint : CaptureHint, align : AlignInfo) =
    static let emptyInstance = Property("", null, CaptureHint.Default, AlignInfo.empty)
    static member empty = emptyInstance
    member x.name = name
    member x.format = format
    member x.captureHint = captureHint
    member x.align = align
    member internal x.AppendPropertyString(sb : StringBuilder, ?replacementName) =
      sb.Append("{")
        .Append(match x.captureHint with CaptureHint.Structure -> "@" | CaptureHint.Stringify -> "$" | _ -> "")
        .Append(defaultArg replacementName name)
        .Append(match x.format with null | "" -> "" | _ -> ":" + x.format) |> ignore
      
      if not (x.align.isEmpty) then
        sb.Append(if x.align.direction = AlignDirection.Right then ",-" else ",")
          .Append(string x.align.width) |> ignore

      sb.Append("}")
    override x.ToString() = x.AppendPropertyString(StringBuilder()).ToString()

  module internal ParserBits =

    let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
    let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
    let inline isValidInAlignment c = c = '-' || System.Char.IsDigit c
    let inline isValidInCaptureHint c = c = '@' || c = '$'
    let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
    let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c || isValidInCaptureHint c

    [<Struct>]
    type Range(startIndex : int, endIndex : int) =
      member inline x.start = startIndex
      member inline x.``end`` = endIndex
      member inline x.length = (endIndex - startIndex) + 1
      member inline x.getSubstring (s : string) = s.Substring(startIndex, x.length)
      member inline x.isEmpty = startIndex = -1 && endIndex = -1
      override x.ToString() = sprintf "(start=%i, end=%i)" x.start x.``end``
      static member inline substring (s : string, startIndex, endIndex) = s.Substring(startIndex, (endIndex - startIndex) + 1)
      static member inline empty = Range(-1, -1)

    let inline tryGetFirstCharInRange predicate (s : string) (range : Range) =
      let rec go i =
        if i > range.``end`` then -1
        else if not (predicate s.[i]) then go (i+1) else i
      go range.start

    let inline tryGetFirstChar predicate (s : string) first =
      tryGetFirstCharInRange predicate s (Range(first, s.Length - 1))

    let inline hasAnyInRange predicate (s : string) (range : Range) =
      match tryGetFirstChar (predicate) s range.start with
      | -1 ->
        false
      | i ->
        i <= range.``end``

    let inline hasAny predicate (s : string) = hasAnyInRange predicate s (Range(0, s.Length - 1))
    let inline indexOfInRange s range c = tryGetFirstCharInRange ((=) c) s range
    
    /// Attemps to parse an integer from the range within a string. Returns invalidValue if the
    /// string does not contain an integer. The '-' is allowed only as the first character.
    let inline tryParseIntFromRng (invalidValue : int) (s : string) (range : Range) =
      if range.length = 1 && '0' <= s.[0] && s.[0] <= '9' then
        int (s.[range.start]) - 48
      else
        let indexOfLastCharPlus1 = range.``end``+1
        let rec inside isNeg numSoFar i =
          if i = indexOfLastCharPlus1 then
            if isNeg then -numSoFar else numSoFar
          else
            let c = s.[i]
            if c = '-' then
              if i = range.start then inside true (numSoFar) (i+1)
              else invalidValue // no '-' character allowed other than first char
            elif '0' <= c && c <= '9' then
              inside isNeg (10*numSoFar + int c - 48) (i+1)
            else invalidValue

        inside false 0 range.start

    /// Will parse ",-10"   as AlignInfo(direction=Left, width=10).
    /// Will parse ",5"     as AlignInfo(direction=Right, width=5).
    /// Will parse ",0"     as AlignInfo(isValid=false) because 0 is not a valid alignment.
    /// Will parse ",-0"    as AlignInfo(isValid=false) because 0 is not a valid alignment.
    /// Will parse ",,5"    as AlignInfo(isValid=false) because ',5' is not an int.
    /// Will parse ",5-"    as AlignInfo(isValid=false) because '-' is in the wrong place.
    /// Will parse ",asdf"  as AlignInfo(isValid=false) because 'asdf' is not an int.
    let inline tryParseAlignInfoRng (s:string) (rng:Range) =
      match s, rng with
      | s, rng when (rng.start > rng.``end``) || (hasAnyInRange (not << isValidInAlignment) s rng) ->
        AlignInfo.invalid

      | s, rng ->
        let width =
          match tryParseIntFromRng (System.Int32.MinValue) s rng with
          | System.Int32.MinValue -> 0 // not a valid align number (e.g. dash in wrong spot)
          | n -> n

        if width = 0 then AlignInfo.invalid
        else
          let isNegativeAlignWidth = width < 0
          let direction = if isNegativeAlignWidth then AlignDirection.Left else AlignDirection.Right
          AlignInfo(direction, abs(width))

    /// Attempts to validate and parse a property token within the specified range. If the property
    /// insides contains any invalid characters, then the `Property.empty' instance is returned.
    let inline tryGetPropInRange (template : string) (within : Range) : Property =
      let nameRange, alignRange, formatRange =
        match indexOfInRange template within ',', indexOfInRange template within ':' with
        | -1, -1 ->
          // neither align nor format
          within, Range.empty, Range.empty

        | -1, fmtIdx ->
          // has format part, but does not have align part
          Range(within.start, fmtIdx-1), Range.empty, Range(fmtIdx+1, within.``end``)

        | alIdx, -1 ->
          // has align part, but does not have format part
          Range(within.start, alIdx-1), Range(alIdx+1, within.``end``), Range.empty

        | alIdx, fmtIdx when alIdx < fmtIdx && alIdx <> (fmtIdx-1) ->
          // has both align and format parts, in the correct order
          let align = Range(alIdx+1, fmtIdx-1)
          let fmt = Range(fmtIdx+1, within.``end``)
          Range(within.start, alIdx-1), align, fmt

        | alIdx, fmtIdx when alIdx > fmtIdx ->
          // has format part, no align (but one or more commas *inside* the format string)
          Range(within.start, fmtIdx-1), Range.empty, Range(fmtIdx+1, within.``end``)

        | _, _ ->
          Range.empty, Range.empty, Range.empty

      if nameRange.isEmpty then
        Property.empty // property name is empty
      else
        let maybeCaptureHintChar = template.[nameRange.start]
        let propertyNameStartIndex, captureHint =
          match maybeCaptureHintChar with
          | '@' -> nameRange.start+1, CaptureHint.Structure
          | '$' -> nameRange.start+1, CaptureHint.Stringify
          | _ -> nameRange.start, CaptureHint.Default

        let propertyName = Range.substring (template, propertyNameStartIndex, nameRange.``end``)
        if propertyName = "" || (hasAny (not<<isValidInPropName) propertyName) then
          // property name has invalid characters
          Property.empty
        else
          if (not formatRange.isEmpty) && (hasAnyInRange (not<<isValidInFormat) template formatRange) then
            Property.empty // format range has invalid characters
          else
            match alignRange.isEmpty, formatRange.isEmpty with
            | true, true -> Property(propertyName, null, captureHint, AlignInfo.empty)
            | true, false -> Property(propertyName, formatRange.getSubstring template, captureHint, AlignInfo.empty)
            | false, _ ->
              let formatString = if formatRange.isEmpty then null else formatRange.getSubstring template
              match tryParseAlignInfoRng template alignRange with
              | ai when ai.isValid -> Property(propertyName, formatString, captureHint, ai)
              | _ -> Property.empty // align has invalid characters

    let inline findNextNonPropText (startAt : int) (template : string) (foundText : string->unit) : int =
      // Finds the next text token (starting from the 'startAt' index) and returns the next character
      // index within the template string. If the end of the template string is reached, or the start
      // of a property token is found (i.e. a single { character), then the 'consumed' text is passed
      // to the 'foundText' method, and index of the next character is returned.
      let mutable escapedBuilder = Unchecked.defaultof<StringBuilder> // don't create one until it's needed
      let inline append (ch : char) = if not (isNull escapedBuilder) then escapedBuilder.Append(ch) |> ignore
      let inline createStringBuilderAndPopulate i =
        if isNull escapedBuilder then
          escapedBuilder <- StringBuilder() // found escaped open-brace, take the slow path
          for chIndex = startAt to i-1 do append template.[chIndex] // append all existing chars
      let rec go i =
        if i >= template.Length then
          template.Length // bail out at the end of the string
        else
          let ch = template.[i]
          match ch with
          | '{' ->
            if (i+1) < template.Length && template.[i+1] = '{' then
              createStringBuilderAndPopulate i
              append ch; go (i+2)
            else i // found an open brace (potentially a property), so bail out
          | '}' when (i+1) < template.Length && template.[i+1] = '}' ->
            createStringBuilderAndPopulate i
            append ch; go (i+2)
          | _ ->
            append ch; go (i+1)

      let nextIndex = go startAt
      if (nextIndex > startAt) then // if we 'consumed' any characters, signal that we 'foundText'
        if isNull escapedBuilder then
          foundText (Range.substring(template, startAt, nextIndex - 1))
        else
          foundText (escapedBuilder.ToString())
      nextIndex

    let findPropOrText (start : int) (template : string)
                        (foundText : string -> unit)
                        (foundProp : Property -> unit) : int =
      // Attempts to find the indices of the next property in the template
      // string (starting from the 'start' index). Once the start and end of
      // the property token is known, it will be further validated (by the
      // tryGetPropInRange method). If the range turns out to be invalid, it's
      // not a property token, and we return it as text instead. We also need
      // to handle some special case here: if the end of the string is reached,
      // without finding the close brace (we just signal 'foundText' in that case).
      let nextInvalidCharIndex =
        match tryGetFirstChar (not << isValidCharInPropTag) template (start+1) with
        | -1 ->
          template.Length
        | idx ->
          idx

      if nextInvalidCharIndex = template.Length || template.[nextInvalidCharIndex] <> '}' then
        foundText (Range.substring(template, start, (nextInvalidCharIndex - 1)))
        nextInvalidCharIndex
      else
        let nextIndex = nextInvalidCharIndex + 1
        let propInsidesRng = Range(start + 1, nextIndex - 2)
        match tryGetPropInRange template propInsidesRng with
        | prop when not (obj.ReferenceEquals(prop, Property.empty)) ->
          foundProp prop
        | _ ->
          foundText (Range.substring(template, start, (nextIndex - 1)))
        nextIndex

  /// Parses template strings such as "Hello, {PropertyWithFormat:##.##}"
  /// and calls the 'foundTextF' or 'foundPropF' functions as the text or
  /// property tokens are encountered.
  let parseParts (template : string) foundTextF foundPropF =
    let tlen = template.Length
    let rec go start =
      if start >= tlen then ()
      else match ParserBits.findNextNonPropText start template foundTextF with
            | next when next <> start ->
              go next
            | _ ->
              go (ParserBits.findPropOrText start template foundTextF foundPropF)
    go 0

module MessageTemplate =

  open System
  open System.Globalization
  open FsMtParserFull

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

  type TemplatePropertyValue =
    | RefId of int64
    | ScalarValue of obj
    | SequenceValue of refId:int64 * TemplatePropertyValue list
    | StructureValue of refId:int64 * typeTag:string * values:PropertyNameAndValue list
    | DictionaryValue of refId:int64 * data: (TemplatePropertyValue * TemplatePropertyValue) list
  and PropertyNameAndValue =
    { Name:string; Value:TemplatePropertyValue }

  let parseToTemplate raw =
    let tokens = ResizeArray<TemplateToken>()
    let foundText text = tokens.Add(Text text)
    let foundProp prop = tokens.Add(Property prop)
    parseParts raw foundText foundProp
    {
      raw = raw
      tokens = tokens.ToArray()
    }

  module Destructure =

    open System.Collections.Generic
    open System.Runtime.Serialization
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Microsoft.FSharp.Quotations.Patterns

    
    type RefIdManager () = 
      let showAsIdSet = new HashSet<int64> ()
      let idGen = new ObjectIDGenerator ()

      member __.ShowAsRefId id = 
        do showAsIdSet.Add id |> ignore
        RefId id
      member __.GetRefId data = idGen.GetId data
      member __.IsShowRefId id = showAsIdSet.Contains id

    [<NoEquality;NoComparison>]
    type DestructureRequest =
      {
        value : obj
        hint  : CaptureHint
        idManager : RefIdManager
        destructurer : Destructurer
      }
    with
      member __.TryAgain (data) =
        __.destructurer {__ with value = data}
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
             | [onlyOne] -> hieraMap |> Map.add onlyOne [] 
             | head :: rests  -> 
               match hieraMap |> Map.tryFind head with
               | Some byKey -> 
                 hieraMap |> Map.add head (rests :: byKey)
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
    
    let someRefId = RefId >> Some
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

    let private isDictionable (t: Type) =
      t.GetInterfaces()
      |> Seq.exists (fun iface -> 
         if iface.IsGenericType && iface.GetGenericTypeDefinition() = typedefof<IEnumerable<_>> then
           let elementType = iface.GetGenericArguments().[0]
           elementType.IsGenericType && elementType.GetGenericTypeDefinition() = typedefof<KeyValuePair<_,_>>
         else iface = typeof<System.Collections.IDictionary>)

    let inline tryContainerTypes req =
      let instance = req.value
      let refCount = req.idManager
      let (refId, firstOccurence) = refCount.GetRefId instance
      if not firstOccurence then refCount.ShowAsRefId refId |> Some
      else
        match instance.GetType() with
        | t when FSharpType.IsTuple t ->
          let tupleValues =
              instance
              |> FSharpValue.GetTupleFields
              |> Seq.map req.TryAgain
              |> Seq.toList
          someSequence (refId, tupleValues)

        | t when isDictionable t ->

          let keyProp, valueProp = ref Unchecked.defaultof<PropertyInfo>, ref Unchecked.defaultof<PropertyInfo>
          let getKey o = if isNull !keyProp  then keyProp := o.GetType().GetRuntimeProperty("Key")
                         (!keyProp).GetValue(o)
          let getValue o = if isNull !valueProp then valueProp := o.GetType().GetRuntimeProperty("Value")
                           (!valueProp).GetValue(o)
          let objEnumerable = instance :?> System.Collections.IEnumerable |> Seq.cast<obj>
          let skvps = objEnumerable
                      |> Seq.map (fun o ->  req.TryAgain (getKey o),  req.TryAgain (getValue o))
                      |> Seq.toList
          someDictionary (refId, skvps)

        | t when FSharpType.IsUnion t ->
          let case, fields = FSharpValue.GetUnionFields(instance, t)
          let caseName = ScalarValue case.Name
          match fields with
          | [||] -> Some caseName
          | [| oneField |] ->
            let oneValue = req.TryAgain oneField
            someDictionary (refId, [caseName, oneValue])
          | _ ->
            let fieldsValue = fields |> Array.map req.TryAgain |> Array.toList
            let (fieldsRefId, fieldsFirstOccurence) = refCount.GetRefId instance
            let fieldsPropValue = 
              if not fieldsFirstOccurence then refCount.ShowAsRefId fieldsRefId
              else SequenceValue (fieldsRefId, fieldsValue)
            someDictionary (refId, [caseName, fieldsPropValue])
        | _ -> 
          match instance with
          | :? System.Collections.IEnumerable as e -> 
            let eles = e |> Seq.cast<obj> |> Seq.map req.TryAgain |> Seq.toList
            someSequence (refId, eles)
          | _ -> None

    let inline tryDefault req =
      tryScalar |? tryContainerTypes <| req

    let inline getCanReadProp (t: Type) name =
      let lookup = BindingFlags.Public ||| BindingFlags.Instance
      match t.GetProperty(name,lookup,null,null,Array.empty,null) with
      | null -> None
      | p when p.CanRead && (p.Name <> "Item" || p.GetIndexParameters().Length = 0) -> 
        Some p
      | _ -> None

    let rec reflectionProperties (tryProjection : ProjectionStrategy) (req : DestructureRequest) =
      let ty = req.value.GetType()
      match tryProjection ty with
      | None -> reflectionByProjection (Except (List.empty)) req // try reflection all
      | Some how -> reflectionByProjection how req
    and reflectionByProjection how req =
  

      let instance = req.value
      let refCount = req.idManager
      let (refId, firstOccurence) = refCount.GetRefId instance
      if not firstOccurence then refCount.ShowAsRefId refId
      else
        let ty = instance.GetType()
        let typeTag = ty.Name

        let rzPubProps = ResizeArray<PropertyInfo>()
        
        match how with
        | Except names -> failwith ""
          // let top = Projection.TopHierarchy names

        | Only names ->
          let nvs =
            names 
            |> Projection.TopHierarchy
            |> Map.toSeq
            |> Seq.fold (fun nvs (name, childNames) ->
               match getCanReadProp ty name with
               | None -> nvs
               | Some pInfo ->
                 let nv =
                   try
                     let propValue = pInfo.GetValue(instance)
                     let childHow = Only childNames
                     let tpv = req.TryAgain propValue
                     { Name = pInfo.Name; Value = tpv }
                   with
                   | ex -> 
                     let tpv = ScalarValue ("The property accessor threw an exception:" + ex.Message)
                     { Name = pInfo.Name; Value = tpv } 
                  in 
                  nv :: nvs
              ) List.empty

          StructureValue (refId, typeTag, nvs)

    and destructurerChild childHow childReq =
      match tryDefault childReq with
        | Some tpv -> tpv
        | _ -> reflectionByProjection childHow childReq
        // for rtp in ty.GetProperty() do
        //   if isPublicInstanceReadProp rtp then rzPubProps.Add rtp

        // // Recursively destructure the child properties
        // let rec loopDestrChildren i acc =
        //   if i < 0 then acc
        //   else
        //     let pi = rzPubProps.[i]
        //     try
        //       let propValue = pi.GetValue(value)
        //       let propTpv = { Name=pi.Name; Value=req.TryAgain propValue }
        //       loopDestrChildren (i-1) (propTpv :: acc)
        //     with
        //     | :? TargetParameterCountException as ex ->
        //         // r.Log("The property accessor {0} is a non-default indexer", [|pi|])
        //         loopDestrChildren (i-1) (acc)
        //     | :? TargetInvocationException as ex ->
        //         // r.Log("The property accessor {0} threw exception {1}", [| pi; ex; |])
        //         let propValue = "The property accessor threw an exception:" + ex.InnerException.GetType().Name
        //         let propTpv = { Name=pi.Name; Value=req.TryAgain propValue }
        //         loopDestrChildren (i-1) (propTpv :: acc)

        // let childStructureValues = loopDestrChildren (rzPubProps.Count-1) []
        // StructureValue (refId, typeTag, childStructureValues)

    // | :? Gauge as gauge, _ ->
    //   let (Gauge (value, units)) = gauge
    //   let (scaledValue, unitsFormat) = Units.scale units value
    //   if String.IsNullOrEmpty unitsFormat then ScalarValue scaledValue
    //   else ScalarValue (sprintf "%s %s" (string scaledValue) unitsFormat)
    let generatePropValue (tryCustom: DestructurerStrategy) 
                          (tryProjection: ProjectionStrategy)
                          (req: DestructureRequest) 
                          : TemplatePropertyValue =
      let tryNullOrCustom = tryNull |? tryCustom
      match tryNullOrCustom req with
      | Some tpv -> tpv
      | _ ->
        match req.hint with
        | CaptureHint.Stringify -> ScalarValue (string req.value)
        | CaptureHint.Structure -> reflectionProperties tryProjection req 
        | _ ->
          match tryDefault req with
          | Some tpv -> tpv
          | _ -> ScalarValue (req.value)


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

    let formatWithProvider (provider : IFormatProvider) (arg : obj) format =
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
        let (newline, newlineReplaced) =
          let origin = Environment.NewLine
          if origin = "\n" then (origin, @"\n")
          else (origin, @"\r\n")

        let escape = str.Replace("\"","\\\"").Replace(newline,newlineReplaced)
        "\"" + escape + "\""

    module Literate =

      /// The output tokens, which can be potentially coloured.
      type LiterateToken =
        | Text | Subtext
        | Punctuation
        | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
        | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
        | MissingTemplateField

      let tokeniseScalarValue (provider : IFormatProvider) (sv: obj) (format: string) =
        match sv with
        | null -> "null", StringSymbol
        | :? string as s ->
            if format = "l" then s, Subtext
            else (escapeNewlineAndQuote s), StringSymbol
        | _ ->
          // build in scalar write directly
          // else escape newline and double quote
          let formated = formatWithProvider provider sv format
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
          else (escapeNewlineAndQuote formated), StringSymbol

      let tokeniseSequenceValueCompact (provider : IFormatProvider) (svs: TemplatePropertyValue list) recurse =
        let mutable isFirst = true
        let valueTokens =
          svs
          |> List.map (fun sv -> seq {
             if not isFirst then yield ", ", Punctuation
             isFirst <- false
             yield! recurse provider sv
             })
          |> Seq.concat

        seq {
          yield "[", Punctuation
          yield! valueTokens
          yield "]", Punctuation
        }

      // use for formatting message template
      let rec tokenisePropValueCompact (provider : IFormatProvider) (tpv: TemplatePropertyValue) (format: string) =
        seq {
          match tpv with
          | RefId id -> yield (string id), KeywordSymbol
          | ScalarValue sv ->
            yield tokeniseScalarValue provider sv format
          | SequenceValue (refId, svs) ->
            let recurs provider tpv = tokenisePropValueCompact provider tpv null
            yield! tokeniseSequenceValueCompact provider svs recurs
          | StructureValue(refId, typeTag, values) ->
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
                yield! tokenisePropValueCompact provider tp.Value null
                yield (if i = lastIndex then (" ", Subtext) else (", ", Punctuation))

              yield "}", Punctuation
          | DictionaryValue(refId, data) ->
            let valueTokens =
              data
              |> List.map (fun (entryKey, entryValue) -> seq {
                 yield "(", Punctuation
                 yield! tokenisePropValueCompact provider entryKey null
                 yield ": ", Punctuation
                 yield! tokenisePropValueCompact provider entryValue null
                 yield ")", Punctuation
                 })
              |> Seq.concat

            yield "[", Punctuation
            yield! valueTokens
            yield "]", Punctuation
        }

      // use for formatting message context
      let rec tokenisePropValueIndent (provider : IFormatProvider) (tpv: TemplatePropertyValue) (nl: string) (depth: int) =
        // fields/gauges/other use 2 indent, depth start from 0, so 2+2 = 6
        let indent = new String (' ', depth * 2 + 4)
        seq {
          match tpv with
          | RefId id -> yield (string id), KeywordSymbol
          | ScalarValue sv -> yield tokeniseScalarValue provider sv null

          | SequenceValue (refId, svs) ->
            let isAllScalar = svs |> List.forall (function ScalarValue _ -> true | _ -> false)
            if isAllScalar then
              let recurs provider = function 
                | ScalarValue sv -> tokeniseScalarValue provider sv null |> Seq.singleton
                | _ -> Seq.empty
              yield! tokeniseSequenceValueCompact provider svs recurs
            else
              let lastIndex = svs.Length - 1
              for i = 0 to lastIndex do
                let sv = svs.[i]
                yield nl, Text
                yield indent, Text
                yield "- ", Punctuation
                yield! tokenisePropValueIndent provider sv nl (depth + 1)

          | StructureValue(refId, typeTag, values) ->
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
              yield! tokenisePropValueIndent provider nv.Value nl (if writeTypeTag then depth + 2 else depth + 1)

            if writeTypeTag then yield "}", Punctuation

          | DictionaryValue(refId, kvList) ->
            let lastIndex = kvList.Length - 1
            for i = 0 to lastIndex do
              let (entryKey, entryValue) = kvList.[i]
              match entryKey with
              | ScalarValue sv ->
                yield nl, Text
                yield indent, Text
                yield tokeniseScalarValue provider sv null
                yield " => ", Punctuation
                yield! tokenisePropValueIndent provider entryValue nl (depth + 1)
              | _ ->
                // default case will not go to here, unless user define their own DictionaryValue which its entryKey is not ScalarValue
                yield nl, Text
                yield indent, Text
                yield "- key => ", Punctuation
                yield! tokenisePropValueIndent provider entryKey nl (depth + 2)

                yield nl, Text
                yield indent, Text
                yield "  value => ", Punctuation
                yield! tokenisePropValueIndent provider entryValue nl (depth + 2)
        }

      let tokeniseProperty (provider: IFormatProvider) (pt: Property) (pv: TemplatePropertyValue) =
        if Destructure.isEmptyKeepTrying pv then
          (pt.ToString(), MissingTemplateField) |> Seq.singleton
        else
          match pv with
          | ScalarValue sv ->
            let tokenised = tokeniseScalarValue provider sv pt.Format

            if pt.Align.IsEmpty then tokenised |> Seq.singleton
            else
              let (formated, token) = tokenised
              let padded =
                match pt.Align.Direction with
                |  Direction.Right -> formated.PadRight(pt.Align.Width, ' ')
                |  Direction.Left -> formated.PadLeft(pt.Align.Width, ' ')
                | _ -> formated
              (padded, token) |> Seq.singleton
          | _ -> tokenisePropValueCompact provider pv null

      let tokeniseTemplate (t:Template) provider getValueByName =
        t.Tokens
        |> Seq.collect (function
           | Token.TextToken (_, raw) -> (raw, Text) |> Seq.singleton
           | Token.PropToken (_, pd)  -> tokeniseProperty provider pd (getValueByName pd.Name))
