namespace Logary

open System
open System.Globalization
open System.Text
open System.IO
open NodaTime
open Logary
open Logary.Internals.FsMessageTemplates

module internal LiterateFormatting =

  module Tokens =
    /// The output tokens, which can be potentially coloured.
    type LiterateToken =
      | Text | Subtext
      | Punctuation
      | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
      | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
      | MissingTemplateField

  open Tokens

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

  module internal FsMessageTemplates =

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
        | ScalarValue sv ->
          yield tokeniseScalarValue provider sv format
        | SequenceValue svs ->
          let recurs provider tpv = tokenisePropValueCompact provider tpv null
          yield! tokeniseSequenceValueCompact provider svs recurs
        | StructureValue(typeTag, values) ->
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
        | DictionaryValue(data) ->
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
      // context: use 2 indent, fields/gauges/other use 2 indent, depth start from 0, so 2+2+2 = 6
      let indent = new String (' ', depth * 2 + 6)
      seq {
        match tpv with
        | ScalarValue sv -> yield tokeniseScalarValue provider sv null

        | SequenceValue svs ->
          let isAllScalar = svs |> List.forall (function ScalarValue _ -> true | _ -> false)
          if isAllScalar then
            let recurs provider tpv = tokeniseScalarValue provider tpv null |> Seq.singleton
            yield! tokeniseSequenceValueCompact provider svs recurs
          else
            let lastIndex = svs.Length - 1
            for i = 0 to lastIndex do
              let sv = svs.[i]
              yield nl, Text
              yield indent, Text
              yield "- ", Punctuation
              yield! tokenisePropValueIndent provider sv nl (depth + 1)

        | StructureValue(typeTag, values) ->
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

        | DictionaryValue(kvList) ->
          let lastIndex = kvList.Length - 1
          for i = 0 to lastIndex do
            let (entryKey, entryValue) = kvList.[i]
            match entryKey with
            | ScalarValue _ ->
              yield nl, Text
              yield indent, Text
              yield tokeniseScalarValue provider entryKey null
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

  module internal MessageParts =
    open Microsoft.FSharp.Reflection

    /// Returns the case name of the object with union type 'ty.
    let caseNameOf (x:'a) =
      match FSharpValue.GetUnionFields(x, typeof<'a>) with
      | case, _ -> case.Name

    /// Format a timestamp in nanoseconds since epoch into a ISO8601 string
    let formatTimestamp (ticks : int64) =
      Instant.FromTicksSinceUnixEpoch(ticks)
        .ToDateTimeOffset()
        .ToString("o", CultureInfo.InvariantCulture)

    let tokeniseTemplateByGauges (pvd: IFormatProvider) (gauges : List<string * Gauge>) =
      if gauges.Length = 0 then Seq.empty
      else
        seq {
          yield "Gauges: ", Text
          yield "[", Punctuation

          let lastIndex = gauges.Length - 1
          for i=0 to lastIndex do
            let (gaugeType, Gauge (value, units)) = gauges.[i]
            let (scaledValue, unitsFormat) = Units.scale units value
            let valueFormated = formatWithProvider pvd scaledValue null
            yield gaugeType, Subtext
            yield " : ", Punctuation
            yield valueFormated, NumericSymbol
            if not <| String.IsNullOrEmpty unitsFormat then
              yield " ", Subtext
              yield unitsFormat, Text
            if i <> lastIndex then yield ", ", Punctuation

          yield "]", Punctuation
        }

    let tokeniseTemplateByFields (pvd: IFormatProvider) (template : Template)
                                          (destr : Destructurer) (maxDepth : int)
                                          (fields : seq<string * obj>) =
      let fieldsMap = fields |> HashMap.ofSeq
      let propertiesMap = template.Properties |> Seq.map (fun p -> (p.Name, p.Destr)) |> HashMap.ofSeq
      let getValueByName name =
        match (HashMap.tryFind name fieldsMap, HashMap.tryFind name propertiesMap) with
        | Some (value), Some (destrHint) ->
          destr (DestructureRequest(destr, value, maxDepth, 1, hint=destrHint))
        | _ -> TemplatePropertyValue.Empty
      FsMessageTemplates.tokeniseTemplate template pvd getValueByName

    let tokeniseTemplateWithGauges pvd nl destr maxDepth message =
      let tplByGauges =
        message |> Message.getAllGauges |> List.ofSeq |> tokeniseTemplateByGauges pvd |> List.ofSeq

      let (Event (formatTemplate)) = message.value

      if String.IsNullOrEmpty formatTemplate then tplByGauges |> Seq.ofList
      else
        let parsedTemplate = Parser.parse (formatTemplate)
        let tplByFields =
          message |> Message.getAllFields |> tokeniseTemplateByFields pvd parsedTemplate destr maxDepth
        if List.isEmpty tplByGauges then tplByFields
        else seq { yield! tplByFields; yield " ", Text; yield! tplByGauges}

    let tokeniseContext (pvd: IFormatProvider) (nl: string) (destr: Destructurer) maxDepth message =
      let padding = new String (' ', 4)

      let inline processKvs (pvd: IFormatProvider) (prefix: string) (nl: string) (kvs: seq<string * obj * DestrHint>) =
        let valuesToken =
          kvs
          |> Seq.collect (fun (name, value, destrHint) -> seq {
             yield nl, Text
             yield padding, Text
             yield name, Subtext
             yield " => ", Punctuation
             let destrValue = destr (DestructureRequest(destr, value, maxDepth, 1, hint= destrHint))
             yield! FsMessageTemplates.tokenisePropValueIndent pvd destrValue nl 1
             })
          |> List.ofSeq

        if not <| List.isEmpty valuesToken then
          [
            yield nl, Text
            yield prefix, Text
            yield! valuesToken
          ]
        else List.empty

      // process fields
      let (Event (formatTemplate)) = message.value
      let fieldsPropInTemplate =
        if not <| String.IsNullOrEmpty formatTemplate then
          let parsedTemplate = Parser.parse (formatTemplate)
          parsedTemplate.Properties |> Seq.map (fun prop -> prop.Name, prop) |> HashMap.ofSeq
        else HashMap.empty

      let fields =
        message
        |> Message.getAllFields
        |> Seq.map (fun (name, value) ->
           match HashMap.tryFind name fieldsPropInTemplate with
           | None -> (name, value, DestrHint.Destructure)
           | Some prop -> (name, value, prop.Destr))
        |> processKvs pvd "  fields:" nl

      // process gauge
      let gauges =
        message |> Message.getAllGauges
        |> Seq.map (fun (k, gauge) -> (k, box gauge, DestrHint.Destructure))
        |> processKvs pvd "  gauges:" nl

      // process others
      let others =
        message |> Message.getContextsOtherThanGaugeAndFields
        |> Seq.map (fun (k, v) -> (k, v, DestrHint.Destructure))
        |> processKvs pvd "  others:" nl

      [fields; gauges; others] |> Seq.concat

    let rec formatValueLeafs (ns : string list) (value : Value) =
      let rns = lazy (PointName.ofList (List.rev ns))
      seq {
        match value with
        | String s ->
          yield rns.Value, s
        | Bool b ->
          yield rns.Value, b.ToString()
        | Float f ->
          yield rns.Value, f.ToString()
        | Int64 i ->
          yield rns.Value, i.ToString ()
        | BigInt b ->
          yield rns.Value, b.ToString ()
        | Binary (b, _) ->
          yield rns.Value, BitConverter.ToString b |> fun s -> s.Replace("-", "")
        | Fraction (n, d) ->
          yield rns.Value, sprintf "%d/%d" n d
        | Array list ->
          for item in list do
            yield! formatValueLeafs ns item
        | Object m ->
          for KeyValue (k, v) in m do
            yield! formatValueLeafs (k :: ns) v
      }

module internal CustomFsMessageTemplates =
  open System.Reflection
  open Microsoft.FSharp.Reflection

  let destructureFSharpTypes (req: DestructureRequest) : TemplatePropertyValue =
    let value = req.Value
    match req.Value.GetType() with
    | t when FSharpType.IsTuple t ->
      let tupleValues =
          value
          |> FSharpValue.GetTupleFields
          |> Seq.map req.TryAgainWithValue
          |> Seq.toList
      SequenceValue tupleValues

    | t when t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typedefof<List<_>> ->
      let objEnumerable = value :?> System.Collections.IEnumerable |> Seq.cast<obj>
      SequenceValue(objEnumerable |> Seq.map req.TryAgainWithValue |> Seq.toList)

    | t when t.IsConstructedGenericType
      && (t.GetGenericTypeDefinition() = typedefof<Map<_,_>>
          || (t.BaseType.IsConstructedGenericType
              && t.BaseType.GetGenericTypeDefinition() = typedefof<HashMap<string,_>>)) ->

      let keyProp, valueProp = ref Unchecked.defaultof<PropertyInfo>, ref Unchecked.defaultof<PropertyInfo>
      let getKey o = if isNull !keyProp  then keyProp := o.GetType().GetRuntimeProperty("Key")
                     (!keyProp).GetValue(o)
      let getValue o = if isNull !valueProp then valueProp := o.GetType().GetRuntimeProperty("Value")
                       (!valueProp).GetValue(o)
      let objEnumerable = value :?> System.Collections.IEnumerable |> Seq.cast<obj>
      let skvps = objEnumerable
                  |> Seq.map (fun o ->  req.TryAgainWithValue (getKey o),  req.TryAgainWithValue (getValue o))
                  |> Seq.toList
      DictionaryValue skvps

    | t when FSharpType.IsUnion t ->
      let case, fields = FSharpValue.GetUnionFields(value, t)
      let caseName = ScalarValue case.Name
      match fields with
      | [||] -> caseName
      | [| oneField |] ->
        let oneValue = req.TryAgainWithValue oneField
        DictionaryValue [caseName, oneValue]
      | _ ->
        let fieldsValue = fields |> Array.map req.TryAgainWithValue |> Array.toList |> SequenceValue
        DictionaryValue [caseName, fieldsValue]
    | _ -> TemplatePropertyValue.Empty

  let destructureCustomScalar (req: DestructureRequest) : TemplatePropertyValue =
    let origin = req.Value
    match origin with
    | :? Gauge as gauge ->
      let (Gauge (value, units)) = gauge
      let (scaledValue, unitsFormat) = Units.scale units value
      if String.IsNullOrEmpty unitsFormat then ScalarValue scaledValue
      else ScalarValue (sprintf "%f %s" scaledValue unitsFormat)
    | _ -> TemplatePropertyValue.Empty

/// A thing that efficiently writes a message to a TextWriter.
type MessageWriter =
  abstract write : TextWriter -> Message -> unit

[<AutoOpen>]
module MessageWriterEx =
  type MessageWriter with
    [<Obsolete "Try to write directly to a System.IO.TextWriter instead">]
    member x.format (m : Message) =
      use sw = StringWriter()
      x.write sw m
      sw.ToString()

/// simple message writer use messagetemplates
/// json writer should use from other project that use fspickler.json
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MessageWriter =
  open LiterateFormatting.MessageParts
  open LiterateFormatting.Tokens

  let private appendToString (tokenised: seq<string * LiterateToken>) =
    let sb = StringBuilder ()
    tokenised |> Seq.map fst |> Seq.iter (sb.Append >> ignore)
    sb.ToString ()


  let private defaultDestr =
    Capturing.createCustomDestructurer
      (Some CustomFsMessageTemplates.destructureCustomScalar)
      (Some CustomFsMessageTemplates.destructureFSharpTypes)

  /// maxDepth can be avoided if cycle reference are handled properly
  let expanded destr maxDepth nl ending : MessageWriter =
    { new MessageWriter with
        member x.write tw m =
          let level = string (caseNameOf m.level).[0]
          // https://noda-time.googlecode.com/hg/docs/api/html/M_NodaTime_OffsetDateTime_ToString.htm
          let time = formatTimestamp m.timestampTicks
          let body = tokeniseTemplateWithGauges tw.FormatProvider nl destr maxDepth m |> appendToString
          let name = m.name.ToString()
          let context = tokeniseContext tw.FormatProvider nl destr maxDepth m |> appendToString
          sprintf "%s %s: %s [%s]%s%s" level time body name context ending
          |> tw.Write
    }

  /// Verbatim simply outputs the message and no other information
  /// and doesn't append a newline to the string.
  let verbatim =
    { new MessageWriter with
        member x.write tw m =
          tokeniseTemplateWithGauges tw.FormatProvider Environment.NewLine defaultDestr 10 m
          |> Seq.map fst |> Seq.iter tw.Write
    }

  /// VerbatimNewline simply outputs the message and no other information
  /// and does append a newline to the string.
  let verbatimNewLine =
    { new MessageWriter with
        member x.write tw m =
          verbatim.write tw m
          tw.WriteLine()
    }

  /// <see cref="MessageWriter.LevelDatetimePathMessageNewLine" />
  let levelDatetimeMessagePath =
    expanded defaultDestr 10 Environment.NewLine ""

  /// LevelDatetimePathMessageNl outputs the most information of the Message
  /// in text format, starting with the level as a single character,
  /// then the ISO8601 format of a DateTime (with +00:00 to show UTC time),
  /// then the path in square brackets: [Path.Here], the message and a newline.
  /// Exceptions are called ToString() on and prints each line of the stack trace
  /// newline separated.
  let levelDatetimeMessagePathNewLine =
    expanded defaultDestr 10 Environment.NewLine Environment.NewLine
