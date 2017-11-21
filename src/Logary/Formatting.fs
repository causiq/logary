namespace Logary

open System
open System.Globalization
open System.Text
open System.IO
open NodaTime
open Logary
open Logary.Internals.FsMessageTemplates

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

module internal CustomFsMessageTemplates =
  open System.Reflection
  open Microsoft.FSharp.Reflection

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

  let writeScalarValue (w: TextWriter) (sv: obj) format =
    match sv with
    | null -> w.Write "null"
    | :? string as s ->
        if format = "l" then w.Write s
        else w.Write (escapeNewlineAndQuote s)
    | _ ->
      // build in scalar write directly
      // else escape newline and double quote
      let formated = formatWithProvider w.FormatProvider sv format
      if Destructure.scalarTypeHash.Contains(sv.GetType()) then w.Write formated
      else w.Write (escapeNewlineAndQuote formated)

  // use for formatting message template
  let rec writePropValueCompact (w: TextWriter) (tpv: TemplatePropertyValue) (format: string) =
    match tpv with
    | ScalarValue sv ->
      writeScalarValue w sv format
    | SequenceValue svs ->
      w.Write '['
      let lastIndex = svs.Length - 1
      svs 
      |> List.iteri (fun i sv ->
         writePropValueCompact w sv null
         if i <> lastIndex then w.Write ", ")
      w.Write ']'
    | StructureValue(typeTag, values) ->
      if not <| isNull typeTag then w.Write typeTag; w.Write ' '
      w.Write "{ "
      let lastIndex = values.Length - 1
      for i = 0 to lastIndex do
        let tp = values.[i]
        w.Write tp.Name; w.Write ": "
        writePropValueCompact w tp.Value null
        w.Write (if i = lastIndex then " " else ", ")
      w.Write "}"
    | DictionaryValue(data) ->
      w.Write '['
      data 
      |> List.iter (fun (entryKey, entryValue) ->
         w.Write '('
         writePropValueCompact w entryKey null
         w.Write ": "
         writePropValueCompact w entryValue null
         w.Write ")")
      w.Write ']'

  // use for formatting message context
  let rec writePropValueIndent (w: TextWriter) (tpv: TemplatePropertyValue) depth =
    // context: use 2 indent, fields/gauges/other use 2 indent, depth start from 0, so 2+2+2 = 6
    let indent = new String (' ', depth * 2 + 6)
    match tpv with
    | ScalarValue sv -> writeScalarValue w sv null

    | SequenceValue svs ->
      let isAllScalar = svs |> List.forall (function ScalarValue _ -> true | _ -> false)
      if isAllScalar then
        let lastIndex = svs.Length - 1
        w.Write '['
        svs |> List.iteri (fun i sv ->
               writePropValueIndent w sv 0  // writeScalarValue will not use depth, so pass zero
               if i <> lastIndex then w.Write ", ")
        w.Write ']'
      else
        svs |> List.iter (fun sv ->
               w.WriteLine (); w.Write indent; w.Write "- "; writePropValueIndent w sv (depth + 1);)

    | StructureValue(typeTag, values) ->
      let writeTypeTag = not <| isNull typeTag
      if writeTypeTag then w.WriteLine (); w.Write indent; w.Write typeTag; w.Write " {";

      values
      |> List.iter (fun nv ->
         w.WriteLine ();
         if writeTypeTag then w.Write "  "
         w.Write indent; w.Write nv.Name; w.Write " => "; writePropValueIndent w nv.Value (if writeTypeTag then depth + 2 else depth + 1);)

      if writeTypeTag then w.Write "}";

    | DictionaryValue(kvList) ->
      kvList
      |> List.iter (fun (entryKey, entryValue) ->
         match entryKey with
         | ScalarValue _ ->
           w.WriteLine (); w.Write indent; writePropValueIndent w entryKey (depth + 1); w.Write " => "; writePropValueIndent w entryValue (depth + 1);
         | _ ->
           // default case will not go to here, unless user define their own DictionaryValue which its entryKey is not ScalarValue
           w.WriteLine (); w.Write indent; w.Write "- key => "; writePropValueIndent w entryKey (depth + 2);
           w.WriteLine (); w.Write indent; w.Write "  value => "; writePropValueIndent w entryValue (depth + 2);)

  let writeToken (buffer: StringBuilder) (w: TextWriter) (token:Token) (value:TemplatePropertyValue) =
    match token, value with
    | Token.TextToken (_, raw), _ -> w.Write raw
    | Token.PropToken (_, pt), pv ->
      if Destructure.isEmptyKeepTrying pv then
          let propertyTokenAsString = pt.AppendPropertyString(buffer, true, pt.Name).ToStringAndClear()
          w.Write propertyTokenAsString
      else
          if pt.Align.IsEmpty then
              writePropValueCompact w pv pt.Format
          else
              let alignWriter = new StringWriter(w.FormatProvider)
              writePropValueCompact alignWriter pv pt.Format
              let valueAsString = alignWriter.ToString()
              if valueAsString.Length >= pt.Align.Width then
                  w.Write valueAsString
              else
                  let pad = pt.Align.Width - valueAsString.Length
                  if pt.Align.Direction = Direction.Right then w.Write (System.String(' ', pad))
                  w.Write valueAsString
                  if pt.Align.Direction = Direction.Left then w.Write (System.String(' ', pad))

  let formatCustom (t:Template) w getValueByName =
    let buffer = StringBuilder()
    for tok in t.Tokens do
        match tok with
        | Token.TextToken _ as tt -> writeToken buffer w tt TemplatePropertyValue.Empty
        | Token.PropToken (_, pd) as tp ->
            let value = getValueByName pd.Name
            writeToken buffer w tp value

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

  let getTextWriter (provider : IFormatProvider) nl =
    let tw = new StringWriter (provider)
    tw.NewLine <- nl
    tw

  let generateFormattedTemplateByGauges (tw: TextWriter) (gauges : List<string * Gauge>) =
    if gauges.Length = 0 then ()
    else
      tw.Write "Gauges: ["

      let lastIndex = gauges.Length - 1
      gauges
      |> List.iteri (fun i (gaugeType, Gauge (value, units)) ->
         let (scaledValue, unitsFormat) = Units.scale units value
         let valueFormated = CustomFsMessageTemplates.formatWithProvider tw.FormatProvider scaledValue null
         tw.Write gaugeType
         tw.Write " : "
         tw.Write valueFormated
         if not <| String.IsNullOrEmpty unitsFormat then tw.Write (" " + unitsFormat)
         if i <> lastIndex then tw.Write ", ")

      tw.Write "]"

  let generateFormattedTemplateByFields (tw: TextWriter) (template : Template)
                                        (destr : Destructurer) maxDepth (fields : seq<string * obj>) =
    if Seq.isEmpty fields then tw.Write template.FormatString
    else
      let fieldsMap = fields |> HashMap.ofSeq
      let propertiesMap = template.Properties |> Seq.map (fun p -> (p.Name, p.Destr)) |> HashMap.ofSeq
      let getValueByName name =
        match (HashMap.tryFind name fieldsMap, HashMap.tryFind name propertiesMap) with
        | Some (value), Some (destrHint) ->
          destr (DestructureRequest(destr, value, maxDepth, 1, hint=destrHint))
        | _ -> TemplatePropertyValue.Empty
      CustomFsMessageTemplates.formatCustom template tw getValueByName

  let formatTemplate provider nl destr maxDepth message =
    use gaugeTw = getTextWriter provider nl
    message |> Message.getAllGauges |> List.ofSeq |> generateFormattedTemplateByGauges gaugeTw
    let generateGaugesInfo = gaugeTw.ToString ()

    let inline appendGaugesInfo origin gaugeInfo =
      if String.IsNullOrEmpty gaugeInfo then origin
      else origin  + " " + gaugeInfo

    let (Event (formatTemplate)) = message.value

    if String.IsNullOrEmpty formatTemplate then generateGaugesInfo
    else
      let parsedTemplate = Parser.parse (formatTemplate)
      if parsedTemplate.HasAnyProperties then
        use templateTw = getTextWriter provider nl
        message |> Message.getAllFields |> generateFormattedTemplateByFields templateTw parsedTemplate destr maxDepth
        let tplByFields = templateTw.ToString()
        appendGaugesInfo tplByFields generateGaugesInfo
      else
        // raw message with no fields needs format
        appendGaugesInfo formatTemplate generateGaugesInfo

  let formatContext (formatProvider: IFormatProvider) (nl: string) (destr: Destructurer) maxDepth message =
    let padding = new String (' ', 6)

    let inline appendWithNlPrefix (sb: StringBuilder) (prefix: string) (value: string) (nl: string) =
      if not <| String.IsNullOrEmpty value then sb.Append(nl).Append(prefix).Append(value) |> ignore

    let inline processKvs sb (prefix: string) (nl: string) (kvs: seq<string * obj>) =
      use tw = getTextWriter formatProvider nl
      kvs
      |> Seq.iter (fun (name, value) ->
         let destrValue = destr (DestructureRequest(destr, value, maxDepth, 1, hint=DestrHint.Destructure))
         tw.Write nl; tw.Write padding; tw.Write name; tw.Write " => "; CustomFsMessageTemplates.writePropValueIndent tw destrValue 1)

      appendWithNlPrefix sb prefix (tw.ToString()) nl

    let sb = StringBuilder ()

    // process fields
    let (Event (formatTemplate)) = message.value
    if not <| String.IsNullOrEmpty formatTemplate then
      let parsedTemplate = Parser.parse (formatTemplate)
      if parsedTemplate.HasAnyProperties then
        use tw = getTextWriter formatProvider nl

        let fieldsHap = message |> Message.getAllFields |> HashMap.ofSeq
        parsedTemplate.Tokens
        |> Seq.iter (fun token ->
           match token with
           | TextToken _ -> ()
           | PropToken (_, prop) as fieldToken ->
             match fieldsHap |> HashMap.tryFind prop.Name with
             | None -> ()
             | Some fieldValue ->
               tw.Write nl; tw.Write padding; tw.Write prop.Name; tw.Write " => ";
               let buffer = StringBuilder ()
               let destrValue = destr (DestructureRequest(destr, fieldValue, maxDepth, 1, hint= prop.Destr))
               CustomFsMessageTemplates.writeToken buffer tw fieldToken destrValue)

        appendWithNlPrefix sb "    fields:" (tw.ToString()) nl
      else ()
    else ()

    // process gauge
    message |> Message.getAllGauges |> Seq.map (fun (k, gauge) -> (k, box gauge)) |> processKvs sb "    gauges:" nl

    // process others
    message |> Message.getContextsOtherThanGaugeAndFields |> processKvs sb "    others:" nl

    let wholeRes = sb.ToString ()
    if String.IsNullOrEmpty wholeRes then String.Empty
    else String.Concat(nl,"  context:", wholeRes)

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

/// simple message writer use messagetemplates
/// json writer should use from other project that use fspickler.json
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MessageWriter =
  open MessageParts

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
          let body = formatTemplate tw.FormatProvider nl destr maxDepth m
          let name = m.name.ToString()
          let context = formatContext tw.FormatProvider nl destr maxDepth m
          sprintf "%s %s: %s [%s]%s%s" level time body name context ending
          |> tw.Write
    }

  /// Verbatim simply outputs the message and no other information
  /// and doesn't append a newline to the string.
  let verbatim =
    { new MessageWriter with
        member x.write tw m =
          formatTemplate tw.FormatProvider Environment.NewLine defaultDestr 10 m
          |> tw.Write
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
