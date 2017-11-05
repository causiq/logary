namespace Logary

open System
open System.Globalization
open System.Text
open System.IO
open System.Runtime.InteropServices
open Microsoft.FSharp.Reflection
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
      use sw = new StringWriter(StringBuilder())
      x.write sw m
      sw.ToString()

module internal MessageParts =
  open Message

  let append (sb : StringBuilder) (s : string) =
    sb.Append s |> ignore

  /// Returns the case name of the object with union type 'ty.
  let caseNameOf (x:'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

  /// Format a timestamp in nanoseconds since epoch into a ISO8601 string
  let formatTimestamp (ticks : int64) =
    Instant.FromTicksSinceUnixEpoch(ticks)
      .ToDateTimeOffset()
      .ToString("o", CultureInfo.InvariantCulture)

  let formatWithProvider (provider : IFormatProvider) (arg : obj) format =
    let customFormatter = provider.GetFormat(typeof<System.ICustomFormatter>) :?> System.ICustomFormatter
    match customFormatter with
    | cf when not (isNull cf) ->
      (cf.Format(format, arg, provider))
    | _ ->
      match arg with
      | :? System.IFormattable as f -> f.ToString(format, provider)
      | _ -> arg.ToString()

  let generateFormattedTemplateByGauges provider (gauges : seq<string * Gauge>) =
    let gaugeList =
      gauges
      |> Seq.map (fun (gaugeType, Gauge (value, units)) ->
         let valueFormat = formatWithProvider provider value null
         let unitsFormat = Units.symbol units
         sprintf "%s : %s %s" gaugeType valueFormat unitsFormat)
      |> Seq.toList

    let sb = StringBuilder ()
    append sb "Gauges: ["
    let lastIndex = gaugeList.Length
    gaugeList |> List.iteri (fun i gauge ->
      append sb gauge
      if i <> lastIndex then append sb ", "
      )
    append sb "]"

    sb.ToString ()

  let generateFormattedTemplateByFields (provider : IFormatProvider) (template : Template)
                                        (destr : Destructurer) maxDepth (fields : seq<string * obj>) =
    if Seq.isEmpty fields then template.FormatString
    else
      use tw = new StringWriter(provider)
      let fieldsMap = fields |> HashMap.ofSeq
      let propertiesMap = template.Properties |> Seq.map (fun p -> (p.Name, p.Destr)) |> HashMap.ofSeq
      let getValueByName name =
        match (HashMap.tryFind name fieldsMap, HashMap.tryFind name propertiesMap) with
        | Some (value), Some (destrHint) ->
          destr (DestructureRequest(destr, value, maxDepth, 1, hint=destrHint))
        | _ -> TemplatePropertyValue.Empty
      Formatting.formatCustom template tw getValueByName
      tw.ToString ()

  let formatTemplate formatProvider destr maxDepth message =
    let (Event (formatTemplate)) = message.value
    if String.IsNullOrEmpty formatTemplate then
      if hasGauge message then
        message |> getAllGauges |> generateFormattedTemplateByGauges formatProvider
      else
        String.Empty
    else
      let parsedTemplate = Parser.parse (formatTemplate)
      if parsedTemplate.HasAnyProperties then
        message |> getAllFields |> generateFormattedTemplateByFields formatProvider parsedTemplate destr maxDepth
      else
        // raw message with no fields needs format
        formatTemplate

  let rec writePropValue (w: TextWriter) (tpv: TemplatePropertyValue) depth =
    // context: use 2 indent, fields/gauges/other use 2 indent, depth start from 0, so 2+2+2 = 6
    let indent = new String (' ', depth * 2 + 6)
    match tpv with
    | ScalarValue sv ->
      match sv with
      | null -> w.Write "null"
      | :? string as s ->
        w.Write "\""
        w.Write (s.Replace("\"", "\\\""))
        w.Write "\""
      | _ ->
        let customFormatter = w.FormatProvider.GetFormat(typeof<System.ICustomFormatter>) :?> System.ICustomFormatter
        match customFormatter with
        | cf when not (isNull cf) ->
          w.Write (cf.Format(null, sv, w.FormatProvider))
        | _ ->
          match sv with
          | :? System.IFormattable as f -> w.Write (f.ToString(null, w.FormatProvider))
          | _ -> w.Write(sv.ToString())

    | SequenceValue svs ->
      svs
      |> List.iter (fun sv ->
         w.WriteLine (); w.Write indent; w.Write '-'; writePropValue w sv (depth + 1);)

    | StructureValue(typeTag, values) ->
      let writeTypeTag = not <| isNull typeTag
      if writeTypeTag then w.WriteLine (); w.Write indent; w.Write typeTag; w.Write " {";

      values
      |> List.iter (fun nv ->
         w.WriteLine ()
         w.Write indent; w.Write nv.Name; w.Write " => "; writePropValue w nv.Value (depth + 1);)

      if writeTypeTag then w.WriteLine (); w.Write indent; w.Write "}";

    | DictionaryValue(kvList) ->
      kvList
      |> List.iter (fun (entryKey, entryValue) ->
         match entryKey with
         | ScalarValue _ ->
           w.WriteLine ()
           w.Write indent; writePropValue w entryKey (depth + 1); w.Write " => "; writePropValue w entryValue (depth + 1);
         | _ ->
           // default case will not go to here, unless user define their own DictionaryValue which its entryKey is not ScalarValue
           w.WriteLine (); w.Write indent; w.Write "- key => "; writePropValue w entryKey (depth + 1);
           w.WriteLine (); w.Write indent; w.Write "  value => "; writePropValue w entryValue (depth + 1);)
    ()

  let formatContext (formatProvider: IFormatProvider) (destr: Destructurer) maxDepth message =
    use tw = StringWriter (formatProvider)

    // process fields
    tw.Write "    fields:"
    let (Event (formatTemplate)) = message.value
    if String.IsNullOrEmpty formatTemplate then ()
    else
      let parsedTemplate = Parser.parse (formatTemplate)
      if parsedTemplate.HasAnyProperties then
        let fieldsHap = message |> getAllFields |> HashMap.ofSeq
        parsedTemplate.Tokens
        |> Seq.iter (fun token ->
           match token with
           | TextToken _ -> ()
           | PropToken (_, prop) as fieldToken ->
             tw.WriteLine (); tw.Write prop.Name; tw.Write " => ";
             let buffer = StringBuilder ()
             match fieldsHap |> HashMap.tryFind prop.Name with
             | None -> tw.Write "null"
             | Some fieldValue ->
               let destrValue = destr (DestructureRequest(destr, fieldValue, maxDepth, 1, hint= prop.Destr))
               Formatting.writeToken buffer tw fieldToken destrValue)
      else ()

    let processSeq (kvs: seq<string * obj>) =
      kvs
      |> Seq.iter (fun (name, value) ->
         let destrValue = destr (DestructureRequest(destr, value, maxDepth, 1, hint=DestrHint.Destructure))
         tw.Write name; tw.Write " => "; writePropValue tw destrValue 0)

    // process gauge
    tw.WriteLine ()
    tw.Write "    gauges:"
    message |> getAllGauges |> Seq.map (fun (k, gauge) -> (k, box gauge)) |> processSeq

    // process others
    tw.WriteLine ()
    tw.Write "    others:"
    message |> GetContextsOtherThanGaugeAndFields |> processSeq

    tw.ToString ()

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

  /// maxDepth can be avoided if cycle reference are handled properly
  let expanded destr maxDepth nl ending : MessageWriter =
    { new MessageWriter with
        member x.write tw m =
          let level = string (caseNameOf m.level).[0]
          // https://noda-time.googlecode.com/hg/docs/api/html/M_NodaTime_OffsetDateTime_ToString.htm
          let time = formatTimestamp m.timestampTicks
          let body = formatTemplate tw.FormatProvider destr maxDepth m
          let name = m.name.ToString()
          let context = formatContext tw.FormatProvider destr maxDepth m
          sprintf "%s %s: %s [%s]%s  context:%s%s%s" level time body name nl nl context ending
          |> tw.Write
    }

  /// Verbatim simply outputs the message and no other information
  /// and doesn't append a newline to the string.
  let verbatim =
    { new MessageWriter with
        member x.write tw m =
          formatTemplate tw.FormatProvider Capturing.defaultDestructureNoCustoms 10 m
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
    expanded Capturing.defaultDestructureNoCustoms 10 Environment.NewLine ""

  /// LevelDatetimePathMessageNl outputs the most information of the Message
  /// in text format, starting with the level as a single character,
  /// then the ISO8601 format of a DateTime (with +00:00 to show UTC time),
  /// then the path in square brackets: [Path.Here], the message and a newline.
  /// Exceptions are called ToString() on and prints each line of the stack trace
  /// newline separated.
  let levelDatetimeMessagePathNewLine =
    expanded Capturing.defaultDestructureNoCustoms 10 Environment.NewLine Environment.NewLine
