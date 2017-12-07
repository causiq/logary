namespace Logary

open System
open System.Globalization
open System.Text
open System.IO
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

  let isDictionable (t: Type) =
    t.GetInterfaces()
    |> Seq.exists (fun iface -> 
       if iface.IsGenericType && iface.GetGenericTypeDefinition() = typedefof<System.Collections.Generic.IEnumerable<_>> then
         let elementType = iface.GetGenericArguments().[0]
         elementType.IsGenericType && elementType.GetGenericTypeDefinition() = typedefof<System.Collections.Generic.KeyValuePair<_,_>>
       else iface = typeof<System.Collections.IDictionary>)

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

    | t when t.IsConstructedGenericType && t.GetGenericTypeDefinition() = typedefof<FSharp.Collections.List<_>> ->
      let objEnumerable = value :?> System.Collections.IEnumerable |> Seq.cast<obj>
      SequenceValue(objEnumerable |> Seq.map req.TryAgainWithValue |> Seq.toList)

    | t when isDictionable t ->

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
      else ScalarValue (sprintf "%s %s" (string scaledValue) unitsFormat)
    | _ -> TemplatePropertyValue.Empty


/// simple message writer use messagetemplates
/// json writer should use from other project that use fspickler.json
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MessageWriter =
  open Logary.Formatting.Literate.MessageParts
  open Logary.Formatting.Literate.Tokens
  open Microsoft.FSharp.Reflection
  

    /// Returns the case name of the object with union type 'ty.
  let private caseNameOf (x:'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

  /// Format a timestamp in nanoseconds since epoch into a ISO8601 string
  let formatTimestamp (timestamp : EpochNanoSeconds) =
    Instant.ofEpoch(timestamp).ToDateTimeOffset().ToString("o", CultureInfo.InvariantCulture)

  let private appendToString (tokenised: seq<string * LiterateToken>) =
    let sb = StringBuilder ()
    tokenised |> Seq.map fst |> Seq.iter (sb.Append >> ignore)
    sb.ToString ()


  let internal defaultDestr =
    Capturing.createCustomDestructurer
      (Some CustomFsMessageTemplates.destructureCustomScalar)
      (Some CustomFsMessageTemplates.destructureFSharpTypes)

  /// maxDepth can be avoided if cycle reference are handled properly
  let expanded destr maxDepth nl ending : MessageWriter =
    { new MessageWriter with
        member x.write tw m =
          let level = string (caseNameOf m.level).[0]
          let time = formatTimestamp m.timestamp
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

  let contextWriter =
    { new MessageWriter with
        member x.write tw m =
          tokeniseContext tw.FormatProvider Environment.NewLine defaultDestr 10 m
          |> Seq.map fst |> Seq.iter tw.Write
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
