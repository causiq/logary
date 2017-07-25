/// A module for converting Messages to text/strings.
module Logary.Formatting

open System
open System.Globalization
open System.Collections
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Reflection
open NodaTime
open Logary
open Logary.Utils
open Logary.Utils.FsMessageTemplates

module MessageParts =

  let formatTemplate (template : string) (args : Map<PointName, Field>) =
    let template = Parser.parse template
    let sb       = StringBuilder()

    let append (sb : StringBuilder) (s : string) =
      sb.Append s |> ignore

    template.Tokens
    |> Seq.map (function
      | TextToken (_, t) ->
        t

      | PropToken (_, p) ->
        match Map.tryFind (PointName.ofSingle p.Name) args with
        | Some (Field (value, units)) -> 
            match units with
            | Some units ->
              Units.formatWithUnit Units.Suffix (units) value
            | None ->
              Units.formatValue value
        | None -> p.ToString()
        )
    |> Seq.iter (append sb)
    sb.ToString()

  let formatValueShallow (msg: Message) =
    match msg.value with
    | Event t ->
      formatTemplate t msg.fields

    | Gauge (v, u)
    | Derived (v, u) ->
      Units.formatWithUnit Units.UnitOrientation.Prefix u v

  /// Returns the case name of the object with union type 'ty.
  let caseNameOf (x:'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

  let private app (s : string) (sb : StringBuilder) =
    sb.Append s

  let rec formatValue (nl: string) (depth: int) (value : Value) =
    let indent = new String(' ', depth * 2 + 2)
    match value with
    | String s -> "\"" + s + "\""
    | Bool b -> b.ToString ()
    | Float f -> f.ToString ()
    | Int64 i -> i.ToString ()
    | BigInt b -> b.ToString ()
    | Binary (b, _) -> BitConverter.ToString b |> fun s -> s.Replace("-", "")
    | Fraction (n, d) -> sprintf "%d/%d" n d
    | Array list ->
      list
      |> Seq.fold (fun (sb: StringBuilder) t ->
          sb
          |> app nl
          |> app indent
          |> app "- "
          |> app (formatValue nl (depth + 1) t))
        (StringBuilder ())
      |> fun sb -> sb.ToString ()
    | Object m ->
      m
      |> Map.toSeq
      |> Seq.fold (fun (sb: StringBuilder) (key, value) ->
          sb
          |> app nl
          |> app indent
          |> app key
          |> app " => "
          |> app (formatValue nl (depth + 1) value))
        (StringBuilder ())
      |> fun sb -> sb.ToString ()

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

  /// Formats the data in a nice fashion for printing to e.g. the Debugger or Console.
  let formatFields (nl : string) (fields : Map<PointName, Field>) =
    Map.toSeq fields
    |> Seq.map (fun (key, (Field (value, _))) -> PointName.format key, value)
    |> Map.ofSeq
    |> Object
    |> formatValue nl 0

  /// Formats the data in a nice fashion for printing to e.g. the Debugger or Console.
  let formatContext (nl : string) (context : Map<string, Value>) =
    let s = StringBuilder()
    let cs =
      Map.toSeq context
      |> Seq.map (fun (key, value) -> key, value)
      |> Map.ofSeq
      |> Object
      |> formatValue nl 1
    s.Append(nl)
     .Append("  Context:")
     .Append(cs) |> ignore
    s.ToString()

  /// Format a timestamp in nanoseconds since epoch into a ISO8601 string
  let formatTimestamp (ticks : int64) =
    (Instant.FromUnixTimeTicks ticks)
      .ToDateTimeOffset()
      .ToString("o", CultureInfo.InvariantCulture)

/// A StringFormatter is the thing that takes a message and returns it as a string
/// that can be printed, sent or otherwise dealt with in a manner that suits the target.
type StringFormatter =
  abstract format : Message -> string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StringFormatter =
  open MessageParts

  let internal expanded nl ending : StringFormatter =
    { new StringFormatter with
        member x.format m =
          let level = string (caseNameOf m.level).[0]
          // https://noda-time.googlecode.com/hg/docs/api/html/M_NodaTime_OffsetDateTime_ToString.htm
          let time = formatTimestamp m.timestampTicks
          let body = formatValueShallow m
          let name = m.name.ToString()
          let fields = (if Map.isEmpty m.fields then "" else formatFields nl m.fields)
          let context = (if Map.isEmpty m.context then "" else formatContext nl m.context)
          sprintf "%s %s: %s [%s]%s%s%s" level time body name fields context ending
    }

  /// Verbatim simply outputs the message and no other information
  /// and doesn't append a newline to the string.
  // TODO: serialize properly
  let verbatim =
    { new StringFormatter with
        member x.format m =
          match m.value with
          | Event format ->
            formatTemplate format m.fields

          | Gauge (value, unit)
          | Derived (value, unit) ->
            value.ToString()
    }

  /// VerbatimNewline simply outputs the message and no other information
  /// and does append a newline to the string.
  let verbatimNewLine =
    { new StringFormatter with
        member x.format m =
          sprintf "%s%s" (verbatim.format m) (Environment.NewLine)
    }

  /// <see cref="StringFormatter.LevelDatetimePathMessageNl" />
  let levelDatetimeMessagePath =
    expanded Environment.NewLine ""

  /// LevelDatetimePathMessageNl outputs the most information of the Message
  /// in text format, starting with the level as a single character,
  /// then the ISO8601 format of a DateTime (with +00:00 to show UTC time),
  /// then the path in square brackets: [Path.Here], the message and a newline.
  /// Exceptions are called ToString() on and prints each line of the stack trace
  /// newline separated.
  let levelDatetimeMessagePathNl =
    expanded Environment.NewLine Environment.NewLine

open NodaTime.TimeZones

/// A JsonFormatter takes a message and converts it into a JSON string.
module JsonFormatter =
  open Chiron

  /// Creates a new JSON formatter.
  let Default =
    { new StringFormatter with
        member x.format msg =
          Json.format (Json.serialize msg)
    }
