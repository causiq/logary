/// A module for converting log lines to text/strings.
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
      | Text (_, t) ->
        t

      | Prop (_, p) ->
        let (Field (value, units)) = Map.find (PointName.ofSingle p.Name) args
        match units with
        | Some units ->
          Units.formatWithUnit Units.Suffix (units) value

        | None ->
          Units.formatValue value)
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

  let private app (s : string) (sb : StringBuilder) = sb.Append s

  let rec printValue (nl: string) (depth: int) (value : Value) =
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
          |> app (printValue nl (depth + 1) t))
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
          |> app (printValue nl (depth + 1) value))
        (StringBuilder ())
      |> fun sb -> sb.ToString ()

  /// Formats the data in a nice fashion for printing to e.g. the Debugger or Console.
  let formatFields (nl : string) (fields : Map<PointName, Field>) =
    Map.toSeq fields
    |> Seq.map (fun (key, (Field (value, _))) -> PointName.joined key, value)
    |> Map.ofSeq
    |> Object
    |> printValue nl 0

  /// Format a timestamp in nanoseconds since epoch into a ISO8601 string
  let formatTimestamp (ts : int64) =
    Instant.FromTicksSinceUnixEpoch(ts / 100L (* ns per tick *))
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
          let time = formatTimestamp m.timestamp
          let body = formatValueShallow m
          let name = m.name.ToString()
          let fields = (if Map.isEmpty m.fields then "" else formatFields nl m.fields)
          sprintf "%s %s: %s [%s]%s%s" level time body name fields ending
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

  /// LevelDatetimePathMessageNl outputs the most information of the log line
  /// in text format, starting with the level as a single character,
  /// then the ISO8601 format of a DateTime (with +00:00 to show UTC time),
  /// then the path in square brackets: [Path.Here], the message and a newline.
  /// Exceptions are called ToString() on and prints each line of the stack trace
  /// newline separated.
  let levelDatetimeMessagePathNl =
    expanded Environment.NewLine Environment.NewLine

open NodaTime.TimeZones

module Json =
  open Logary.Utils.Chiron
  open Logary.Utils.Chiron.Operators

  let inline toNumber i = (decimal >> Json.Number) i

  let rec valueToJson (value : Value) =
    match value with
    | Value.String s ->
      Json.String s

    | Value.Bool b ->
      Json.Bool b

    | Value.Float f ->
      toNumber f

    | Value.Int64 i ->
      toNumber i

    | Value.BigInt bi -> toNumber bi
    | Value.Binary (bytes, mime) ->
      [ "mime",   Json.String mime
        "base64", Json.String (System.Convert.ToBase64String bytes) ]
      |> Map.ofList
      |> Json.Object

    | Value.Fraction (nom, denom) ->
      [ "nom",   toNumber nom
        "denom", toNumber denom ]
      |> Map.ofList
      |> Json.Object

    | Value.Object values ->
      Map.map (fun _ v -> valueToJson v) values
      |> Json.Object

    | Value.Array items ->
      List.map (valueToJson) items
      |> Json.Array

  let fieldsToJson (fields : Map<PointName, Field>) =
    Map.toSeq fields
    |> Seq.map (fun (k, Field (v, _)) -> (PointName.joined k, valueToJson v))
    |> Map.ofSeq |> Json.Object

  let isEmpty = function
    | String "" ->
      true

    | Object v ->
      v.IsEmpty

    | Array [] ->
      true

    | _ ->
      false

  let messageToJson (msg: Message) =
    // TODO: consider using native message formatting attached as static methods
    // on Message.
    [ "name", Json.String <| PointName.joined msg.name
      "session", valueToJson msg.session
      "level", Json.String <| msg.level.ToString ()
      "timestamp", Json.String (MessageParts.formatTimestamp msg.timestamp)
      "session", valueToJson msg.session
      "context", Map.map (fun _ v -> valueToJson v) msg.context |> Json.Object
      "data", fieldsToJson msg.fields
    ] @
    (match msg.value with
     | Event m ->
       let message = MessageParts.formatTemplate m msg.fields
       [ "type", Json.String "event"
         "message", Json.String message ]
     | Gauge   (g, u) -> [("type", Json.String "gauge");   ("value", valueToJson g);
                          ("unit", Json.String <| Units.symbol u)]
     | Derived (d, u) -> [("type", Json.String "derived"); ("value", valueToJson d);
                          ("unit", Json.String <| Units.symbol u)])
    |> Seq.filter (snd >> isEmpty >> not)
    |> Map.ofSeq |> Json.Object

  let format = Json.format

/// A JsonFormatter takes a message and converts it into a JSON string.
module JsonFormatter =
  /// Creates a new JSON formatter.
  let Default =
    { new StringFormatter with
        member x.format msg =
          Json.format (Json.messageToJson msg)
    }
