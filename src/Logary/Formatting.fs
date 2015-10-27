/// A module for converting log lines to text/strings.
module Logary.Formatting

open System
open System.Globalization
open System.Collections
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Reflection

open Logary.Utils.FsMessageTemplates
open Logary.DataModel

let internal formatMessage (template:string) (args : Map<PointName, Field>) =
  let template = Parser.parse template
  let sb       = System.Text.StringBuilder()

  let append (sb : System.Text.StringBuilder) (s : string) = sb.Append s |> ignore

  template.Tokens
  |> Seq.map (function
    | Text (_, t) -> t
    | Prop (_, p) ->
      let (Field (value, units)) = Map.find [p.Name] args
      match units with
      | Some units ->
        Units.formatWithUnit Units.Suffix (units) value
      | None ->
        Units.formatValue value)
  |> Seq.iter (append sb)
  sb.ToString()

/// Returns the case name of the object with union type 'ty.
let internal caseNameOf (x:'a) =
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
  | Binary (b, _) -> System.BitConverter.ToString b |> fun s -> s.Replace("-", "")
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
let internal formatFields (nl : string) (fields : Map<PointName, Field>) =
  Map.toSeq fields
  |> Seq.map (fun (key, (Field (value, _))) -> (PointName.joined key, value))
  |> Map.ofSeq
  |> Object
  |> printValue nl 0

/// A StringFormatter is the thing that takes a message and returns it as a string
/// that can be printed, sent or otherwise dealt with in a manner that suits the target.
type StringFormatter =
  { format   : Message -> string }
  static member private Expanded nl ending =
    let format' =
      fun (m : Message) ->
        sprintf "%s %s: %s [%s]%s%s"
          (string (caseNameOf m.level).[0])
          // https://noda-time.googlecode.com/hg/docs/api/html/M_NodaTime_OffsetDateTime_ToString.htm
          (NodaTime.Instant(m.timestamp).ToDateTimeOffset().ToString("o", CultureInfo.InvariantCulture))
          ((function Event format -> formatMessage format m.fields | _ -> "") m.value)
          (Message.Context.serviceGet m)
          (if Map.isEmpty m.fields then "" else formatFields nl m.fields)
          ending
    { format  = format' }

  /// Takes c# Func delegates to initialise a StringFormatter
  static member Create (format : Func<Message, string>) =
    { format = fun x -> format.Invoke x }

  /// Verbatim simply outputs the message and no other information
  /// and doesn't append a newline to the string.
  // TODO: serialize properly
  static member Verbatim =
    { format   =
      fun m ->
        match m.value with
        | Event format -> formatMessage format m.fields
        | Gauge (value, unit) | Derived (value, unit) -> value.ToString () }

  /// VerbatimNewline simply outputs the message and no other information
  /// and does append a newline to the string.
  static member VerbatimNewline =
    { format = fun m -> sprintf "%s%s" (StringFormatter.Verbatim.format m) (Environment.NewLine)}

  /// <see cref="StringFormatter.LevelDatetimePathMessageNl" />
  static member LevelDatetimeMessagePath =
    StringFormatter.Expanded (Environment.NewLine) ""

  /// LevelDatetimePathMessageNl outputs the most information of the log line
  /// in text format, starting with the level as a single character,
  /// then the ISO8601 format of a DateTime (with +00:00 to show UTC time),
  /// then the path in square brackets: [Path.Here], the message and a newline.
  /// Exceptions are called ToString() on and prints each line of the stack trace
  /// newline separated.
  static member LevelDatetimeMessagePathNl =
    StringFormatter.Expanded (Environment.NewLine) (Environment.NewLine)

open NodaTime.TimeZones

module internal Json =
  open Logary.Utils.Chiron
  open Logary.Utils.Chiron.Operators

  let inline toNumber i = (decimal >> Json.Number) i

  let rec valueToJson (value : Value) =
    match value with
    | Value.String s -> Json.String s
    | Value.Bool b -> Json.Bool b
    | Value.Float f -> toNumber f
    | Value.Int64 i -> toNumber i
    | Value.BigInt bi -> toNumber bi
    | Value.Binary (bytes, mime) ->
      [("mime",   Json.String mime)
       ("base64", Json.String (System.Convert.ToBase64String bytes))]
      |> Map |> Json.Object
    | Value.Fraction (nom, denom) ->
      [("nom",   toNumber nom)
       ("denom", toNumber denom)]
      |> Map |> Json.Object
    | Value.Object values -> Map.map (fun _ v -> valueToJson v) values |> Json.Object
    | Value.Array items -> List.map (valueToJson) items |> Json.Array

  let fieldsToJson (fields : Map<PointName, Field>) =
    Map.toSeq fields
    |> Seq.map (fun (k, Field (v, _)) -> (PointName.joined k, valueToJson v))
    |> Map |> Json.Object

  let isEmpty = function
  | String "" -> true
  | Object v -> v.IsEmpty
  | Array [] -> true
  | _ -> false

  let messageToJson (msg: Message) =
    [("name", Json.String <| PointName.joined msg.name)
     ("session", valueToJson msg.session)
     ("level", Json.String <| msg.level.ToString ())
     ("timestamp", Json.String <| NodaTime.Instant(msg.timestamp).ToDateTimeOffset().ToString("o", CultureInfo.InvariantCulture))
     ("session", valueToJson msg.session)
     ("context", Map.map (fun _ v -> valueToJson v) msg.context |> Json.Object)
     ("data", fieldsToJson msg.fields)] @
    (match msg.value with
     | Event m ->        [("type", Json.String "event");   ("message", Json.String <| formatMessage m msg.fields)]
     | Gauge   (g, u) -> [("type", Json.String "gauge");   ("value", valueToJson g);
                          ("unit", Json.String <| Units.symbol u)]
     | Derived (d, u) -> [("type", Json.String "derived"); ("value", valueToJson d);
                          ("unit", Json.String <| Units.symbol u)])
    |> Seq.filter (snd >> isEmpty >> not)
    |> Map |> Json.Object

  let format = Json.format

/// A JsonFormatter takes a message and converts it into a JSON string.
type JsonFormatter =
  /// Creates a new JSON formatter.
  static member Default =
    { format = fun msg -> Json.format (Json.messageToJson msg) }
