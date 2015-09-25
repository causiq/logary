/// A module for converting log lines to text/strings.
module Logary.Formatting

open System
open System.Globalization
open System.Collections
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Reflection

open Logary.DataModel
open Logary.Measure

/// Returns the case name of the object with union type 'ty.
let internal caseNameOf (x:'a) =
  match FSharpValue.GetUnionFields(x, typeof<'a>) with
  | case, _ -> case.Name

let app (s : string) (sb : StringBuilder) = sb.Append s

let rec print (nl : string) (depth : int) (x : obj) =
  let indent () = new String(' ', depth * 2 + 2)
  match x with
  | null -> "null"
  | :? string as s ->  "\"" + s + "\""
  | x ->
    let typ = x.GetType()
    let interfaces =
      typ.GetInterfaces()
      |> Array.filter (fun it -> it.IsGenericType)
      |> Array.map (fun it -> it.GetGenericTypeDefinition())
      |> fun ii -> new HashSet<_>(ii)
    let dicType = typeof<IDictionary<_, _>>.GetGenericTypeDefinition()

    if interfaces.Contains dicType then
      let ([| keyType; valueType |] as argTypes) = typ.GetGenericArguments()
      let dicTypeFilled = dicType.MakeGenericType argTypes
      let keysProp = dicTypeFilled.GetProperty "Keys"
      let itemProp = dicTypeFilled.GetProperty "Item"
      let item (key : 'a) = itemProp.GetValue(x, [| box key |])
      keysProp.GetValue x :?> System.Collections.IEnumerable
      |> Seq.cast<obj>
      |> Seq.fold (fun sb key ->
          sb
          |> app nl
          |> app (indent ())
          |> app (key.ToString())
          |> app " => "
          |> app (print nl (depth + 1) (item key)))
        (StringBuilder())
      |> fun sb -> sb.ToString()
    else
      match x with
      | :? System.Collections.IEnumerable as xs ->
        xs
        |> Seq.cast<obj>
        |> Seq.fold (fun sb t ->
            sb
            |> app nl
            |> app (indent ())
            |> app "- "
            |> app (print nl (depth + 1) t))
          (StringBuilder())
        |> fun sb -> sb.ToString()
      | x -> x.ToString()

/// Formats the data in a nice fashion for printing to e.g. the Debugger or Console.
let internal formatData (nl : string) (data : Map<string, obj>) =
  print nl 0 data

/// A StringFormatter is the thing that takes a log line and returns it as a string
/// that can be printed, sent or otherwise dealt with in a manner that suits the target.
type StringFormatter =
  { format   : Message -> string }
  static member private Expanded nl ending =
    let format' =
      fun (l : Message) ->
        // TODO / HACKHACKHACK: only serializes a few fields
        //let mex = l.``exception``
        sprintf "%s %s: %s [%s]%s"
          (string (caseNameOf l.level).[0])
          // https://noda-time.googlecode.com/hg/docs/api/html/M_NodaTime_OffsetDateTime_ToString.htm
          (NodaTime.Instant(l.timestamp).ToDateTimeOffset().ToString("o", CultureInfo.InvariantCulture))
          //l.message
          ((function Event format -> format | _ -> "") l.value)
          //l.path
          (l.context.ToString ())
          //(match l.tags with [] -> "" | _ -> " {" + String.Join(", ", l.tags) + "}")
          //(if Map.isEmpty l.data then "" else formatData nl l.data)
          //(match mex with None -> "" | Some ex -> sprintf " cont...%s%O" nl ex)
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
        | Event event -> event
        | Gauge (value, unit) -> value.ToString () }

  /// VerbatimNewline simply outputs the message and no other information
  /// and does append a newline to the string.
  static member VerbatimNewline =
    { format = fun m -> sprintf "%s%s" (StringFormatter.Verbatim.format m) (Environment.NewLine)}
      //format = fun m -> sprintf "%s%s" (Measure.getValueStr m) (Environment.NewLine) }

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

/// Wrapper that constructs a Json.Net JSON formatter.
type JsonFormatter =
  /// Create a new JSON formatter with optional function that can modify the serialiser
  /// settings before any other alteration is done.
  static member Default =
    { format = (fun ll -> "") }
