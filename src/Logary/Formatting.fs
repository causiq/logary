namespace Logary

/// A module for converting log lines to text/strings.
module Formatting =

  open Newtonsoft.Json
  open Intelliplan.JsonNet

  open System

  open Microsoft.FSharp.Reflection

  ///Returns the case name of the object with union type 'ty.
  let internal caseNameOf (x:'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

  /// A StringFormatter is the thing that takes a log line and returns it as a string
  /// that can be printed, sent or otherwise dealt with in a manner that suits the target.
  type StringFormatter =
    { format : LogLine -> string }
    static member private Expanded nl ending =
      { format = fun l ->
          let mex = l.``exception``
          sprintf "%s %s: %s [%s]%s%s%s"
            (string (caseNameOf l.level).[0])
            // https://noda-time.googlecode.com/hg/docs/api/html/M_NodaTime_OffsetDateTime_ToString.htm
            (l.timestamp.ToDateTimeOffset().ToString("o"))
            l.message
            l.path
            (match l.tags with [] -> "" | _ -> " {" + String.Join(", ", l.tags) + "}")
            (match mex with None -> "" | Some ex -> sprintf " cont...%s%O" nl ex)
            ending }

    /// Verbatim simply outputs the message and no other information
    /// and doesn't append a newline to the string.
    static member Verbatim = { format = fun l -> l.message }

    /// VerbatimNewline simply outputs the message and no other information
    /// and does append a newline to the string.
    static member VerbatimNewline = { format = fun l -> sprintf "%s%s" l.message (Environment.NewLine) }

    /// <see cref="StringFormatter.LevelDatetimePathMessageNl" />
    static member LevelDatetimePathMessage =
      StringFormatter.Expanded (Environment.NewLine) ""

    /// LevelDatetimePathMessageNl outputs the most information of the log line
    /// in text format, starting with the level as a single character,
    /// then the ISO8601 format of a DateTime (with +00:00 to show UTC time),
    /// then the path in square brackets: [Path.Here], the message and a newline.
    /// Exceptions are called ToString() on and prints each line of the stack trace
    /// newline separated.
    static member LevelDatetimePathMessageNl =
      StringFormatter.Expanded (Environment.NewLine) (Environment.NewLine)

  open NodaTime.TimeZones
  open NodaTime.Serialization.JsonNet

  /// A LogLevel to/from string converter for Json.Net.
  type LogLevelStringConverter() =
    inherit JsonConverter()

    override x.CanConvert typ =
      typ = typeof<LogLevel>

    override x.WriteJson(writer, value, serializer) =
      match value :?> LogLevel with | _ as v -> writer.WriteRawValue(sprintf "\"%O\"" v)

    override x.ReadJson(reader, t, _, serializer) =
      match reader.TokenType with
      | JsonToken.Null -> LogLevel.Info |> box
      | JsonToken.String -> LogLevel.FromString(reader.ReadAsString()) |> box
      | _ as t -> failwithf "invalid token %A when trying to read LogLevel" t

  /// Wrapper that constructs a Json.Net JSON formatter.
  type JsonFormatter =
    /// Construct some JSON.Net JsonSerializerSettings, with all the
    /// types that are relevant to serialize in a custom way in Logary.
    static member Settings ?funOpts =
      JsonSerializerSettings()
      |> fun o -> (defaultArg funOpts (fun x -> x)) o
      |> fun o -> o.Converters.Add <| LogLevelStringConverter() ; o
      |> Serialisation.extend
      |> fun o -> o.ConfigureForNodaTime(new DateTimeZoneCache(TzdbDateTimeZoneSource.Default))

    /// Create a new JSON formatter with optional function that can modify the serialiser
    /// settings before any other alteration is done.
    static member Default ?funOpts =
      let opts =
        match funOpts with
        | None -> JsonFormatter.Settings()
        | Some f -> JsonFormatter.Settings(f)
      let serialiser = JsonSerializer.Create opts
      { format =
        fun ll ->
          use sw = new System.IO.StringWriter()
          use w = new JsonTextWriter(sw)
          serialiser.Serialize(w, ll)
          sw.ToString() }
