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


  let internal defaultDestr = logaryDestructure Global.Destructure.destructureFac

  /// maxDepth can be avoided if cycle reference are handled properly
  let expanded maxDepth nl ending : MessageWriter =
    { new MessageWriter with
        member x.write tw m =
          let level = string (caseNameOf m.level).[0]
          let time = formatTimestamp m.timestamp
          let body = tokeniseTemplateWithGauges tw.FormatProvider nl defaultDestr maxDepth m |> appendToString
          let name = m.name.ToString()
          let context = tokeniseContext tw.FormatProvider nl defaultDestr maxDepth m |> appendToString
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
    expanded 10 Environment.NewLine ""

  /// LevelDatetimePathMessageNl outputs the most information of the Message
  /// in text format, starting with the level as a single character,
  /// then the ISO8601 format of a DateTime (with +00:00 to show UTC time),
  /// then the path in square brackets: [Path.Here], the message and a newline.
  /// Exceptions are called ToString() on and prints each line of the stack trace
  /// newline separated.
  let levelDatetimeMessagePathNewLine =
    expanded 10 Environment.NewLine Environment.NewLine
