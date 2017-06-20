﻿namespace Logary.Targets

/// A module implementing a text writer target. Useful for writing to the
/// console output, or writing to a custom text writer.
module TextWriter =

  open System.IO
  open System.Runtime.CompilerServices
  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Internals
  open Logary.Target
  open Logary.Formatting

  /// Configuration for a text writer
  type TextWriterConf =
    { /// A string formatter to specify how to write the Message
      formatter  : StringFormatter
      /// the non-error text writer to output to
      output     : TextWriter
      /// the error text writer to output to
      error      : TextWriter
      /// whether to flush text writer after each line
      flush      : bool
      /// the log level that is considered 'important' enough to write to the
      /// error text writer
      isErrorAt  : LogLevel
      /// The semaphore to use when printing to the text writer.
      semaphore  : obj option }

    [<CompiledName "Create">]
    static member create(output, error, ?formatter : StringFormatter, ?semaphore : obj) =
      { formatter = defaultArg formatter JsonFormatter.Default
        output    = output
        error     = error
        flush     = false
        isErrorAt = LogLevel.Error
        semaphore = semaphore }

  module internal Impl =

    let loop (twConf : TextWriterConf)
             (ri : RuntimeInfo)
             (requests : RingBuffer<TargetMessage>)
             (shutdown : Ch<IVar<unit>>) =

      // Note: writing lines does not matching the behaviour wanted from the
      // coloured console target.
      let writeLine (tw : TextWriter) (str : string) : Job<unit> =
        match twConf.semaphore with
        | Some sem ->
          // this should be the default for the console unless explicitly disabled;
          // the combination of locks and async isn't very good, so we'll use the
          // synchronous function to do the write if we have a semaphore
          Job.Scheduler.isolate <| fun _ ->
            lock sem <| fun _ ->
              tw.WriteLine str
        | None ->
          Job.fromUnitTask (fun _ -> tw.WriteLineAsync str)

      let rec loop () : Job<unit> =
        Alt.choose [
          shutdown ^=> fun ack ->
            twConf.output.Dispose()

            if not (obj.ReferenceEquals(twConf.output, twConf.error)) then
              twConf.error.Dispose()

            ack *<= () :> Job<_>

          RingBuffer.take requests ^=> function
            | Log (logMsg, ack) ->
              job {
                let writer = if logMsg.level < twConf.isErrorAt then twConf.output else twConf.error
                do! writeLine writer (twConf.formatter.format logMsg)

                if twConf.flush then
                  do! Job.fromUnitTask (fun _ -> writer.FlushAsync())

                do! ack *<= ()
                return! loop ()
              }

            | Flush (ack, nack) ->
              job {
                do! Job.fromUnitTask (fun _ -> twConf.output.FlushAsync())
                do! Job.fromUnitTask (fun _ -> twConf.error.FlushAsync())
                do! Ch.give ack () <|> nack
                return! loop ()
              }
        ] :> Job<_>

      loop ()

  [<CompiledName "Create">]
  let create (conf : TextWriterConf) name =
    TargetUtils.stdNamedTarget (Impl.loop conf) name

  /// Use with LogaryFactory.New( s => s.Target<TextWriter.Builder>() )
  type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
    member x.WriteTo(out : #TextWriter, err : #TextWriter) =
      ! (callParent <| Builder({ conf with output = out; error = err }, callParent))

    new(callParent : FactoryApi.ParentCallback<_>) =
      Builder(TextWriterConf.create(System.Console.Out, System.Console.Error), callParent)

    interface Logary.Target.FactoryApi.SpecificTargetConf with
      member x.Build name = create conf name

/// The console Target for Logary
module Console =
  open System.Runtime.CompilerServices
  open Logary
  open Logary.Internals
  open Logary.Formatting
  open Logary.Target

  /// Console configuration structure.
  type ConsoleConf =
    { formatter : StringFormatter }

    [<CompiledName "Create">]
    static member create formatter =
      { formatter = formatter }

  /// Default console target configuration.
  let empty =
    { formatter = StringFormatter.levelDatetimeMessagePath }

  [<CompiledName "Create">]
  let create conf name =
    TextWriter.create
      { formatter = conf.formatter
        output    = System.Console.Out
        error     = System.Console.Error
        flush     = false
        isErrorAt = Error
        semaphore = Some Globals.consoleSemaphore }
      name

  /// Use with LogaryFactory.New( s => s.Target<Console.Builder>() )
  type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

    /// Specify the formatting style to use when logging to the console
    member x.WithFormatter( sf : StringFormatter ) =
      ! (callParent <| Builder({ conf with formatter = sf }, callParent))

    new(callParent : FactoryApi.ParentCallback<_>) =
      Builder(empty, callParent)

    interface Logary.Target.FactoryApi.SpecificTargetConf with
      member x.Build name = create conf name

module LiterateConsole =
  open System
  open System.Globalization
  open Logary
  open Logary.Formatting
  open Logary.Target
  open Logary.Internals
  open Hopac

  module Tokens =
    /// The output tokens, which can be potentially coloured.
    type LiterateToken =
      | Text | Subtext
      | Punctuation
      | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
      | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
      | MissingTemplateField

  type ConsoleColours = { foreground : ConsoleColor; background : ConsoleColor option }
  type ColouredText = { text : string; colours : ConsoleColours }

  /// Console configuration structure
  type LiterateConsoleConf =
    { formatProvider    : IFormatProvider
      /// Converts a log level into a display string. By default: VRB, DBG, INF, WRN, ERR, FTL
      getLogLevelText   : LogLevel -> string
      /// Formats the ticks since the Unix epoch of (ISO) January 1st 1970, midnight, UTC (aka.
      /// Message.timestamp) into a string. By default "HH:mm:ss".
      formatLocalTime   : IFormatProvider -> EpochNanoSeconds -> string * Tokens.LiterateToken
      /// Converts a message into the appropriate tokens which can later be themed with colours.
      tokenise          : LiterateConsoleConf -> Message -> (string * Tokens.LiterateToken) seq
      /// Converts a token into the appropriate Foreground*Background colours. The default theme
      /// tries to emphasise the message template field values based on data type, make it easy to
      /// scan the output and find the most relevant information.
      theme             : Tokens.LiterateToken -> ConsoleColours
      /// Takes an object (console semaphore) and a list of string*colour pairs and writes them
      /// to the console with the appropriate colours.
      colourWriter      : obj -> ColouredText seq -> unit }

  module internal LiterateFormatting =
    open System.Text
    open Logary.Utils.FsMessageTemplates
    open Tokens

    let literateTokeniseException (options : LiterateConsoleConf) (typeName) (message) (stackTrace) =
      let windowsStackFrameLinePrefix = "   at "
      let monoStackFrameLinePrefix = "  at "
      let typeMessageNlStackTrace = String.Concat (typeName, ": ", message, Environment.NewLine, stackTrace)
      let exnLines = new System.IO.StringReader(typeMessageNlStackTrace)
      seq {
        let mutable line = exnLines.ReadLine()
        while not (isNull line) do
          if line.StartsWith(windowsStackFrameLinePrefix) || line.StartsWith(monoStackFrameLinePrefix) then
            // subtext
            yield Environment.NewLine, Subtext
            yield line, Subtext
          else
            // regular text
            yield Environment.NewLine, Text
            yield line, Text

          line <- exnLines.ReadLine()
      }

    /// Get the exceptions out from the Message (errors_)
    let literateTokeniseMessageExceptions (context : LiterateConsoleConf) message =
      let exns = Utils.Aether.Lens.getPartialOrElse Message.Lenses.errors_ [] message
      let getStringFromMapOrFail exnObjMap fieldName =
        match exnObjMap |> Map.find fieldName with
        | String m -> m
        | _ -> failwithf "Couldn't find %s in %A" fieldName exnObjMap
      let getStringFromMapOrDefault defaultIfMissing exnObjMap fieldName =
        match exnObjMap |> Map.tryFind fieldName with
        | Some (String m) -> m
        | _ -> defaultIfMissing

      match exns with
      | [] -> Seq.empty
      | values ->
        values
        |> List.choose (function | Object v -> Some v | _ -> None)
        |> List.map (fun exnObjMap ->
          let exnTypeName = getStringFromMapOrFail exnObjMap "type"
          let message = getStringFromMapOrFail exnObjMap "message"
          let stackTrace = getStringFromMapOrDefault "" exnObjMap "stackTrace"
          literateTokeniseException context exnTypeName message stackTrace
        )
        |> Seq.concat

    let literateTokeniseArray conf prop arrValue recurse =
      let items = match arrValue with Array items -> items | _ -> failwithf "cannot tokenise %A with %s" arrValue (prop.ToString())
      let mutable isFirst = true
      let valueTokens =
        items
        |> List.map (fun v -> seq {
            if not isFirst then
              yield ", ", Punctuation
            isFirst <- false
            yield! recurse conf prop v
          })
        |> Seq.concat

      seq {
          yield "[", Punctuation
          yield! valueTokens
          yield "]", Punctuation
      }

    let literateTokeniseObject conf prop objValue recurse =
      let stringValueMap = match objValue with Object svm -> svm | _ -> failwithf "cannot tokenise %A with %s" objValue (prop.ToString())
      let mutable isFirst = true
      let objectTokens =
        stringValueMap
        |> Map.toSeq
        |> Seq.rev
        |> Seq.map (fun (k, v) -> seq {
            if k <> "_typeTag" then
              if not isFirst then
                yield ", ", Punctuation
              isFirst <- false
              yield k, Subtext
              yield "=", Punctuation
              yield! recurse conf prop v
          })
        |> Seq.concat

      let maybeTypeTag = stringValueMap |> Map.tryFind ("_typeTag")
      let hasAnyValues = stringValueMap |> Map.exists (fun k v -> k <> "_typeTag")
      seq {
        match maybeTypeTag with
          | Some (String tt) ->
            yield tt, (if hasAnyValues then Subtext else OtherSymbol)
          | _ -> ()
        if hasAnyValues then
          if maybeTypeTag.IsSome then
            yield " ", Subtext
          yield "{", Punctuation
          yield! objectTokens
          yield "}", Punctuation
      }

    let literateTokeniseBinary conf prop binary =
      let bytes, contentType = match binary with Binary (x, y) -> x, y | _ -> failwithf "cannot tokenise %A with %s" binary (prop.ToString())
      let maxBytes = 15
      seq {
        yield (bytes |> Array.take(min bytes.Length maxBytes) |> Array.map (fun b -> b.ToString("X2")) |> String.Concat), StringSymbol
        if bytes.Length > maxBytes then
          yield "... (", Punctuation
          yield string bytes.Length, Subtext
          yield " bytes", Subtext
          yield ")", Punctuation
        yield " (", Punctuation
        yield contentType, Subtext
        yield ")", Punctuation
      }

    let literateTokeniseFraction (conf : LiterateConsoleConf) prop fraction =
      let x, y = match fraction with Fraction (x, y) -> x, y | _ -> failwithf "cannot tokenise %A with %s" fraction (prop.ToString())
      seq {
        yield x.ToString(conf.formatProvider), NumericSymbol
        yield "/", Punctuation
        yield y.ToString(conf.formatProvider), NumericSymbol
      }

    let rec literateTokeniseField (conf : LiterateConsoleConf)
                                  (prop : Property)
                                  (propValue : Field) =
      let fp = conf.formatProvider
      let recurseTokenise = fun c p v -> literateTokeniseField c p (Field (v, None))

      let (Field (value, units)) = propValue
      match units, value with
      | Some units, Value.Float f ->
        let f, unitsStr = Units.scale units f
        seq {
          yield f.ToString("N2", fp), NumericSymbol
          yield " ", Subtext
          yield unitsStr, Text
        }
      | None, Value.Float f ->
        seq { yield f.ToString(prop.Format, fp), NumericSymbol }

      | Some units, Value.Int64 i ->
        seq {
          let f, unitsStr = Units.scale units (float i)
          yield f.ToString(prop.Format, fp), NumericSymbol
          yield " ", Subtext
          yield unitsStr, Text
        }
      | None, Value.Int64 i ->
        seq { yield i.ToString(prop.Format, fp), NumericSymbol }

      | _, Value.String s ->
        seq { yield s, StringSymbol }
      | _, Value.Bool b ->
        seq { yield b.ToString().ToLowerInvariant(), KeywordSymbol }
      | _, Value.BigInt bi ->
        seq { yield bi.ToString(prop.Format, fp), NumericSymbol }
      | _, (Value.Binary (_) as binary) ->
        literateTokeniseBinary conf prop binary
      | _, (Value.Fraction (_) as fraction) ->
        literateTokeniseFraction conf prop fraction
      | _, (Value.Array _ as arr) ->
        literateTokeniseArray conf prop arr recurseTokenise
      | _, (Value.Object _ as o) ->
        literateTokeniseObject conf prop o recurseTokenise

    let literateTokenisePointValue (options : LiterateConsoleConf) (message : Message) = function
      | Event eventTemplate ->
        let textAndTokens = ResizeArray<string * LiterateToken>()
        Parser.parse(eventTemplate).Tokens
        |> Seq.collect (function
          | PropToken (_, prop) ->
            match Map.tryFind (PointName.ofSingle prop.Name) message.fields with
            | Some field ->
              literateTokeniseField options prop field
            | None ->
              seq { yield prop.ToString(), MissingTemplateField }
          | TextToken (_, text) ->
            seq { yield text, Text })

      | Gauge (Int64 nanos, Scaled (Seconds, scale))
        when scale = float Constants.NanosPerSecond ->

        let number, unitStr = (float nanos / float scale) |> Units.scale Seconds
        let format = if nanos < 1000L then "N0" else "N2"
        seq {
          yield message.name.ToString(), NameSymbol
          yield " took ", Subtext
          yield number.ToString(format, options.formatProvider), NumericSymbol
          yield " ", Subtext
          yield unitStr, Text
          yield " to execute.", Subtext
        }

      | Gauge (value, units)
        when message |> Message.hasTag KnownLiterals.SuppressPointValue ->
        seq {
          yield message.name.ToString(), NameSymbol
          yield " (M)", Subtext
          yield ":", Punctuation
          yield " ", Subtext
          let mutable first = true
          for KeyValue (name, Field (value, fieldUnits)) in message.fields do
            let field =
              let fu = fieldUnits |> Option.orDefault units
              Field (value, Some fu)
            if not first then yield " | ", Punctuation
            else first <- false
            yield PointName.format name, NameSymbol
            yield "=", Punctuation
            yield! literateTokeniseField options Property.Empty field
        }

      | Gauge (value, units) ->
        seq {
          yield message.name.ToString(), NameSymbol
          yield " (M)", Subtext
          yield ":", Punctuation
          yield " ", Subtext
          yield! literateTokeniseField options (Property.Empty) (Field (value, None))
          yield " ", Subtext
          yield Units.symbol units, Text }

      | Derived (value, units) ->
        seq {
          yield message.name.ToString(), NameSymbol
          yield " (MD)", Subtext
          yield ":", Punctuation
          yield " ", Subtext
          yield! literateTokeniseField options (Property.Empty) (Field (value, None))
          yield " ", Subtext
          yield Units.symbol units, Text }

    /// Split a structured message up into theme-able parts (tokens), allowing the
    /// final output to display to a user with colours to enhance readability.
    let literateDefaultTokeniser (options : LiterateConsoleConf) (message : Message) =
      let messageTokens = message.value |> literateTokenisePointValue options message
      let exceptionTokens = literateTokeniseMessageExceptions options message

      let getLogLevelToken = function
        | Verbose -> LevelVerbose
        | Debug -> LevelDebug
        | Info -> LevelInfo
        | Warn -> LevelWarning
        | Error -> LevelError
        | Fatal -> LevelFatal

      seq {
        yield "[", Punctuation
        yield options.formatLocalTime options.formatProvider message.timestamp
        yield " ", Subtext
        yield options.getLogLevelText message.level, getLogLevelToken message.level
        yield "] ", Punctuation
        yield! messageTokens
        yield! exceptionTokens
      }

    module DefaultTheme =
      let textColours = { foreground=ConsoleColor.White; background=None }
      let subtextColours = { foreground=ConsoleColor.Gray; background=None }
      let punctuationColours = { foreground=ConsoleColor.DarkGray; background=None }
      let levelVerboseColours = { foreground=ConsoleColor.Gray; background=None }
      let levelDebugColours = { foreground=ConsoleColor.Gray; background=None }
      let levelInfoColours = { foreground=ConsoleColor.White; background=None }
      let levelWarningColours = { foreground=ConsoleColor.Yellow; background=None }
      let levelErrorColours = { foreground=ConsoleColor.White; background=Some ConsoleColor.Red }
      let levelFatalColours = { foreground=ConsoleColor.White; background=Some ConsoleColor.Red }
      let keywordSymbolColours = { foreground=ConsoleColor.Blue; background=None }
      let numericSymbolColours = { foreground=ConsoleColor.Magenta; background=None }
      let stringSymbolColours = { foreground=ConsoleColor.Cyan; background=None }
      let otherSymbolColours = { foreground=ConsoleColor.Green; background=None }
      let nameSymbolColours = { foreground=ConsoleColor.Gray; background=None }
      let missingTemplateFieldColours = { foreground=ConsoleColor.Red; background=None }

      let theme = function
        | Tokens.Text -> textColours | Tokens.Subtext -> subtextColours | Tokens.Punctuation -> punctuationColours
        | Tokens.LevelVerbose -> levelVerboseColours | Tokens.LevelDebug -> levelDebugColours
        | Tokens.LevelInfo -> levelInfoColours | Tokens.LevelWarning -> levelWarningColours
        | Tokens.LevelError -> levelErrorColours | Tokens.LevelFatal -> levelFatalColours
        | Tokens.KeywordSymbol -> keywordSymbolColours | Tokens.NumericSymbol -> numericSymbolColours
        | Tokens.StringSymbol -> stringSymbolColours | Tokens.OtherSymbol -> otherSymbolColours
        | Tokens.NameSymbol -> nameSymbolColours | Tokens.MissingTemplateField -> missingTemplateFieldColours

    let consoleWriteLineColourParts (parts : ColouredText seq) =
        let originalForegroundColour = Console.ForegroundColor
        let originalBackgroundColour = Console.BackgroundColor

        // The console APIs are quite slow and clumsy. We avoid changing the foreground
        // and background colours whenever possible, which speeds things up a bit.
        let mutable currentForegroundColour = originalForegroundColour
        let mutable currentBackgroundColour = originalBackgroundColour

        let inline maybeResetBgColour (backgroundColour : ConsoleColor option) =
          match backgroundColour with
          | Some bgc ->
            if bgc <> currentBackgroundColour then
              Console.BackgroundColor <- bgc
              currentBackgroundColour <- bgc
          | None -> // "we don't have a specific colour, so leave (or reset to) the original/default"
            match currentBackgroundColour with
            | c when c = originalBackgroundColour -> ()
            | otherwise ->
              // calling reset here helps with different default background colours
              Console.ResetColor()
              currentForegroundColour <- Console.ForegroundColor
              currentBackgroundColour <- originalBackgroundColour

        parts |> Seq.iter (fun part ->
          maybeResetBgColour part.colours.background
          if currentForegroundColour <> part.colours.foreground then
            Console.ForegroundColor <- part.colours.foreground
            currentForegroundColour <- part.colours.foreground
          Console.Write(part.text)
        )
        if currentForegroundColour <> originalForegroundColour then
          Console.ForegroundColor <- originalForegroundColour
        maybeResetBgColour None
        Console.WriteLine()

    let consoleWriteColourPartsAtomically sem (parts : ColouredText seq) =
      lock sem <| fun _ -> consoleWriteLineColourParts parts

  /// Default console target configuration.
  let empty =
    { formatProvider  = Globalization.CultureInfo.CurrentCulture
      formatLocalTime = fun provider epochNanoSeconds ->
        let ts = DateTimeOffset.ofEpoch epochNanoSeconds
        ts.LocalDateTime.ToString("HH:mm:ss", provider),
        Tokens.Subtext
      getLogLevelText = function
        | Verbose -> "VRB"
        | Debug ->   "DBG"
        | Info ->    "INF"
        | Warn ->    "WRN"
        | Error ->   "ERR"
        | Fatal ->   "FTL"
      tokenise = LiterateFormatting.literateDefaultTokeniser
      theme = LiterateFormatting.DefaultTheme.theme
      colourWriter = LiterateFormatting.consoleWriteColourPartsAtomically }

  module internal Impl =
    open Hopac
    open Hopac.Infixes

    let loop (lcConf : LiterateConsoleConf)
             (ri : RuntimeInfo)
             (requests : RingBuffer<TargetMessage>)
             (shutdown : Ch<IVar<unit>>) =

      let output (data : ColouredText seq) : Job<unit> =
        Job.Scheduler.isolate <| fun _ ->
          lcConf.colourWriter Logary.Internals.Globals.consoleSemaphore data

      let rec loop () : Job<unit> =
        Alt.choose [
          shutdown ^=> fun ack ->
            ack *<= () :> Job<_>

          RingBuffer.take requests ^=> function
            | Log (logMsg, ack) ->
              job {
                try
                  do! lcConf.tokenise lcConf logMsg
                    |> Seq.map (fun (text, token) ->
                      { text = text; colours = lcConf.theme token })
                    |> output
                with e ->
                  do! output (seq {
                    yield { text="Error in Logary console target rendering: "; colours=LiterateFormatting.DefaultTheme.levelErrorColours }
                    yield { text=e.ToString(); colours=LiterateFormatting.DefaultTheme.textColours }
                  })
                  do! output (seq { yield { text=sprintf "%A" logMsg; colours=LiterateFormatting.DefaultTheme.subtextColours }})
                do! ack *<= ()
                return! loop ()
              }

            | Flush (ack, nack) ->
              job {
                do! Ch.give ack () <|> nack
                return! loop ()
              }
        ] :> Job<_>

      loop()

  [<CompiledName "Create">]
  let create conf name =
    TargetUtils.stdNamedTarget (Impl.loop conf) name

  /// Use with LogaryFactory.New( s => s.Target<LiterateConsole.Builder>() )
  type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
    let update (conf' : LiterateConsoleConf) : Builder =
      Builder(conf', callParent)

    /// Specify the formatting provider to use when formatting values to string
    member x.WithFormatProvider(fp : IFormatProvider) =
      update { conf with formatProvider = fp }

    /// Lets you specify how log levels are written out.
    member x.WithLevelFormatter(toStringFun : Func<LogLevel, string>) =
      update { conf with getLogLevelText = toStringFun.Invoke }

    new(callParent : FactoryApi.ParentCallback<_>) =
      Builder(empty, callParent)

    interface Logary.Target.FactoryApi.SpecificTargetConf with
      member x.Build name = create conf name

// boolean IsLogging() method, correct by excluded middle
#nowarn "25"

#if !NETSTANDARD1_5
/// The Debugger target is useful when running in Xamarin Studio or VS.
module Debugger =
  open System.Diagnostics
  open System.Runtime.CompilerServices
  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Internals
  open Logary.Target
  open Logary.Formatting

  type DebuggerConf =
    { formatter : StringFormatter }

    /// Create a new Debugger configuration with a given formatter (which
    /// formats how the Messages and Gauges/Derived-s are printed)
    [<CompiledName "Create">]
    static member create(?formatter) =
      { formatter = defaultArg formatter (StringFormatter.levelDatetimeMessagePathNl) }

  /// Default debugger configuration
  let empty =
    { formatter = StringFormatter.levelDatetimeMessagePathNl }

  module private Impl =

    let loop conf metadata
             (requests : RingBuffer<_>)
             (shutdown : Ch<_>) =
      let formatter = conf.formatter
      let offLevel = 6

      let rec loop () : Job<unit> =
        Alt.choose [
          shutdown ^=> fun ack ->
            ack *<= () :> Job<_>

          RingBuffer.take requests ^=> function
            //TODO: Debugger.IsLogging support https://docs.microsoft.com/en-us/dotnet/core/api/system.diagnostics.debugger
         
            | Log (message, ack) when Debugger.IsLogging() ->
              job {
                let path = PointName.format message.name
                Debugger.Log(offLevel, path, formatter.format message)
                do! ack *<= ()
                return! loop ()
              }

            | Log _ ->
              loop ()

            | Flush (ackCh, nack) ->
              job {
                do! Ch.give ackCh () <|> nack
                return! loop ()
              }

        ] :> Job<_>

      loop ()

  [<CompiledName "Create">]
  let create conf name =
    TargetUtils.stdNamedTarget (Impl.loop conf) name

  /// Use with LogaryFactory.New( s => s.Target<Debugger.Builder>() )
  type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

    /// Specify the formatting style to use when logging to the debugger
    member x.WithFormatter( sf : StringFormatter ) =
      ! (callParent <| Builder({ conf with formatter = sf }, callParent))

    new(callParent : FactoryApi.ParentCallback<_>) =
      Builder(empty, callParent)

    interface Logary.Target.FactoryApi.SpecificTargetConf with
      member x.Build name = create conf name

#endif

/// The file system module makes the File module testable by allowing the tests
/// to implement a custom implementation of the file system.
module FileSystem =
  open System
  open System.IO
  open System.Text.RegularExpressions

  /// A folder path is a fully qualified path to a directory/folder; it's
  /// should not be interpreted as a file and it should not just contain
  /// a non-rooted path, i.e. it should be a rooted path.
  type FolderPath = string

  /// A file name does not contain a path/folder structure as part of its
  /// contents. It's purely the name of the file and its extension (which is)
  /// part of the file name.
  type FileName = string

  /// A file path is a fully qualified name of a file, in that it contains the
  /// parent folders that the file is housed in.
  type FilePath = string

  /// How large the size is as judged from the file target (when a file is append)
  /// mode its metadata may not reflect the accurate size.
  type FileSize = int64

  /// A regex that can be used to glob for files.
  type FileNameRegex = Regex

  /// A file-system abstraction
  type FileSystem =
    /// Gets a file in the current directory
    abstract getFile : FilePath -> FileInfo
    /// Moves a file to another location, within the current (chrooted)
    /// directory.
    abstract moveFile : FileName -> FileName -> unit
    /// Gets a sub-folder
    abstract getFolder : FolderPath -> DirectoryInfo
    /// Finds all files in the current folder (that the file system is chrooted
    /// to).
    abstract glob : (*file name*)FileNameRegex -> FileInfo seq
    /// Deletes the file passed as a parameter.
    abstract deleteFile : FilePath -> unit
    /// Chroot the file system to a given folder to avoid accidental deletions
    /// or modifications outside the folder of interest.
    abstract chroot : FolderPath -> FileSystem
    /// Ensures that the currently rooted path exists (mkdir -p)
    abstract ensureCurrent : unit -> DirectoryInfo

  module Path =
    let combine (segments : string seq) =
      Path.Combine(segments |> Array.ofSeq)

  /// This file system implementation contains the necessary code to use the
  /// .Net System.IO abstractions.
  [<Sealed>]
  type DotNetFileSystem(root : FolderPath) =
    let ensureRooted (path : string) =
      if not (path.StartsWith root) then
        invalidOp (sprintf "Path '%s' is not within root '%s'" path root)
    let combEns (path : string) =
      let combined = Path.combine [ root; path ]
      ensureRooted combined
      combined
    let getFolder (path : FolderPath) =
      let combined = combEns path
      DirectoryInfo combined

    interface FileSystem with
      member x.getFile path =
        let combined = combEns path
        FileInfo combined
      member x.moveFile fileName nextFileName =
        let source = combEns fileName
        let target = combEns nextFileName
        File.Move(source, target)
      member x.getFolder path =
        getFolder path
      member x.glob fnr =
        let info = getFolder root
        info.GetFiles()
        |> Array.filter (fun file -> fnr.IsMatch file.Name)
        |> Seq.ofArray
      member x.deleteFile filePath =
        ensureRooted filePath
        File.Delete filePath
      member x.chroot subPath =
        let combined = getFolder subPath
        DotNetFileSystem combined.FullName :> FileSystem
      member x.ensureCurrent () =
        Directory.CreateDirectory root

  [<Sealed>]
  type CountingStream(inner : Stream, written : int64 ref) =
    inherit Stream()

    /// Updates the passed bytes refrence
    member x.updateBytesRef () =
      written := inner.Position

    override x.Write(buffer, offset, length) =
      written := !written + int64 length
      inner.Write(buffer, offset, length)

    override x.WriteAsync(buffer, offset, length, cancellationToken) =
      written := !written + int64 length
      inner.WriteAsync(buffer, offset, length, cancellationToken)
    //TODO: Begin/EndWrite seems to have been removed https://docs.microsoft.com/en-us/dotnet/core/api/system.io.stream
    #if !NETSTANDARD1_5
    override x.BeginWrite(buffer, offset, count, callback, state) =
      written := !written + int64 count
      inner.BeginWrite(buffer, offset, count, callback, state)
   
    override x.EndWrite(state) =
      inner.EndWrite(state)
    #endif
    // others:

    // others:
    override x.CanRead = inner.CanRead
    override x.CanWrite = inner.CanWrite
    override x.CanSeek = inner.CanSeek
    override x.Length = inner.Length
    override x.Position
      with get() = inner.Position
      and set value = inner.Position <- value
    override x.Flush() = inner.Flush()
    override x.Seek (offset, origin) = inner.Seek (offset, origin)
    override x.SetLength value = inner.SetLength value
    override x.Read(buffer, offset, count) = inner.Read(buffer, offset, count)


/// This is the File target of Logary. It can be both a rolling file and a
/// "normal", non-rolling file target, depending on the deletion and rotation
/// policies supplied.
module File =
  open System
  open System.IO
  open System.Text
  open System.Text.RegularExpressions
  open NodaTime
  open Hopac
  open Hopac.Infixes
  open Logary
  open Logary.Message
  open Logary.Target
  open Logary.Internals
  open FileSystem

  /// Bytes
  let B (n : int64) = n
  /// Kilobytes
  let KiB n = n * 1024L * 1024L
  /// Megabytes
  let MiB n = n * 1024L * 1024L * 1024L
  /// Gigabytes
  let GiB n = n * 1024L * 1024L * 1024L * 1024L

  /// These are the available rotation policies
  type RotationPolicy =
    /// The callback rotation policy is called whenever the check to rotate is
    /// made. In the callback, the programmer can choose to read the file
    /// attributes. Reading the files' attributes (such as last-modified) may
    /// have side-effects on Windows, such as flushing the page cache to disk
    /// and thereby fsyncing, so if you end up scheduling flushes often (less
    /// than every second or so), then consider to let your callback remember
    /// that last lookup of the file's attribute(s).
    | Callback of (FileInfo * FileSize -> bool)
    /// Rotate every given file size.
    | FileSize of maxSize:int64
    | FileAge of age:Duration

  /// This module contains some rotation policies you may like to use by
  /// default.
  module RotationPolicies =
    /// This module contains rotation policies that rotate the files after
    /// specific periods.
    module ByAge =
      let everyMinute = FileAge (Duration.FromMinutes 1L)
      let every10minutes = FileAge (Duration.FromMinutes 10L)
      let hourly = FileAge (Duration.FromHours 1)
      let daily = FileAge (Duration.FromHours 24)
      let weekly = FileAge (Duration.FromHours (7 * 24))
      let ofDuration dur = FileAge dur

    /// This module contains rotation policies that specify that files should
    /// be rotated every nth byte (e.g. every 100 MiB).
    module BySize =
      let everyMegabyte = FileSize (MiB 1L)
      let everyNthMegabyte n = FileSize (MiB n)
      let everyHundredMegabytes = everyNthMegabyte 100L
      let everyGigabyte = FileSize (GiB 1L)

    /// Implements a custom rotation policy signified by the passed callback
    /// function.
    let callback cb =
      Callback cb

  /// This is the result output of the deletion policy. Use these cases
  /// to decide the fate of the log file.
  type DeletionPolicyResult =
    /// The file should be deleted right away.
    | DeleteFile
    /// The file should be kept for now.
    | KeepFile

  /// A deletion policy gets the directory info that the logs are being stored
  /// in, the file info and should return a decision of whether to delete or
  /// keep the file.
  type DeletionPolicy = DirectoryInfo * FileInfo -> DeletionPolicyResult

  /// This module contains some pre-made deletion policies that serve to remove
  /// files from the logging folder, as to keep the disk-space of your disks
  /// free(-ish!).
  module DeletionPolicies =
    /// This deletion policy specifies that there are to be max n number of
    /// files present in the folder.
    let maxNoOfFiles (n : int16) : DeletionPolicy =
      fun (dirInfo, fileInfo) ->
        // TODO: implement
        KeepFile

    /// Deletes all log files (but not the current non-rotated one) older than
    /// the given duration.
    let olderThan (dur : Duration) : DeletionPolicy =
      fun (dirInfo, fileInfo) ->
        // TODO: implement
        KeepFile

    /// Deletes files in oldest-first order when the folder size has passed a
    /// given bytes-large threshold. Be aware that you should not use this
    /// policy alone when there are other services with the same policy (alone)
    /// logging to the same folder.
    let folderSize (bytesSize : int64) : DeletionPolicy =
      fun (dirInfo, fileInfo) ->
        // TODO: implement
        KeepFile

  /// The discriminated union that specifies whether to do file rotation in the
  /// File target.
  [<RequireQualifiedAccess>]
  type Rotation =
    /// Don't rotate the (single) log file, instead, keep appending to it.
    | SingleFile
    /// The rotation policy decides when it's time to rotate the file log. It's a
    /// list so that the user can supply multiple policies that all act on a
    /// first-come-first-served basis.
    /// The deletion policy decides when a given file should be deleted.
    /// You have to supply a non-empty rotation policy list, but you can supply
    /// an empty deletion policy.
    | Rotate of rotation:RotationPolicy list * deletion:DeletionPolicy list

  /// A module of oh-so-sweet file rolling and retention/deletion policies. Use
  /// these and sleep well at night.
  module Policies =
    open DeletionPolicies

    /// Rotates your log files when each file has reached 200 MiB.
    /// Lets your (for your service; single-purpose) logging folder grow to
    /// 2 GiB or letting your log files remain in the folder for two weeks
    /// tops.
    let ``rotate >200 MiB, delete if folder >2 GiB or file age >2 weeks`` =
      let rotate, delete =
        [ FileSize (MiB 200L) ],
        [ folderSize (GiB 2L)
          olderThan (Duration.FromHours (14 * 24)) ]
      Rotation.Rotate (rotate, delete)

    /// Rotates your log files when each file has reached 200 MiB.
    /// Lets your (for your service; single-purpose) logging folder grow to
    /// 2 GiB.
    let ``rotate >200 MiB, delete if folder >3 GiB`` =
      let rotate, delete =
        [ FileSize (MiB 200L) ],
        [ folderSize (GiB 3L) ]
      Rotation.Rotate (rotate, delete)

    /// Rotates your log files when each file has reached 200 MiB.
    /// Never deletes your log files. Remember to put other sorts of monitoring
    /// in place, e.g. by using Logary's health checks.
    let ``rotate >200 MiB, never delete`` =
      let rotate, delete =
        [ FileSize (MiB 200L) ],
        []
      Rotation.Rotate (rotate, delete)

  module internal P =
    // conflicts with Hopac.Hopac.run, Logary.Internals.TimeoutResult
    open FParsec

    type Token =
      | Placeholder of name:string
      | Lit of str:string

    let ph : Parser<Token, unit> =
      between (pstring "{" <?> "Placeholders must start with '{'")
              (pstring "}" <?> "Placeholders must end with '}'")
              (manyChars letter |>> Placeholder
               <?> "Only letters are supported as placeholders")

    let lit : Parser<Token, unit> =
      let invalids =
        [| yield! Path.GetInvalidFileNameChars()
           yield! "{}\n\r\b\a:;\\\"'".ToCharArray() |]

      let error =
        invalids
        |> Array.map (function
          | '\n' -> "\\n"
          | '\t' -> "\\t"
          | '\r' -> "\\r"
          | '\b' -> "\\b"
          | '\a' -> "\\a"
          | c when Char.IsControl c -> sprintf "%x" (int c)
          | c -> "'" + string c + "'")
        |> String.concat ", "
        |> sprintf "Literal fillers in the name may not contain any of these characters (%s)"

      many1Satisfy (fun c -> not (Array.contains c invalids)) |>> Lit
      <?> error

    let tokens : Parser<Token list, unit> =
      many1 (ph <|> lit)

    let combined : Parser<Token list, unit> =
      spaces >>. tokens .>> eof

    let parse spec =
      match run combined spec with
      | Success (results, _, _) ->
        Choice1Of2 results
      | Failure (error, _, _) ->
        Choice2Of2 error

    let private foldStr fph flit =
      fun tokens ->
        let sb = StringBuilder()
        let app (sb : StringBuilder) (value : string) = sb.Append value
        let folder sb = function
          | Placeholder ph -> app sb (fph ph)
          | Lit lit -> app sb (flit lit)
        tokens |> List.fold folder sb |> sprintf "%O"

    let format (known : Map<_, _>) (tokens : Token list) =
      foldStr (flip Map.find known) id tokens

    let formatRegex (known : Map<_, _>) (tokens : Token list) =
      foldStr (flip Map.find known) Regex.Escape tokens

  /// The naming specification gives the File target instructions on how to
  /// name files when they are created and rotated.
  type Naming =
    /// "{date}", "log" => "2016-10-15.log"
    Naming of spec:string * ext:string

    with
      /// Gives back a file-name without extention and an extension from the
      /// given RuntimeInfo.
      member x.format (ri : RuntimeInfo) : string * string =
        let (Naming (spec, ext)) = x
        let now = ri.clock.GetCurrentInstant().ToDateTimeOffset()
        let known =
          [ "service", ri.serviceName
            "date", now.ToString("yyyy-MM-dd")
            "datetime", now.ToString("yyyy-MM-ddTHH-mm-ssZ")
            "host", ri.host
          ] |> Map
        match P.parse spec with
        | Choice1Of2 tokens ->
          P.format known tokens, ext
        | Choice2Of2 error ->
          failwith error

      /// Gives back a file-name WITH extention given RuntimeInfo.
      member x.formatS (ri : RuntimeInfo) : string =
        x.format ri ||> sprintf "%s.%s"

      /// Gives back a regex that matches what this Naming spec will name files
      /// at any point in time (i.e. it will match any date/datetime file name)
      /// for the Naming spec.
      member x.regex (ri : RuntimeInfo) =
        let (Naming (spec, ext)) = x
        let known =
          [ "service", Regex.Escape ri.serviceName
            "date", @"\d{4}-\d{2}-\d{2}"
            "datetime", @"\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}Z"
            "host", Regex.Escape ri.host
          ] |> Map
        match P.parse spec with
        | Choice1Of2 tokens ->
          P.formatRegex known tokens
          |> fun specRegex -> sprintf @"^%s\.%s$" specRegex (Regex.Escape ext)
          |> Regex
        | Choice2Of2 error ->
          failwith error

  /// Module for deleting old log files.
  module internal Janitor =
    open System.IO

    let globFiles (ri : RuntimeInfo) (fs : FileSystem) (Naming (spec, ext) as naming) =
      fs.glob (naming.regex ri)
      |> Seq.map (fun fi ->
        let dir = Path.GetDirectoryName fi.FullName
        DirectoryInfo dir, fi)

    let iter (globber : unit -> seq<DirectoryInfo * FileInfo>) deleter policies () =
      seq {
        for dir, file in globber () do
          for policy in policies do
            match policy (dir, file) with
            | KeepFile -> ()
            | DeleteFile ->
              yield (file.FullName : FilePath) }
      |> Seq.iter deleter

    type T =
      | NullJanitor
      | LiveJanitor of stop:IVar<unit>

    let create (ri : RuntimeInfo) (fs : FileSystem) (naming : Naming) : Rotation -> Job<T> = function
      | Rotation.SingleFile
      | Rotation.Rotate (_, []) ->
        Job.result NullJanitor
      | Rotation.Rotate (_, deletions) ->
        let stopIV = IVar ()
        let glob () = globFiles ri fs naming
        let tickCh = Ch ()
        let reschedule () = timeOutMillis 5000 ^=> Ch.send tickCh
        let loop = Job.delay <| fun () ->
          Alt.choose [
            stopIV :> Alt<unit>
            (tickCh ^-> iter glob fs.deleteFile deletions) ^=> reschedule
          ]
        Job.foreverServer loop >>-. LiveJanitor stopIV

    let shutdown (t:T) : Job<unit> =
      match t with
      | NullJanitor ->
        Job.result()
      | LiveJanitor stopIV ->
        IVar.fill stopIV ()

  /// The file target configuration record. This is used to customise the behaviour
  /// of the file target. You should have a look at the README for more details
  /// on how the File target is built – its intended behaviour is rather well
  /// specified.
  type FileConf =
      /// Whether to buffer the string writer in this process' memory. Defaults to false,
      /// so that the textwriter that writes to the underlying file stream is continuously
      /// flushed.
    { inProcBuffer : bool
      /// Whether to force the operating system's page cache to flush to persistent
      /// storage for each log batch. By default this is false, but sending a
      /// Flush message to the target forces the page cache to be flushed to disk
      /// (and of course will also force-flush the in-process buffer if needed).
      /// If you specify the `writeThrough` flag (which is true by default)
      /// then you can have this flag as false.
      ///
      /// This flag corresponds to `Flush(true)`.
      flushToDisk : bool
      /// Whether the `FileStream` is opened with the flags FILE_FLAG_WRITE_THROUGH
      /// and FILE_FLAG_NO_BUFFERING. Defaults to true, to let this target model a
      /// transaction log. See https://support.microsoft.com/en-us/kb/99794 for more
      /// details. Turning this flag off gives a 21x throughput boost on SSD and
      /// a 140x throughput boost on spinning disk on Windows -
      /// ref https://ayende.com/blog/174785/fast-transaction-log-windows. Linux
      /// gets a 10x performance boost on SSD without this flag.
      writeThrough : bool
      /// Use the `Rotation` discriminated union to specify the policy for
      /// when to rotate the file. If you rotate the files, you can optionally
      /// choose to let this target delete files, too, by supplying a list of
      /// deletion policies.
      policies : Rotation
      /// Where to save the logs.
      logFolder : FolderPath
      /// What encoding to use for writing the text to the file.
      encoding  : Encoding
      /// The naming specification gives the File target instructions on how to
      /// name files when they are created and rotated.
      naming : Naming
      /// The file system abstraction to write to.
      fileSystem : FileSystem
      /// The formatter is responsible for writing to the textwriter in an async manner,
      /// however, the returned Alts do not have mean that any flush has been done.
      /// The returned value is hot, i.e. it's begun executing. Must never throw
      /// exceptions; instead place the exceptions in the returned promise.
      formatter : Message -> TextWriter -> Promise<unit>
      /// How many log messages to write in one go.
      batchSize : uint16
      /// How many times to try to recover a failed batch messages.
      attempts  : uint16 }

  /// The empty/default configuration for the Logary file target.
  let empty =
    { inProcBuffer = false
      flushToDisk  = false
      writeThrough = true
      policies     = Policies.``rotate >200 MiB, delete if folder >3 GiB``
      //https://github.com/dotnet/corefx/issues/5089
      logFolder    = Directory.GetCurrentDirectory() 
      encoding     = Encoding.UTF8
      naming       = Naming ("{service}-{date}", "log")
      fileSystem   = DotNetFileSystem("/var/lib/logs")
      formatter    = fun m tw ->
        let str = Formatting.StringFormatter.levelDatetimeMessagePathNl.format m
        tw.Write str
        Promise (())
      batchSize    = 100us
      attempts     = 3us }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module FileConf =
    /// Creates a new file config from the given folder path with the naming standard.
    let create (folderPath : FolderPath) (naming : Naming) =
      { empty with
          logFolder = folderPath
          fileSystem = DotNetFileSystem folderPath
          naming = naming }

  module internal Impl =
    type State =
      { underlying : FileStream
        /// The FileInfo as it were when the file was opened.
        fileInfo   : FileInfo
        writer     : TextWriter
        written    : int64 ref
        janitor    : Janitor.T }

      static member create (underlying, fi, writer, written) janitor =
        { underlying = underlying
          fileInfo   = fi
          writer     = writer
          written    = written
          janitor    = janitor }

    let applyRotation counter (fs : Stream) (policies : RotationPolicy list) : Stream =
      let rec iter state = function
        | [] -> state
        | Callback _ :: rest -> iter state rest // TODO: also needs counter
        | FileSize size :: rest ->
          let stream = new CountingStream(state, counter)
          stream.updateBytesRef()
          stream :> Stream
        | FileAge age :: rest -> iter state rest
      iter fs policies

    /// Takes the counter reference cell, and if there exists a rotation policy
    /// dependent on the 
    let applyStreamPolicies counter fs : Rotation -> Stream = function
      | Rotation.SingleFile ->
        fs
      | Rotation.Rotate ([] as rotation, _) ->
        fs
      | Rotation.Rotate (rotations, _) ->
        applyRotation counter fs rotations

    let openFile ri (fs : FileSystem) conf =
      let fileName = conf.naming.formatS ri
      let fi = fs.getFile fileName

      let fs =
        new FileStream(
          fi.FullName,
          FileMode.Append, // only append; seeks error
          FileAccess.Write, // request write access
          FileShare.Read, // allow concurrent readers (tail -f)
          0x1000, // default buffer size
          FileOptions.Asynchronous // IO overlapped
          ||| (if conf.writeThrough then FileOptions.WriteThrough else enum<FileOptions>(0)))

      let counter = ref 0L

      let writer =
        new StreamWriter(
          applyStreamPolicies counter fs conf.policies,
          conf.encoding)

      fs, fi, writer, counter

    let shouldRotate (clock : IClock) (conf : FileConf) (state : State) =
      let size = !state.written

      let rec apply state = function
        | [] ->
          false
        | Callback cb :: rest ->
          if cb (state.fileInfo, size) then true else apply state rest
        | FileSize maxSize :: rest ->
          if maxSize <= size then true else apply state rest
        | FileAge maxAge :: rest ->
          let created = Instant.FromDateTimeUtc(state.fileInfo.CreationTimeUtc)
          let age = created - clock.GetCurrentInstant()
          if maxAge <= age then true else apply state rest

      match conf.policies with
      | Rotation.SingleFile
      | Rotation.Rotate ([], _) ->
        false
      | Rotation.Rotate (policies, _) ->
        policies |> apply state

    let flushWriter (state : State) : Job<unit> =
      Job.fromUnitTask state.writer.FlushAsync

    let flushToDisk (state : State) : Job<unit> =
      Job.Scheduler.isolate (fun _ -> state.underlying.Flush true)

    let flushAndCloseFile (state : State) : Job<unit> =
      flushWriter state >>=. flushToDisk state >>- fun () ->
      state.writer.Dispose()

    let shutdownState (state : State) : Job<unit> =
      flushAndCloseFile state >>=.
      Janitor.shutdown state.janitor

    /// Writes all passed requests to disk, handles acks and flushes.
    let writeRequests (ilogger : Logger) conf state (reqs : TargetMessage[]) =
      // `completed` can throw
      let ack (completed, ack) = completed >>=. IVar.fill ack ()
      let ackAll acks = acks |> Seq.map ack |> Job.conIgnore
      let flush (ack, nack) = Ch.give ack () <|> nack
      let flushAll flushes = flushes |> Seq.map flush |> Job.conIgnore

      let acks = ResizeArray<_>() // Updated by the loop.
      let flushes = ResizeArray<_>() // Updated by the loop.
      let mutable forceFlush = false // Updated by the loop.

      for m in reqs do
        match m with
        | Log (message, ack) ->
          // Write and save the ack for later.
          acks.Add (conf.formatter message state.writer, ack)
          // Invariant: always flush fatal messages.
          if message.level = Fatal then
            ilogger.debug (eventX "Got fatal message; scheduling disk flush.")
            forceFlush <- true
        | Flush (ackCh, nack) ->
          ilogger.debug (eventX "Scheduling disk flush.")
          flushes.Add (ackCh, nack)
          // Invariant: calling Flush forces a flush to disk.
          forceFlush <- true

      let flushWriter = // Invariant: only schedule after writing messages.
        memo <|
          // Unless forceFlush is included in the check, there can be state in
          // the TextWriter that is not written to disk.
          if not conf.inProcBuffer || forceFlush then
            flushWriter state
          else
            Job.result ()

      let flushPageCache = // Invariant: only schedule after all messages written
        memo <|
          if conf.flushToDisk || forceFlush then
            flushToDisk state >>=.
            ilogger.debugWithBP (eventX "Flushed to disk.")
          else
            Job.result ()

      // after the batch, consider flushing the writer
      flushWriter >>=.
      // then consider flushing the page cache
      flushPageCache >>=.
      // finally, start acking; clients not wanting to wait for the above
      // can handle their own concurrency outside of the target
      ackAll acks >>=.
      // deal with all the flush requests
      flushAll flushes

    let writeRequestsSafe ilogger conf state reqs =
      Job.tryIn (writeRequests ilogger conf state reqs)
                (Choice1Of2 >> Job.result)
                (function
                | :? IOException as e ->
                  Job.result (Choice2Of2 e)
                | other ->
                  Job.raises other)

    let loop (conf : FileConf)
             (ri : RuntimeInfo)
             (requests : RingBuffer<_>)
             (shutdown : Ch<_>)
             (saveWill : obj -> Job<unit>)
             (lastWill : obj option) =

      let rotateCh = Ch ()

      // For type checking purposes, shadows parameters.
      let saveWill (msgs : TargetMessage[], recoverCount : uint16) =
        saveWill (box (msgs, recoverCount))

      // For type checking purposes.
      let lastWill =
        lastWill |> Option.map (fun x ->
          let (msgs : TargetMessage[], recoverCount : uint16) = unbox x
          msgs, recoverCount)

      let shutdownState =
        Logger.timeJobSimple ri.logger "shutdownState" shutdownState

      // In this state the File target opens its file stream and checks if it
      // progress to the recovering state.
      let rec init lastWill =
        let fs = conf.fileSystem.chroot conf.logFolder
        fs.ensureCurrent () |> ignore
        Janitor.create ri fs conf.naming conf.policies >>-
        State.create (openFile ri fs conf) >>=
        fun state ->
          match lastWill with
          | Some (msgs, recoverCount) ->
            (msgs, recoverCount) ||> recovering state
          | None ->
            checking state

      // In this state we try to write as many log messages as possible.
      and running (state : State) : Job<unit> =
        Alt.choose [
          shutdown ^=> fun ack ->
            ri.logger.debugWithBP (eventX "Shutting down file target (starting shutdownState)") >>=.
            shutdownState state >>=.
            ack *<= ()

          // TODO: handle scheduled rotation
          rotateCh ^=> fun () -> rotating state

          RingBuffer.takeBatch (conf.batchSize) requests ^=> fun reqs ->
            writeRequestsSafe ri.logger conf state reqs >>= function
              | Choice1Of2 () ->
                checking state
              | Choice2Of2 err ->
                ri.logger.logWithAck Error (
                  eventX "IO Exception while writing to file. Batch size is {batchSize}."
                  >> setField "batchSize" conf.batchSize
                  >> addExn err)
                >>= id
                >>=. saveWill (reqs, 1us)
                >>=. Job.raises err
        ] :> Job<_>

      // In this state we verify the state of the file.
      and checking (state : State) =
        if shouldRotate ri.clock conf state then rotating state
        else running state

      // In this state we do a single rotation.
      and rotating (state : State) =
        shutdownState state >>-
        (fun () ->
          let fnNoExt = Path.GetFileNameWithoutExtension
          let parse n =
            match Int32.TryParse n with
            | false, _ -> None
            | true, value -> Some value
          let currName = fnNoExt state.fileInfo.Name
          let nextName, nextExt = conf.naming.format ri
          let targetName =
            if nextName <> currName then
              sprintf "%s.%s" nextName nextExt
            else
              conf.fileSystem.glob (conf.naming.regex ri)
              |> Seq.map (fun fi -> fnNoExt fi.Name)
              |> Seq.filter (String.contains currName)
              |> Seq.map (fun name ->
                match Regex.``match`` @"\d{3}$" name with
                | None -> None
                | Some g -> parse g.[0].Value)
              |> Seq.filter Option.isSome
              |> Seq.map Option.get
              |> Seq.sortDescending
              |> Seq.tryPick Some
              |> Option.fold (fun s t -> t + 1) 0
              |> sprintf "%s-%03i" currName
          conf.fileSystem.moveFile state.fileInfo.Name targetName) >>=.
        init None

      and recovering (state : State) (lastBatch : TargetMessage[]) = function
        // Called with 1, 2, 3 – 1 is first time around.
        | recoverCount when recoverCount <= conf.attempts ->
          writeRequestsSafe ri.logger conf state lastBatch >>= function
            | Choice1Of2 () ->
              checking state
            | Choice2Of2 ex ->
              ri.logger.logWithAck Error (
                eventX "Attempt {attempts} failed, trying again shortly."
                >> Message.setField "attempts" recoverCount)
              >>= id
              >>=. saveWill (lastBatch, recoverCount + 1us)
              >>=. Job.raises ex

        | recoverCount ->
          ri.logger.logWithAck Fatal (
            eventX "Could not recover in {attempts} attempts."
            >> Message.setField "attempts" recoverCount)
          >>= id
          >>=. Job.raises (Exception "File recovery failed, crashing out.")

      init lastWill

  /// Create a new File target through this.
  [<CompiledName "Create">]
  let create conf name =
    TargetUtils.willAwareNamedTarget (Impl.loop conf) name
    
  /// Use with LogaryFactory.New(s => s.Target<File.Builder>())
  type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
    let update (conf' : FileConf) : Builder =
      Builder(conf', callParent)

    // NOTE: this code is particular to the default configuration values of empty

    member x.BufferInProc() =
      update { conf with inProcBuffer = true }

    member x.FlushToDisk() =
      update { conf with flushToDisk = true }

    member x.NoWriteThrough() =
      update { conf with writeThrough = false }

    member x.Rotate_Gt300GiB_DeleteIfFolder_Gt2GiB_Or_FileAge_Gt2Wks() =
      update { conf with policies = Policies.``rotate >200 MiB, delete if folder >2 GiB or file age >2 weeks`` }

    member x.Rotate_Gt300GiB_DeleteIfFolder_Gt3GiB() =
      update { conf with policies = Policies.``rotate >200 MiB, delete if folder >3 GiB`` }
      
    member x.Rotate_Gt200MiB_Delete_Never() =
      update { conf with policies = Policies.``rotate >200 MiB, never delete`` }

    member x.LogFolder folderPath =
      update { conf with logFolder = folderPath }

    member x.Encoding enc =
      update { conf with encoding = enc }

    member x.Naming (spec, ext) =
      update { conf with naming = Naming (spec, ext) }

    member x.FileSystem fs =
      update { conf with fileSystem = fs }

    member x.Formatter (f : Func<Message, TextWriter, System.Threading.Tasks.Task>) =
      update { conf with
                formatter = fun m tw -> memo (Job.fromUnitTask (fun () -> f.Invoke(m, tw))) }

    member x.BatchSize size =
      { conf with batchSize = size }

    member x.Attempts maxAttempts =
      { conf with attempts = maxAttempts }

    new(callParent : FactoryApi.ParentCallback<_>) =
      Builder(empty, callParent)

    member x.Done() =
      ! (callParent x)

    interface Logary.Target.FactoryApi.SpecificTargetConf with
      member x.Build name = create conf name
