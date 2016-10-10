namespace Logary.Targets

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
          Job.awaitUnitTask (tw.WriteLineAsync str)

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
                  do! Job.awaitUnitTask (writer.FlushAsync())

                do! ack *<= ()
                return! loop ()
              }

            | Flush (ack, nack) ->
              job {
                do! Job.awaitUnitTask (twConf.output.FlushAsync())
                do! Job.awaitUnitTask (twConf.error.FlushAsync())
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
      let recurseTokenise = fun c p v -> literateTokeniseField c p (Field (v, None))
      let value, units = match propValue with Field (v, u) -> v, u
      let fp = conf.formatProvider
      match value with
      | Value.Array _ as arr -> literateTokeniseArray conf prop arr recurseTokenise
      | Value.String s -> seq { yield s, StringSymbol }
      | Value.Bool b -> seq { yield b.ToString().ToLowerInvariant(), KeywordSymbol }
      | Value.Float f -> seq { yield f.ToString(prop.Format, fp), NumericSymbol }
      | Value.Int64 i -> seq { yield i.ToString(prop.Format, fp), NumericSymbol }
      | Value.BigInt bi -> seq { yield bi.ToString(prop.Format, fp), NumericSymbol }
      | Value.Binary (_) as binary -> literateTokeniseBinary conf prop binary
      | Value.Fraction (_) as fraction -> literateTokeniseFraction conf prop fraction
      | Value.Object _ as o -> literateTokeniseObject conf prop o recurseTokenise

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

      | Derived (value, units) ->
        seq {
          yield "Metric (derived) ", Subtext
          yield! literateTokeniseField options (Property.Empty) (Field (value, None))
          yield " ", Subtext
          yield Units.symbol units, Text
          yield " (", Punctuation
          yield message.name.ToString(), Text
          yield ")", Punctuation }
      | Gauge (value, units) ->
        seq {
          yield "Metric (guage) ", Subtext
          yield! literateTokeniseField options (Property.Empty) (Field (value, None))
          yield " ", Subtext
          yield Units.symbol units, Text
          yield " (", Punctuation
          yield message.name.ToString(), Text
          yield ")", Punctuation }

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
      formatLocalTime = fun provider utcTicks ->
                          DateTimeOffset(utcTicks, TimeSpan.Zero).LocalDateTime.ToString("HH:mm:ss", provider),
                          Tokens.Subtext
      getLogLevelText = function
        | Verbose ->  "VRB"
        | Debug ->    "DBG"
        | Info ->     "INF"
        | Warn ->     "WRN"
        | Error ->    "ERR"
        | Fatal ->    "FTL"
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
                      { text=text; colours=lcConf.theme token })
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
