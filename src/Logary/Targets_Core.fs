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

  /// Colours in hex
  type ConsoleColours =
    { foregroundColor : int
      backgroundColor : int }

  /// Console configuration structure.
  type ConsoleConf =
    { formatter : StringFormatter
      colorMap  : (ConsoleColours -> LogLevel -> ConsoleColours) option }

    [<CompiledName "Create">]
    static member create formatter =
      { formatter = formatter
        colorMap  = None }

  /// Default console target configuration.
  let empty =
    { formatter = StringFormatter.levelDatetimeMessagePath
      colorMap  = None (* fun col line -> 0x000000 black *) }

  [<CompiledName "Create">]
  let create conf name =
    // TODO: coloured console output
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

    /// TODO: implement!
    member x.Colourise() =
      ! (callParent <| Builder(conf, callParent))

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
      /// Converts a log level into a display string.
      getLogLevelText   : LogLevel -> string
      /// Converts a message into the appropriate tokens which can later be themed with colours.
      tokenise          : LiterateConsoleConf -> Message -> (string * Tokens.LiterateToken) list 
      /// Converts a token into the appropriate Foreground*Background colours.
      theme             : Tokens.LiterateToken -> ConsoleColours
      /// Takes an object (console semaphore) and a list of string*colour pairs and writes them
      /// to the console with the appropriate colours.
      colourWriter      : obj -> ColouredText list -> unit }

  module internal LiterateFormatting =
    open System.Text
    open Logary.Utils.FsMessageTemplates
    open Tokens

    let literateExceptionColorizer (options : LiterateConsoleConf) (typeName) (message) (stackTrace) =
      let stackFrameLinePrefix = "   "
      let messageNlStackTrace = String.Concat (typeName, ": ", message, Environment.NewLine, stackTrace)
      use exnLines = new System.IO.StringReader(messageNlStackTrace)
      let rec go lines =
        match exnLines.ReadLine() with
        | null ->
          List.rev lines // finished reading
        | line ->
          if line.StartsWith(stackFrameLinePrefix) then
            // subtext
            go ((line, Subtext) :: (Environment.NewLine, Text) :: lines)
          else if String.IsNullOrWhiteSpace line then
            go lines
          else
            // regular text
            go ((line, Text) :: (Environment.NewLine, Text) :: lines)
      go []

    let literateColorizeExceptions (context : LiterateConsoleConf) message =
      let exns = Utils.Aether.Lens.getPartialOrElse Message.Lenses.errors_ [] message
      let getStringFromMapOrFail exnObjMap fieldName =
        match exnObjMap |> Map.find fieldName with
        | String m -> m
        | _ -> failwithf "Couldn't find %s in %A" fieldName exnObjMap

      match exns with
      | [] -> []
      | values ->
        values
        |> List.choose (function | Object v -> Some v | _ -> None)
        |> List.map (fun exnObjMap ->
          let exnTypeName = getStringFromMapOrFail exnObjMap "type"
          let message = getStringFromMapOrFail exnObjMap "message"
          let stackTrace = getStringFromMapOrFail exnObjMap "stackTrace"
          literateExceptionColorizer context exnTypeName message stackTrace
        )
        |> List.concat

    let rec formatValueWithProvider (formatProvider : IFormatProvider) = function
      | String s -> s
      | Bool true -> "true"
      | Bool false -> "false"
      | Float f -> f.ToString(formatProvider)
      | Int64 i -> i.ToString(formatProvider)
      | BigInt bi -> bi.ToString(formatProvider)
      | Binary (b, ct) -> System.BitConverter.ToString b |> fun s -> s.Replace("-", "")
      | Fraction (n, d) -> System.String.Format(formatProvider, "{0}/{1}", n, d)
      | Array values -> String.Concat ["["; values |> List.map (formatValueWithProvider formatProvider) |> String.concat ", "; "]"]
      | Object m ->
          [ "["
            Map.toList m
                |> List.map (fun (key, value) -> key + "=" + (formatValueWithProvider formatProvider value))
                |> String.concat ", "
            "]" ]
          |> String.Concat

    let rec literateFormatField (conf : LiterateConsoleConf)
                                (prop : Property)
                                (propValue : Field) =
      let value, units = match propValue with Field (v, u) -> v, u
      let fp = conf.formatProvider
      match value with
      | Value.Array items ->
        [ "[ ", Punctuation ]
          @ (items |> List.collect (fun v -> literateFormatField conf prop (Field (v, None))))
          @ [ " ]", Punctuation ]
      | Value.String s -> [ s, StringSymbol ]
      | Value.Bool b -> [ b.ToString(fp), KeywordSymbol ]
      | Value.Float f -> [ f.ToString(prop.Format, fp), NumericSymbol ]
      | Value.Int64 i -> [ i.ToString(prop.Format, fp), NumericSymbol ]
      | Value.BigInt bi -> [ bi.ToString(prop.Format, fp), NumericSymbol ]
      | Value.Binary (x, y) -> [ "todo (binary)", OtherSymbol ] // TODO:
      | Value.Fraction (x, y) -> [ "todo (fraction)", OtherSymbol ] // TODO:
      | Value.Object stringValueMap ->
        let objectTokens =
          stringValueMap
          |> Map.remove "_typeTag" // type tag is rendered separately
          |> Map.toArray
          |> fun valArray ->
            valArray |> Array.mapi (fun i (k, v) -> seq {
              yield k, Subtext
              yield ": ", Punctuation
              yield! literateFormatField conf prop (Field (v, None))
              if i < (valArray.Length - 1) then
                yield ", ", Punctuation
            })
          |> Seq.concat
          |> List.ofSeq

        seq {
          match stringValueMap.TryFind("_typeTag") with
            | Some (String tt) -> yield tt, OtherSymbol; yield " ", Text
            | _ -> ()
          if not objectTokens.IsEmpty then
            yield "[ ", Punctuation
            yield! objectTokens 
            yield " ]", Punctuation 
        }
        |> List.ofSeq

    let literateFormatValue (options : LiterateConsoleConf) (message : Message) = function
      | Event eventTemplate ->
        let themedParts = ResizeArray<string * LiterateToken>()
        let matchedFields = ResizeArray<string>()
        let themedParts =
          Parser.parse(eventTemplate).Tokens
          |> Seq.collect (function
            | PropToken (_, prop) ->
              match Map.tryFind (PointName.ofSingle prop.Name) message.fields with
              | Some field ->
                matchedFields.Add prop.Name
                literateFormatField options prop field
              | None ->
                [ prop.ToString(), MissingTemplateField ]
            | TextToken (_, text) ->
              [ text, Text ])

        Set.ofSeq matchedFields, List.ofSeq themedParts

      | Derived (value, units) ->
        Set.empty, [ "Metric (derived) ", Subtext
                     formatValueWithProvider options.formatProvider value, NumericSymbol
                     " ", Subtext
                     Units.symbol units, Text
                     " (", Punctuation
                     message.name.ToString(), Text
                     ")", Punctuation ]
      | Gauge (value, units) ->
        Set.empty, [ "Metric (guage) ", Subtext
                     formatValueWithProvider options.formatProvider value, NumericSymbol
                     " ", Subtext
                     Units.symbol units, Text
                     " (", Punctuation
                     message.name.ToString(), Text
                     ")", Punctuation ]

    /// Split a structured message up into theme-able parts (tokens), allowing the
    /// final output to display to a user with colours to enhance readability.
    let literateDefaultTokenizer (options : LiterateConsoleConf) (message : Message) : (string * LiterateToken) list =
      let formatLocalTime (utcTicks : EpochNanoSeconds) =
        DateTimeOffset(utcTicks, TimeSpan.Zero).LocalDateTime.ToString("HH:mm:ss", options.formatProvider),
        Subtext

      let themedMessageParts =
        message.value |> literateFormatValue options message |> snd
        
      let themedExceptionParts = literateColorizeExceptions options message

      let getLogLevelToken = function
        | Verbose -> LevelVerbose
        | Debug -> LevelDebug
        | Info -> LevelInfo
        | Warn -> LevelWarning
        | Error -> LevelError
        | Fatal -> LevelFatal

      [ "[", Punctuation
        formatLocalTime message.timestamp
        " ", Subtext
        options.getLogLevelText message.level, getLogLevelToken message.level
        "] ", Punctuation ]
      @ themedMessageParts
      @ themedExceptionParts

    let consoleWriteLineColourParts (parts : ColouredText list) =
        let originalForegroundColour = Console.ForegroundColor
        let originalBackgroundColour = Console.BackgroundColor
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
              Console.BackgroundColor <- originalBackgroundColour
              currentBackgroundColour <- originalBackgroundColour
          
        parts |> List.iter (fun part ->
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

    let consoleWriteColourPartsAtomically sem (parts : ColouredText list) =
      lock sem <| fun _ -> consoleWriteLineColourParts parts

  /// Default console target configuration.
  let empty =
    { formatProvider = Globalization.CultureInfo.CurrentCulture
      getLogLevelText = function
              | Debug ->    "DBG"
              | Error ->    "ERR"
              | Fatal ->    "FTL"
              | Info ->     "INF"
              | Verbose ->  "VRB"
              | Warn ->     "WRN"
      tokenise = LiterateFormatting.literateDefaultTokenizer
      theme = function
              | Tokens.Text ->  { foreground=ConsoleColor.White; background=None }
              | Tokens.Subtext -> { foreground=ConsoleColor.Gray; background=None }
              | Tokens.Punctuation -> { foreground=ConsoleColor.DarkGray; background=None }
              | Tokens.LevelVerbose -> { foreground=ConsoleColor.Gray; background=None }
              | Tokens.LevelDebug -> { foreground=ConsoleColor.Gray; background=None }
              | Tokens.LevelInfo -> { foreground=ConsoleColor.White; background=None }
              | Tokens.LevelWarning -> { foreground=ConsoleColor.Yellow; background=None }
              | Tokens.LevelError -> { foreground=ConsoleColor.Red; background=None }
              | Tokens.LevelFatal -> { foreground=ConsoleColor.White; background=Some ConsoleColor.Red }
              | Tokens.KeywordSymbol -> { foreground=ConsoleColor.Blue; background=None }
              | Tokens.NumericSymbol -> { foreground=ConsoleColor.Magenta; background=None }
              | Tokens.StringSymbol -> { foreground=ConsoleColor.Cyan; background=None }
              | Tokens.OtherSymbol -> { foreground=ConsoleColor.Green; background=None }
              | Tokens.NameSymbol -> { foreground=ConsoleColor.Gray; background=None }
              | Tokens.MissingTemplateField -> { foreground=ConsoleColor.Red; background=None }
      colourWriter = LiterateFormatting.consoleWriteColourPartsAtomically }


  module internal Impl =
    open Hopac
    open Hopac.Infixes

    let loop (lcConf : LiterateConsoleConf)
             (ri : RuntimeInfo)
             (requests : RingBuffer<TargetMessage>)
             (shutdown : Ch<IVar<unit>>) =

      let output (data : ColouredText list) : Job<unit> =
        Job.Scheduler.isolate <| fun _ ->
          lcConf.colourWriter Logary.Internals.Globals.consoleSemaphore data
        
      let rec loop () : Job<unit> =
        Alt.choose [
          shutdown ^=> fun ack ->
            ack *<= () :> Job<_>

          RingBuffer.take requests ^=> function
            | Log (logMsg, ack) ->
              job {
                let tokens = lcConf.tokenise lcConf logMsg
                let themedParts = tokens |> List.map (fun (text, token) ->
                                                        let colours = lcConf.theme token
                                                        { text=text; colours=colours })
                do! output themedParts
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