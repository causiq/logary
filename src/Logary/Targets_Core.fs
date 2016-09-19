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

module LiterateConsole =
  open System
  open System.Globalization
  open Logary
  open Logary.Formatting
  open Logary.Target
  open Logary.Internals
  open Hopac

  /// The output tokens, which can be potentially coloured.
  type LiterateToken =
    | Text | Subtext
    | Punctuation
    | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
    | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
    | MissingTemplateField

  /// Console configuration structure
  type LiterateConsoleConf =
    { formatProvider    : IFormatProvider
      getLogLevelText   : LogLevel -> string
      tokenise          : LiterateConsoleConf -> Message -> (string * LiterateToken) list 
      theme             : LiterateToken -> ConsoleColor
      colourWriter      : obj -> (string * ConsoleColor) list -> unit }

  module internal LiterateFormatting =
    open System.Text
    open Logary.Utils.FsMessageTemplates

    let literateExceptionColorizer (options : LiterateConsoleConf) (typeName) (message) (stackTrace) =
      let stackFrameLinePrefix = "   "
      let messageNlStackTrace = System.String.Join(Environment.NewLine, [ typeName; ": "; message; stackTrace ])
      use exnLines = new System.IO.StringReader(messageNlStackTrace)
      let rec go lines =
        match exnLines.ReadLine() with
        | null ->
          List.rev lines // finished reading
        | line ->
          if line.StartsWith(stackFrameLinePrefix) then
            // subtext
            go ((Environment.NewLine, Text) :: ((line, Subtext) :: lines))
          else
            // regular text
            go ((Environment.NewLine, Text) :: ((line, Text) :: lines))
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
          @ [ Environment.NewLine, Text ]
        )
        |> List.concat

    let rec literateFormatField (options : LiterateConsoleConf)
                                (prop : Property)
                                (propValue : Field) =
      let value, units = match propValue with Field (v, u) -> v, u | _ -> failwith "never happens"
      match value with
      | Value.Array items -> [ "todo", OtherSymbol ] // TODO:
      | Value.String s -> [ s, StringSymbol ]
      | Value.Bool b -> [ b.ToString(), KeywordSymbol ]
      | Value.Float f -> [ f.ToString(), NumericSymbol ]
      | Value.Int64 i -> [ i.ToString(), NumericSymbol ]
      | Value.BigInt bi -> [ bi.ToString(), NumericSymbol ]
      | Value.Binary (x, y) -> [ "todo", OtherSymbol ] // TODO:
      | Value.Fraction (x, y) -> [ "todo", OtherSymbol ] // TODO:
      | Value.Object o -> [ o.ToString(), OtherSymbol ] // TODO: recurse

    let literateFormatValue (options : LiterateConsoleConf) (fields : Map<PointName, Field>) = function
      | Event eventTemplate ->
        let themedParts = ResizeArray<string * LiterateToken>()
        let matchedFields = ResizeArray<string>()
        let themedParts =
          Parser.parse(eventTemplate).Tokens
          |> Seq.collect (function
            | PropToken (_, prop) ->
              match Map.tryFind (PointName.ofSingle prop.Name) fields with
              | Some field ->
                matchedFields.Add prop.Name
                literateFormatField options prop field
              | None ->
                [ prop.ToString(), MissingTemplateField ]
            | TextToken (_, text) ->
              [ text, Text ])

        Set.ofSeq matchedFields, List.ofSeq themedParts

      | Derived (value, units)
      | Gauge (value, units) ->
        Set.empty, [ Units.formatValue value, NumericSymbol
                     Units.symbol units, KeywordSymbol ]

    /// Split a structured message up into theme-able parts (tokens), allowing the
    /// final output to display to a user with colours to enhance readability.
    let literateDefaultTokenizer (options : LiterateConsoleConf) (message : Message) : (string * LiterateToken) list =
      let formatLocalTime (utcTicks : EpochNanoSeconds) =
        DateTimeOffset(utcTicks, TimeSpan.Zero).LocalDateTime.ToString("HH:mm:ss", options.formatProvider),
        Subtext

      let themedMessageParts =
        message.value |> literateFormatValue options message.fields |> snd

      let themedExceptionParts =
        let exnParts = literateColorizeExceptions options message
        if not exnParts.IsEmpty then
          [ Environment.NewLine, Text ]
          @ exnParts
          @ [ Environment.NewLine, Text ]
        else []

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

    let consoleWriteLineColourParts (parts : (string * ConsoleColor) list) =
        let originalColour = Console.ForegroundColor
        let mutable currentColour = originalColour
        parts |> List.iter (fun (text, color) ->
          if currentColour <> color then
            Console.ForegroundColor <- color
            currentColour <- color
          Console.Write(text)
        )
        if currentColour <> originalColour then
          Console.ForegroundColor <- originalColour
        Console.WriteLine()

    let consoleWriteColourPartsAtomically sem (parts : (string * ConsoleColor) list) =
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
              | Text -> ConsoleColor.White
              | Subtext -> ConsoleColor.Gray
              | Punctuation -> ConsoleColor.DarkGray
              | LevelVerbose -> ConsoleColor.Gray
              | LevelDebug -> ConsoleColor.Gray
              | LevelInfo -> ConsoleColor.White
              | LevelWarning -> ConsoleColor.Yellow
              | LevelError -> ConsoleColor.Red
              | LevelFatal -> ConsoleColor.Red
              | KeywordSymbol -> ConsoleColor.Blue
              | NumericSymbol -> ConsoleColor.Magenta
              | StringSymbol -> ConsoleColor.Cyan
              | OtherSymbol -> ConsoleColor.Green
              | NameSymbol -> ConsoleColor.Gray
              | MissingTemplateField -> ConsoleColor.Red
      colourWriter = LiterateFormatting.consoleWriteColourPartsAtomically }


  module internal Impl =
    open Hopac
    open Hopac.Infixes

    let loop (lcConf : LiterateConsoleConf)
             (ri : RuntimeInfo)
             (requests : RingBuffer<TargetMessage>)
             (shutdown : Ch<IVar<unit>>) =

      let output (data : (string * ConsoleColor) list) : Job<unit> =
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
                let themedParts = tokens |> List.map (fun (text, token) -> text, lcConf.theme token)
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