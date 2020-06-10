module Logary.Targets.ColouredConsole

  open System
  open Logary
  open Logary.Internals
  open Logary.Configuration.Target
  open Hopac

  [<Struct>]
  type ConsoleColours =
    { foreground: ConsoleColor
      background: ConsoleColor option }

  [<Struct>]
  type ColouredText =
    { text: string
      colours: ConsoleColours }

  type Tokens = Logary.Formatting.MessageTemplates.Formatting.Literate.LiterateToken

  /// Console configuration structure
  type LiterateConsoleConf =
    { formatProvider: IFormatProvider
      /// Converts a log level into a display string. By default: VRB, DBG, INF, WRN, ERR, FTL
      getLogLevelText: LogLevel -> string
      /// Formats the ticks since the Unix epoch of (ISO) January 1st 1970, midnight, UTC (aka.
      /// Message.timestamp) into a string. By default "HH:mm:ss".
      formatLocalTime: IFormatProvider -> EpochNanoSeconds -> string * LiterateToken
      /// Converts a message into the appropriate tokens which can later be themed with colours.
      tokenise: LiterateConsoleConf -> Message -> (string * LiterateToken) seq
      /// Converts a token into the appropriate Foreground*Background colours. The default theme
      /// tries to emphasise the message template field values based on data type, make it easy to
      /// scan the output and find the most relevant information.
      theme: LiterateToken -> ConsoleColours
      /// Takes an object (console semaphore) and a list of string*colour pairs and writes them
      /// to the console with the appropriate colours.
      colourWriter: obj -> ColouredText seq -> unit }

  open Logary.Formatting.MessageTemplates
  open Logary.Formatting.MessageTemplates.Formatting

  module Tokenisers =
    let private getLogLevelToken = function
      | Verbose -> LevelVerbose
      | Debug -> LevelDebug
      | Info -> LevelInfo
      | Warn -> LevelWarning
      | Error -> LevelError
      | Fatal -> LevelFatal

    let private nl, destr, maxDepth = Environment.NewLine, MessageWriter.defaultDestr, 10

    /// Split a structured message up into theme-able parts (tokens), allowing the
    /// final output to display to a user with colours to enhance readability.
    let defaultTokeniser (options: LiterateConsoleConf) (message: Message) =
      seq {
        yield "[", Punctuation
        yield options.formatLocalTime options.formatProvider message.timestamp
        yield " ", Subtext
        yield options.getLogLevelText message.level, getLogLevelToken message.level
        yield "] ", Punctuation
        yield! Literate.tokeniseTemplateWithGauges options.formatProvider destr message

        yield " ", Subtext
        yield "<", Punctuation
        yield string message.name, Subtext
        yield ">", Punctuation

        yield! Literate.tokeniseExceptions options.formatProvider nl message
      }

    /// The extended tokeniser also prints all fields, context and gauges as separate lines in the output.
    /// Split a structured message up into theme-able parts (tokens), allowing the
    /// final output to display to a user with colours to enhance readability.
    let extendedTokeniser (options: LiterateConsoleConf) (message: Message) =
      seq {
        yield "[", Punctuation
        yield options.formatLocalTime options.formatProvider message.timestamp
        yield " ", Subtext
        yield options.getLogLevelText message.level, getLogLevelToken message.level
        yield "] ", Punctuation
        yield! Literate.tokeniseTemplateWithGauges options.formatProvider destr message

        yield " ", Subtext
        yield "<", Punctuation
        yield string message.name, Subtext
        yield ">", Punctuation

        let writeState = { provider = options.formatProvider; idManager = RefIdManager ()}
        yield! Literate.tokeniseContext writeState nl destr message
        yield! Literate.tokeniseExceptions options.formatProvider nl message
      }

  module Themes =
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

    let defaultTheme = function
      | Tokens.Text -> textColours
      | Tokens.Subtext -> subtextColours
      | Tokens.Punctuation -> punctuationColours
      | Tokens.LevelVerbose -> levelVerboseColours
      | Tokens.LevelDebug -> levelDebugColours
      | Tokens.LevelInfo -> levelInfoColours
      | Tokens.LevelWarning -> levelWarningColours
      | Tokens.LevelError -> levelErrorColours
      | Tokens.LevelFatal -> levelFatalColours
      | Tokens.KeywordSymbol -> keywordSymbolColours
      | Tokens.NumericSymbol -> numericSymbolColours
      | Tokens.StringSymbol -> stringSymbolColours
      | Tokens.OtherSymbol -> otherSymbolColours
      | Tokens.NameSymbol -> nameSymbolColours
      | Tokens.MissingTemplateField -> missingTemplateFieldColours

    let internal consoleWriteLineColourParts (parts: ColouredText seq) =
      let originalForegroundColour = Console.ForegroundColor
      let originalBackgroundColour = Console.BackgroundColor

      // The console APIs are quite slow and clumsy. We avoid changing the foreground
      // and background colours whenever possible, which speeds things up a bit.
      let mutable currentForegroundColour = originalForegroundColour
      let mutable currentBackgroundColour = originalBackgroundColour

      let inline maybeResetBgColour (backgroundColour: ConsoleColor option) =
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

    let defaultThemeWriter sem (parts: ColouredText seq) =
      lock sem (fun () -> consoleWriteLineColourParts parts)

  /// Default console target configuration.
  let empty =
    { formatProvider = Globalization.CultureInfo.CurrentCulture
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
      tokenise = Tokenisers.defaultTokeniser
      theme = Themes.defaultTheme
      colourWriter = Themes.defaultThemeWriter }

  module internal Impl =
    open Hopac
    open Hopac.Infixes

    let loop (lcConf: LiterateConsoleConf) (api: TargetAPI) =
      let output (data: ColouredText seq): Job<unit> =
        Job.Scheduler.isolate <| fun _ ->
          lcConf.colourWriter (api.runtime.getConsoleSemaphore ()) data

      let rec loop (): Job<unit> =
        Alt.choose [
          RingBuffer.take api.requests ^=> function
            | Log (logMsg, ack) ->
              job {
                try
                  do! lcConf.tokenise lcConf logMsg
                    |> Seq.map (fun (text, token) ->
                      { text = text; colours = lcConf.theme token })
                    |> output
                with e ->
                  do! output (seq {
                    yield { text="Error in Logary console target rendering: "; colours=Themes.levelErrorColours }
                    yield { text=e.ToString(); colours=Themes.textColours }
                  })
                  do! output (seq { yield { text=sprintf "%A" logMsg; colours=Themes.subtextColours }})
                do! ack *<= ()
                return! loop ()
              }

            | Flush (ack, nack) ->
              job {
                do! IVar.fill ack ()
                return! loop ()
              }

          api.shutdownCh ^=> fun ack ->
            ack *<= ()

        ] :> Job<_>

      loop()

  [<CompiledName "Create">]
  let create conf name =
    TargetConf.createSimple (Impl.loop conf) name

  /// Use with `LogaryFactory.New(s => s.Target<LiterateConsole.Builder>("literate", x => ...))`
  type Builder(conf, callParent: ParentCallback<Builder>) =
    let update (conf' : LiterateConsoleConf): Builder =
      Builder(conf', callParent)

    /// Specify the formatting provider to use when formatting values to string
    member x.WithFormatProvider(fp: IFormatProvider) =
      update { conf with formatProvider = fp }

    /// Lets you specify how log levels are written out.
    member x.WithLevelFormatter(toStringFun: Func<LogLevel, string>) =
      update { conf with getLogLevelText = toStringFun.Invoke }

    /// Specifies how to tokenise the Message values
    member x.WithTokeniser(tokeniser: Func<LiterateConsoleConf, Message, seq<string * LiterateToken>>) =
      let wrapped conf message = tokeniser.Invoke(conf, message)
      update { conf with tokenise = wrapped }

    member x.WithExtendedTokeniser() =
      update { conf with tokenise = Tokenisers.extendedTokeniser }

    member x.WithSingleLineTokeniser() =
      update { conf with tokenise = Tokenisers.defaultTokeniser }

    member x.Done() =
      ! (callParent x)

    new(callParent: ParentCallback<_>) =
      Builder(empty, callParent)

    interface SpecificTargetConf with
      member x.Build name = create conf name

