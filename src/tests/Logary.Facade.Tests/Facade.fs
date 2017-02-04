module Logary.Facade.Tests

open System
open Expecto
open Logary.Facade
open Logary.Facade.Literals
open Logary.Facade.Literate
open Logary.Facade.LiterateExtensions

type ColouredBits = string * ConsoleColor

let getColouredBitsForMessage options (message: Message) messageParts =
  let timestampDto = DateTimeOffset(DateTimeOffset.ticksUTC message.timestamp, TimeSpan.Zero)
  let timestampTimeString = timestampDto.LocalDateTime.ToString("HH:mm:ss", options.formatProvider)
  let logLevel, logLevelToken = message.level, LiterateFormatting.tokeniseLogLevel message.level
  let expectedTokens = [ "[",                               Punctuation
                         timestampTimeString,              Subtext
                         " ",                               Subtext
                         options.getLogLevelText logLevel,  logLevelToken
                         "] ",                              Punctuation ]
                         @ messageParts
                         @ [ Environment.NewLine, Text ]
  let applyTheme (text : string, token) = text, options.theme token
  expectedTokens |> Seq.map applyTheme |> Seq.toList

type Expect =
  /// Asserts the `LiterateConsoleTarget` will render the expected colour parts to the output writer.
  /// Note the `expectedParts` must contain everthing that will be rendered (including the default
  /// output parts such as the line prefix "[00:00:00 INF] ").
  static member literateRenderedPartsEqual (lrc, message, expectedParts) =
    let writtenParts = ResizeArray<ColouredBits>()
    let inMemoryOutputWriter (colouredBits : ColouredBits seq) = writtenParts.AddRange colouredBits
    let target = LiterateConsoleTarget([|"Facade"; "Tests"|], Verbose, renderingContext=lrc, outputWriter=inMemoryOutputWriter) :> Logger
    let applyTheme (text : string, token) = text, lrc.options.theme token
    target.logWithAck message.level (fun _ -> message) |> Async.RunSynchronously
    Expect.equal (expectedParts |> Seq.map applyTheme |> Seq.toList)
                 (writtenParts |> Seq.toList)
                 "literate rendered parts must match"

  static member literateMessagePartsEqualOldCtor (template, fields, expectedMessageParts, ?options) =
    let options = defaultArg options (LiterateOptions.create())
    let writtenColourBits = ResizeArray<ColouredBits>()
    let inMemoryOutputWriter (colouredBits : ColouredBits seq) = writtenColourBits.AddRange colouredBits
    let logLevel = Info
    let message = { Message.event logLevel template with fields = fields }
    let targetViaOldCtor = LiterateConsoleTarget(name = [|"Facade"; "Tests"|],
                                                 minLevel = Verbose,
                                                 options = options,
                                                 outputWriter = inMemoryOutputWriter) :> Logger
    targetViaOldCtor.logWithAck logLevel (fun _ -> message) |> Async.RunSynchronously
    let expectedColourBits = getColouredBitsForMessage options message expectedMessageParts
    let actualColourBits = writtenColourBits |> Seq.toList
    Expect.equal expectedColourBits actualColourBits "literate rendered message parts must match"

  static member literateMessagePartsEqualNewCtor (template, fields, expectedMessageParts, ?options) =
    let lrc = LiterateRenderingContext.create (?options=options)
    let options = lrc.options
    let writtenParts = ResizeArray<ColouredBits>()
    let inMemoryOutputWriter (colouredBits : ColouredBits seq) = writtenParts.AddRange colouredBits
    let now = Global.timestamp ()
    let logLevel = Info
    let message = { Message.event logLevel template with fields = fields; timestamp = now }
    let targetViaNewCtor = LiterateConsoleTarget(name = [|"Facade"; "Tests"|],
                                                 minLevel = Verbose,
                                                 renderingContext = lrc,
                                                 outputWriter = inMemoryOutputWriter) :> Logger
    targetViaNewCtor.logWithAck logLevel (fun _ -> message) |> Async.RunSynchronously
    let expectedColourBits = getColouredBitsForMessage options message expectedMessageParts
    let actualColourBits = writtenParts |> Seq.toList
    Expect.equal expectedColourBits actualColourBits "literate rendered message parts must match"

  /// Asserts the LiterateConsoleTarget will render the expected message template parts.
  /// Note the `expectedMessageParts` should NOT include the default output parts such as
  /// the message line prefix "[00:00:00 INF] ".
  static member literateMessagePartsEqual (template, fields, expectedMessageParts, ?options) =
    Expect.literateMessagePartsEqualOldCtor (template, fields, expectedMessageParts, ?options = options)
    Expect.literateMessagePartsEqualNewCtor (template, fields, expectedMessageParts, ?options = options)

[<Tests>]
let tests =
  let msg = Message.event Info "hi {name}"
  // don't print to console
  Global.initialise { Global.defaultConfig with getLogger = Targets.create Fatal }

  testList "generic" [
    testProperty "DateTime" <| fun (dt : DateTime) ->
      let ticks = dt |> DateTime.timestamp |> DateTime.ticksUTC
      let recreated = DateTime(ticks, DateTimeKind.Utc).Ticks
      Expect.equal recreated (dt.Ticks) "should equal on ticks after conversion"

    testProperty "DateTimeOffset" <| fun (ts : DateTimeOffset) ->
      let ticks = ts |> DateTimeOffset.timestamp |> DateTimeOffset.ticksUTC
      let recreated = DateTimeOffset(ticks, TimeSpan.Zero).Ticks
      Expect.equal recreated (ts.Ticks) "should equal after conversion"

    testCase "event" <| fun _ ->
      Message.event Info "hi {name}" |> ignore

    testCase "gauge" <| fun _ ->
      Message.gauge 5L "Scalar" |> ignore

    testCase "setSingleName" <| fun _ ->
      let msg = msg |> Message.setSingleName "Logary.Other"
      Expect.equal msg.name [| "Logary"; "Other" |] "Should have name set"

    testCase "setName" <| fun _ ->
      let msg = msg |> Message.setName [| "Logary"; "Other" |]
      Expect.equal msg.name [| "Logary"; "Other" |] "Should have name set"

    testCase "setFieldValue" <| fun _ ->
      let msg = msg |> Message.setFieldValue "fv" 33
      Expect.equal msg.fields.["fv"] (box 33) "Should have field"

    testCase "setTimestamp" <| fun _ ->
      let now = DateTimeOffset.UtcNow
      let msg = msg |> Message.setTimestamp (now |> DateTimeOffset.timestamp)
      Expect.equal msg.timestamp (now |> DateTimeOffset.timestamp) "Should have timestamp set"

    testCase "setLevel" <| fun _ ->
      Expect.equal msg.level Info "Initially Info"
      let msg = msg |> Message.setLevel Warn
      Expect.equal msg.level Warn "Should have Warn level now"

    testCase "logSimple" <| fun _ ->
      let logger = LiterateConsoleTarget([| "Facade"; "Tests" |], Warn) :> Logger
      Message.event Info "Aliens descended" |> logger.logSimple

    testProperty "Loggers.create" <| fun level ->
      Targets.create level |> ignore

    testCase "Targets.create and logSimple" <| fun () ->
      let logger = [| "Facade"; "Tests" |] |> Targets.create Error
      Message.event Verbose "Hi {name}" |> Message.setField "name" "haf" |> logger.logSimple

    testList "EventTemplateParser" [

      testCase "extract invalid property tokens as text" <| fun _ ->
        let template = "Hi {ho:##.###}, we're { something going on } special"
        let fields = Map [ "ho", box "hola"; "something going on", box "very" ]
        let tokens = EventTemplateParser.parse template |> Seq.toList
        Expect.equal (tokens |> Seq.length) 5 "Extracts 4 texts parts and 1 property part"
        Expect.equal tokens
                     [ EventTemplateParser.TextToken ("Hi ")
                       EventTemplateParser.PropToken ("ho", "##.###")
                       EventTemplateParser.TextToken (", we're ") // <- a quirk of the parser
                       EventTemplateParser.TextToken ("{ something going on }")
                       EventTemplateParser.TextToken (" special") ]
                     "Should extract correct name and format parts"

      testCase "handles evil property strings correctly" <| fun _ ->
        let template = "{a}{b}{  c}{d  } {e} fghij {k}} {l} m {{}}"
        // validity:     +  +  --    --   +         +    +     --
        let expectedPropertyNames = ["a"; "b"; "e"; "k"; "l"]
        let subject = EventTemplateParser.parse template |> Seq.toList
        Expect.equal (subject |> List.choose (function EventTemplateParser.PropToken (n,f) -> Some n | _ -> None))
                     expectedPropertyNames
                     "Should extract relevant names"

      testCase "treats `{{` and `}}` as escaped braces, resulting in a single brace in the output" <| fun _ ->
        let template = "hello {{@nonPropWithFormat:##}} {{you}} {are} a {{NonPropNoFormat}}"
        let tokens = EventTemplateParser.parse template |> Seq.toList
        Expect.equal tokens
                      [ EventTemplateParser.TextToken("hello {@nonPropWithFormat:##} {you} ")
                        EventTemplateParser.PropToken("are", null)
                        EventTemplateParser.TextToken(" a {NonPropNoFormat}") ]

                     "double open or close braces are escaped"
    ]

    testCase "literate tokenises with field names correctly" <| fun _ ->
      let template = "Added {item} to cart {cartId} for {loginUserId} who now has total ${cartTotal}"
      let itemName, cartId, loginUserId, cartTotal = "TicTacs", Guid.NewGuid(), "AdamC", 123.45M
      let fields = Map [ "item", box itemName
                         "cartId", box cartId
                         "loginUserId", box loginUserId
                         "cartTotal", box cartTotal ]
      let options = { LiterateOptions.create() with printTemplateFieldNames = true }
      let expectedMessageParts =
        [ "Added ",               Text
          "[item] ",              Subtext
          "TicTacs",              StringSymbol
          " to cart ",            Text
          "[cartId] ",            Subtext
          cartId.ToString(),      OtherSymbol
          " for ",                Text
          "[loginUserId] ",       Subtext
          loginUserId,            StringSymbol
          " who now has total $", Text
          "[cartTotal] ",         Subtext
          cartTotal.ToString(),   NumericSymbol ]
      Expect.literateMessagePartsEqual (template, fields, expectedMessageParts, options)

    testCase "literate can tokenise an empty message template" <| fun _ ->
      let emptyFields = Map.empty<string,obj>
      Expect.literateMessagePartsEqual ("", emptyFields, [])

    testCase "literate tokenises missing fields with the `MissingTemplateField` token" <| fun _ ->
      let template = "Added {item} to cart {cartId:X} for {loginUserId} who now has total ${cartTotal}"
      let itemName, cartId, loginUserId, cartTotal = "TicTacs", Guid.NewGuid(), "AdamC", 123.45M
      let fields = Map [ "item", box itemName
                         // "cartId", box cartId
                         // "loginUserId", box loginUserId
                         "cartTotal", box cartTotal ]
      let options = { LiterateOptions.create() with printTemplateFieldNames = false }
      let expectedMessageParts =
        [ "Added ",               Text
          "TicTacs",              StringSymbol
          " to cart ",            Text
          "{cartId:X}",           MissingTemplateField
          " for ",                Text
          "{loginUserId}",        MissingTemplateField
          " who now has total $", Text
          cartTotal.ToString(),   NumericSymbol ]
      Expect.literateMessagePartsEqual (template, fields, expectedMessageParts)

    testCase "literate tokeniser uses the options `getLogLevelText()` correctly" <| fun _ ->
      let customGetLogLevelText = function Verbose->"A"|Debug->"B"|Info->"C"|Warn->"D"|Error->"E"|Fatal->"F"
      let options = { LiterateOptions.createInvariant() with getLogLevelText = customGetLogLevelText }
      let tokeniseContext = { options = options; tokeniser = LiterateTokeniser.create () }
      let now = Global.timestamp ()
      let nowDto = DateTimeOffset(DateTimeOffset.ticksUTC now, TimeSpan.Zero)
      let msg level = Message.event level "" |> fun m -> { m with timestamp = now }
      let nowTimeString = nowDto.LocalDateTime.ToString("HH:mm:ss")
      [ Verbose,  LevelVerbose,   "A"
        Debug,    LevelDebug,     "B"
        Info,     LevelInfo,      "C"
        Warn,     LevelWarning,   "D"
        Error,    LevelError,     "E"
        Fatal,    LevelFatal,     "F" ]
      |> List.iter (fun (logLevel, expectedLevelToken, expectedText) ->
        let tokens = LiterateFormatting.tokeniseMessage tokeniseContext (msg logLevel) |> List.ofSeq
        Expect.equal tokens [ "[",            Punctuation
                              nowTimeString,  Subtext
                              " ",            Subtext
                              expectedText,   expectedLevelToken
                              "] ",           Punctuation ]
                      (sprintf "expect log level %A to render as token %A with text %s" logLevel expectedLevelToken expectedText)
      )

    testPropertyWithConfig FsCheck.Config.QuickThrowOnFailure "literate default tokeniser uses the options `formatProvider` correctly" <| fun (amount: decimal, date: DateTimeOffset) ->
      [ "fr-FR"; "da-DK"; "de-DE"; "en-AU"; "en-US"; ]
      |> List.iter (fun cultureName ->
        let options = { LiterateOptions.createInvariant() with
                          formatProvider = System.Globalization.CultureInfo(cultureName) }
        let template = "As of {date:F}, you have a balance of {amount:C2}"
        let fields = Map [ "amount", box amount
                           "date", box date ]
        let expectedMessageParts =
          [ "As of ",                                       Text
            date.ToString("F", options.formatProvider),     OtherSymbol
            ", you have a balance of ",                     Text
            amount.ToString("C2", options.formatProvider),  NumericSymbol ]

        Expect.literateMessagePartsEqual (template, fields, expectedMessageParts, options)
      )

    testCase "literate default tokeniser can yield exception tokens from the 'errors' and 'exn' fields, even with an empty template" <| fun _ ->
      let template = ""
      let exceptionForExnField = exn "exn field"
      let exceptionObjListForErrorsField = [ box (exn "errors field 1"); box (exn "errors field 2") ]
      let fields = Map [ FieldExnKey,    box exceptionForExnField
                         FieldErrorsKey, box exceptionObjListForErrorsField ]
      let nl = Environment.NewLine
      let expectedMessageParts =
        [ nl, Text //<-- empty message will just start rendering exceptions on a new line
          "System.Exception: exn field", Text //<-- The exception
          nl, Text
          "System.Exception: errors field 1", Text
          nl, Text
          "System.Exception: errors field 2", Text
        ]
      Expect.literateMessagePartsEqual (template, fields, expectedMessageParts)

    testCase "literate tokenises without field names correctly" <| fun _ ->
      let template = "Added {item} to cart {cartId} for {loginUserId} who now has total ${cartTotal}"
      let itemName, cartId, loginUserId, cartTotal = "TicTacs", Guid.NewGuid(), "AdamC", 123.45M
      let fields = Map [ "item", box itemName
                         "cartId", box cartId
                         "loginUserId", box loginUserId
                         "cartTotal", box cartTotal ]
      let options = { LiterateOptions.create() with printTemplateFieldNames = false }
      let expectedMessageParts =
        [ "Added ",               Text
          "TicTacs",              StringSymbol
          " to cart ",            Text
          cartId.ToString(),      OtherSymbol
          " for ",                Text
          loginUserId,            StringSymbol
          " who now has total $", Text
          cartTotal.ToString(),   NumericSymbol ]
      Expect.literateMessagePartsEqual (template, fields, expectedMessageParts, options)

    testCase "customising the literate output with an arbitrary prefix and suffix works" <| fun _ ->
      let template = "Hello from {where}"
      let whereText = "the other side"
      let fields = Map [ "where", box whereText ]
      
      let myCustomMessageTokeniser context message =
        seq { yield "<arbitrary prefix> ", Text
              yield! context.tokeniser.pointValue context message.fields message.value
              yield! context.tokeniser.messageExns context message
              yield " <arbitrary suffix>", Text }
      let customRenderingContext = LiterateRenderingContext.create (messageTokeniser=myCustomMessageTokeniser)

      let expectedParts =
        [ "<arbitrary prefix> ",  Text
          "Hello from ",          Text
          "the other side",       StringSymbol
          " <arbitrary suffix>",  Text
          Environment.NewLine,    Text ] // the LiterateConsoleTarget appends a newline after each message

      let message = { Message.event Info template with fields = fields }
      Expect.literateRenderedPartsEqual (customRenderingContext, message, expectedParts)

    testCase "format template with invalid property correctly" <| fun _ ->
      // spaces are not valid in property names, so the 'property' is treated as text
      let str = "Hi {ho}, we're { something going on  } special"
      let fields = Map [ "ho", box "hola"; "something going on", box "very" ]
      let now = Global.timestamp ()
      let nowDto = DateTimeOffset.ticksUTC now |> fun ticks -> DateTimeOffset(ticks, TimeSpan.Zero)
      let msg =
        Message.event Info str
        |> fun m -> { m with fields = fields; timestamp = now }
        |> Message.setSingleName "Logary.Facade.Tests"
      let subject = Formatting.defaultFormatter msg
      Expect.equal subject
                   (sprintf "[I] %s: Hi hola, we're { something going on  } special [Logary.Facade.Tests]\n - something going on: very" (nowDto.ToString("o")))
                   "Should format correctly"
  ]
