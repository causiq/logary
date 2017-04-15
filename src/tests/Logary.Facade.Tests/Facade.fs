module Logary.Facade.Tests

open System
open Expecto
open Logary.Facade
open Logary.Facade.Literals
open Logary.Facade.Literate
open Logary.Facade.LiterateFormatting

type internal TemplateToken =
  | TextToken of string
  | PropToken of name:string * format:string
  with
    override x.ToString() =
      match x with
      | TextToken s -> "TEXT("+s+")"
      | PropToken (n,f) -> "PROP("+n+":"+f+")"

let internal parseTemplateTokens template =
  let tokens = ResizeArray<TemplateToken>()
  let foundText t = tokens.Add (TextToken(t))
  let foundProp (p: FsMtParser.Property) =
    tokens.Add (PropToken(p.name, p.format))
  FsMtParser.parseParts template foundText foundProp
  tokens |> List.ofSeq

type ColouredText = string * ConsoleColor

type DateTimeOffset with
  member x.ToLiterateTime (?options : LiterateOptions) =
    let options = defaultArg options (LiterateOptions.create())
    x.ToLocalTime().ToString("HH:mm:ss", options.formatProvider)

type System.Int64 with
  member x.ToDateTimeOffsetUtc() = DateTimeOffset(DateTimeOffset.ticksUTC x, TimeSpan.Zero)
  member x.ToLiterateTimeString (options : LiterateOptions) = x.ToDateTimeOffsetUtc().ToLiterateTime(options)

type Expect =
  static member literateMessagePartsEqual (template, fields, expectedMessageParts, ?options) =
    let options = defaultArg options (LiterateOptions.create())
    let logLevel, logLevelToken = Info, LevelInfo
    let message = { Message.event logLevel template with fields = fields
                                                         name = [|"X"; "Y"|] }

    // Insteading of writing out to the console, write to an in-memory list so we can capture the values
    let writtenParts = ResizeArray<ColouredText>()
    let writtenPartsOutputWriter _ (bits : ColouredText list) = writtenParts.AddRange bits

    let target = LiterateConsoleTarget(name = [|"Facade";"Tests";"literateMessagePartsEqual"|],
                                       minLevel = Verbose,
                                       options = options,
                                       outputWriter = writtenPartsOutputWriter) :> Logger

    target.logWithAck Verbose (fun _ -> message) |> Async.RunSynchronously

    let expectedTokens = [  "[",                                              Punctuation
                            message.timestamp.ToLiterateTimeString(options),  Subtext
                            " ",                                              Subtext
                            options.getLogLevelText logLevel,                 logLevelToken
                            "] ",                                             Punctuation ]
                            @ expectedMessageParts
                            @ [ " ",                                          Subtext
                                "<",                                          Punctuation
                                "X.Y",                                        Subtext
                                ">",                                          Punctuation
                                Environment.NewLine,                          Text ]

    let actualParts = writtenParts |> List.ofSeq
    let expectedParts = expectedTokens |> List.map (fun (s, t) -> s, options.theme t)
    Expect.sequenceEqual actualParts expectedParts "literate tokenised parts must be correct"
    
  static member literateCustomTokenisedPartsEqual (message, customTokeniser, expectedTokens) =
    let options = LiterateOptions.create()
    let writtenParts = ResizeArray<ColouredText>()
    let writtenPartsOutputWriter _ (bits : ColouredText list) = writtenParts.AddRange bits
    let target = LiterateConsoleTarget(name = [|"Facade";"Tests"|],
                                       minLevel = Verbose,
                                       options = options,
                                       ?literateTokeniser = customTokeniser,
                                       outputWriter = writtenPartsOutputWriter) :> Logger

    target.logWithAck Verbose (fun _ -> message) |> Async.RunSynchronously
    
    let actualParts = writtenParts |> List.ofSeq
    let expectedParts = expectedTokens |> List.map (fun (s, t) -> s, options.theme t)

    Expect.sequenceEqual actualParts expectedParts "literate custom tokenised parts must be correct"

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

    testCase "LiterateConsoleTarget logSimple" <| fun _ ->
      let logger = LiterateConsoleTarget([| "Facade"; "Tests" |], Warn) :> Logger
      Message.event Info "Aliens descended" |> logger.logSimple

    testProperty "Loggers.create" <| fun level ->
      Targets.create level |> ignore

    testCase "Targets.create and logSimple" <| fun () ->
      let logger = [| "Facade"; "Tests" |] |> Targets.create Error
      Message.event Verbose "Hi {name}" |> Message.setField "name" "haf" |> logger.logSimple

    testCase "extract invalid property tokens as text" <| fun _ ->
      let template = "Hi {ho:##.###}, we're { something going on } special"
      let fields = Map [ "ho", box "hola"; "something going on", box "very" ]
      let tokens = parseTemplateTokens template
      Expect.equal tokens.Length 5 "Extracts 4 texts parts and 1 property part"
      Expect.equal tokens
                   [ TextToken ("Hi ")
                     PropToken ("ho", "##.###")
                     TextToken (", we're ") // <- a quirk of the parser
                     TextToken ("{ something going on }")
                     TextToken (" special") ]
                   "Should extract correct name and format parts"

    testCase "handles evil property strings correctly" <| fun _ ->
      let template = "{a}{b}{  c}{d  } {e} fghij {k}} {l} m {{}}"
      // validity:     +  +  --    --   +         +    +     --
      let expectedPropertyNames = ["a"; "b"; "e"; "k"; "l"]
      let subject = parseTemplateTokens template
      Expect.equal (subject |> List.choose (function PropToken (n,f) -> Some n | _ -> None))
                   expectedPropertyNames
                   "Should extract relevant names"

    testCase "treats `{{` and `}}` as escaped braces, resulting in a single brace in the output" <| fun _ ->
      let template = "hello {{@nonPropWithFormat:##}} {{you}} {are} a {{NonPropNoFormat}}"
      let tokens = parseTemplateTokens template
      Expect.equal tokens
                    [ TextToken("hello {@nonPropWithFormat:##} {you} ")
                      PropToken("are", null)
                      TextToken(" a {NonPropNoFormat}") ]
                   "double open or close braces are escaped"

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

    testCase "literate default tokeniser uses the options `getLogLevelText()` correctly" <| fun _ ->
      let customGetLogLevelText = function Verbose->"A"| Debug->"B"| Info->"C"| Warn->"D"| Error->"E"| Fatal->"F"
      let options = { LiterateOptions.createInvariant() with
                        getLogLevelText = customGetLogLevelText }
      let now = Global.timestamp ()
      let nowDto = DateTimeOffset(DateTimeOffset.ticksUTC now, TimeSpan.Zero)
      let msg level = Message.event level "" |> fun m -> { m with timestamp = now; name = [|"A";"B"|] }
      let nowTimeString = nowDto.LocalDateTime.ToString("HH:mm:ss")
      [ Verbose,  LevelVerbose,   "A"
        Debug,    LevelDebug,     "B"
        Info,     LevelInfo,      "C"
        Warn,     LevelWarning,   "D"
        Error,    LevelError,     "E"
        Fatal,    LevelFatal,     "F" ]
      |> List.iter (fun (logLevel, expectedLevelToken, expectedText) ->
        let tokens = Formatting.literateDefaultTokeniser options (msg logLevel)
        Expect.equal tokens [ "[",            Punctuation
                              nowTimeString,  Subtext
                              " ",            Subtext
                              expectedText,   expectedLevelToken
                              "] ",           Punctuation
                              " ",            Subtext
                              "<",            Punctuation
                              "A.B",          Subtext
                              ">",            Punctuation ]
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

    testCase "when the message has no name (source), the default rendering will not output the name parts" <| fun _ ->
      let message = Message.event Debug "Hello from {where}"
                    |> Message.setSingleName ""
                    |> Message.setField "where" "The Other Side"
      let expectedTimestamp = message.timestamp.ToLiterateTimeString (LiterateOptions.create())
      let expectedTokens =
        [ "[",                  Punctuation
          expectedTimestamp,    Subtext
          " ",                  Subtext
          "DBG",                LevelDebug
          "] ",                 Punctuation
          "Hello from ",        Text
          "The Other Side",     StringSymbol
          Environment.NewLine,  Text ]

      Expect.literateCustomTokenisedPartsEqual (message, None, expectedTokens)

    testCase "replacing the default tokeniser is possible" <| fun _ ->
      let customTokeniser = tokeniserForOutputTemplate "[{timestamp:HH:mm:ss} {level}] {message} [{source}]{exceptions}"
      let message = Message.event Debug "Hello from {where}"
                    |> Message.setSingleName "World.UK.Adele"
                    |> Message.setField "where" "The Other Side"
      let expectedTimestamp = message.timestamp.ToLiterateTimeString (LiterateOptions.create())
      let expectedTokens =
        [ "[",                  Punctuation
          expectedTimestamp,    Subtext
          " ",                  Punctuation
          "DBG",                LevelDebug
          "] ",                 Punctuation
          "Hello from ",        Text
          "The Other Side",     StringSymbol
          " [",                 Punctuation
          "World.UK.Adele",     Subtext
          "]",                  Punctuation
          Environment.NewLine,  Text ]

      Expect.literateCustomTokenisedPartsEqual (message, Some customTokeniser, expectedTokens)

    testList "literate custom output template fields render correctly" [
      let nl = Environment.NewLine
      let level = Info
      let source = "Abc.Def.Ghi"
      let options = LiterateOptions.create()
      let msgTemplate = "Hello {who}"
      let whoValue = "world"
      let msg = Message.event level msgTemplate
                |> Message.setField "who" whoValue
                |> Message.setSingleName source
                |> Message.addExn (exn "ex1")
                |> Message.addExn (exn "ex2")
      let msgDto = msg.timestamp.ToDateTimeOffsetUtc()
      let outputTemplateAndExpected = 
        [ "{timestamp:u}",                    msgDto.ToLocalTime().ToString("u")
          "{timestampUtc:u}",                 msgDto.ToString("u")
          "{level}",                          (options.getLogLevelText msg.level)
          "{source}",                         source
          "{newline}",                        nl
          "{tab}",                            "\t"
          "{message}",                        "Hello world"
          "{exceptions}",                     nl + "System.Exception: ex2" + nl + "System.Exception: ex1"
          "",                                 "" ]

      for (outputTemplate, expected) in outputTemplateAndExpected do
        yield testCase outputTemplate <| fun _ ->
          let output = ResizeArray<ColouredText>()
          let outputWriter (_:obj) (bits:ColouredText list) = output.AddRange bits
          let target = LiterateConsoleTarget([|"Root"|], Verbose, outputTemplate, options, outputWriter) :> Logger
          target.logWithAck level (fun _ -> msg) |> Async.RunSynchronously
          let actualText = String.Join("", output |> Seq.map fst)
          Expect.equal actualText (expected + nl) "output must be correct"
    ]

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
