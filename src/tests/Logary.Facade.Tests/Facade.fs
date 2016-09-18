module Logary.Facade.Tests

open System
open Fuchu
open Logary.Facade
open Logary.Facade.Literals
open Logary.Facade.Literate
open ExpectoPatronum

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

type Assert with
  static member literateMessagePartsEqual (template, fields, expectedMessageParts, ?options, ?logLevel, ?tokeniser) =
    let options = defaultArg options (LiterateOptions.create())
    let tokeniser = defaultArg tokeniser Formatting.literateDefaultTokeniser
    let logLevel = defaultArg logLevel Info
    let now = Global.timestamp ()
    let nowDto = DateTimeOffset(DateTimeOffset.ticksUTC now, TimeSpan.Zero)
    let msg = Message.event logLevel template |> fun m -> { m with fields = fields; timestamp = now }
    let nowTimeString = nowDto.LocalDateTime.ToString("HH:mm:ss", options.formatProvider)
    let actualTokens = tokeniser options msg
    let expectedTokens = [  "[",                              Punctuation
                            nowTimeString,                    Subtext
                            " ",                              Subtext
                            options.getLogLevelText logLevel, LevelInfo
                            "] ",                             Punctuation ]
                            @ expectedMessageParts

    Expect.equal actualTokens expectedTokens "literate tokenised parts must be correct"

[<Tests>]
let tests =
  let msg = Message.event Info "hi {name}"
  // don't print to console
  Global.initialise { Global.DefaultConfig with getLogger = fun _ -> Targets.create Fatal }

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
      let logger = LiterateConsoleTarget(Warn) :> Logger
      Message.event Info "Aliens descended" |> logger.logSimple

    testProperty "Loggers.create" <| fun level ->
      Targets.create level |> ignore

    testProperty "Targets.create and logSimple" <| fun level ->
      Tests.skiptest "verbose test"
      let logger = Targets.create level
      Message.event Verbose "Hi {name}" |> logger.logSimple

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
      Assert.literateMessagePartsEqual (template, fields, expectedMessageParts, options)

    testCase "literate can tokenise an empty message template" <| fun _ ->
      let emptyFields = Map.empty<string,obj>
      Assert.literateMessagePartsEqual ("", emptyFields, [])

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
      Assert.literateMessagePartsEqual (template, fields, expectedMessageParts)

    testCase "literate default tokeniser uses the options `getLogLevelText()` correctly" <| fun _ ->
      let customGetLogLevelText = function Verbose->"A"|Debug->"B"|Info->"C"|Warn->"D"|Error->"E"|Fatal->"F"
      let options = { LiterateOptions.createInvariant() with
                        getLogLevelText = customGetLogLevelText }
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
        let tokens = Formatting.literateDefaultTokeniser options (msg logLevel)
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

        Assert.literateMessagePartsEqual (template, fields, expectedMessageParts, options)
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
          nl, Text
          "System.Exception: errors field 1", Text
          nl, Text
          nl, Text
          "System.Exception: errors field 2", Text
          nl, Text
          nl, Text
          nl, Text //<-- Extra newline at the end
        ]
      Assert.literateMessagePartsEqual (template, fields, expectedMessageParts)

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
      Assert.literateMessagePartsEqual (template, fields, expectedMessageParts, options)

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
