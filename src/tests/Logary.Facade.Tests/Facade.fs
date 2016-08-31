module Logary.Facade.Tests

open System
open Fuchu
open Logary.Facade
open ExpectoPatronum


type internal TemplateToken =
    | TextToken of string
    | PropToken of name:string * format:string
    with override x.ToString() =
                    match x with
                    | TextToken s -> "TEXT("+s+")"
                    | PropToken (n,f) -> "PROP("+n+":"+f+")"

let internal parseTemplateTokens template =
    let tokens = ResizeArray<TemplateToken>()
    let foundText t = tokens.Add (TextToken(t))
    let foundProp (p: FsMtParser.Property) =
        tokens.Add (PropToken(p.Name, p.Format))
    FsMtParser.parseParts (System.Text.StringBuilder()) template foundText foundProp
    tokens |> List.ofSeq

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

    testCase "ConsoleTarget logSimple" <| fun _ ->
      let logger = ConsoleWindowTarget(Warn) :> Logger
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
                   [
                    TextToken ("Hi ")
                    PropToken ("ho", "##.###")
                    TextToken (", we're ") // <- a quirk of the parser
                    TextToken ("{ something going on }")
                    TextToken (" special")
                   ]
                   "Should extract correct name and format parts"

    testCase "handles evil property strings correctly" <| fun _ ->
      let template = "{a}{b}{  c}{d  } {e} fghij {k}} {l} m {{}}"
      // validity:     +  +  --    --   +         +    +     --
      let expectedPropertyNames = ["a"; "b"; "e"; "k"; "l"]
      let strEvilRes = "abcd e fghij k} l m {{}}"
      let subject = parseTemplateTokens template
      Expect.equal (subject |> List.choose (function PropToken (n,f) -> Some n | _ -> None))
                   expectedPropertyNames
                   "Should extract relevant names"

    testCase "literate tokenizes with field names correctly" <| fun _ ->
      let str = "Added {item} to cart {cartId} for {loginUserId} who now has total ${cartTotal}"
      let itemName, cartId, loginUserId, cartTotal = "TicTacs", Guid.NewGuid(), "AdamC", 123.45M
      let fields = Map [ "item", box itemName
                         "cartId", box cartId
                         "loginUserId", box loginUserId
                         "cartTotal", box cartTotal ]
      let now = Global.timestamp ()
      let nowDto = DateTimeOffset(DateTimeOffset.ticksUTC now, TimeSpan.Zero)
      let msg = Message.event Info str |> fun m -> { m with fields = fields; timestamp = now }
      let nowTimeString = nowDto.LocalDateTime.ToString("HH:mm:ss")
      let context = { LiterateContext.Create() with PrintTemplateFieldNames = true }
      let colourParts = Formatting.literateTokenizer context msg
      Expect.equal colourParts
                    [ "[",                    Punctuation
                      nowTimeString,          Subtext
                      " ",                    Subtext
                      "INF",                  LevelInfo
                      "] ",                   Punctuation
                      "Added ",               Text
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
                    "literate tokenized parts must be correct"

    testCase "literate tokenizes missing field with a scary-looking theme (LevelFatal)" <| fun _ ->
      let str = "Added {item} to cart {cartId:X} for {loginUserId} who now has total ${cartTotal}"
      let itemName, cartId, loginUserId, cartTotal = "TicTacs", Guid.NewGuid(), "AdamC", 123.45M
      let fields = Map [ "item", box itemName
                         // "cartId", box cartId
                         // "loginUserId", box loginUserId
                         "cartTotal", box cartTotal ]
      let now = Global.timestamp ()
      let nowDto = DateTimeOffset(DateTimeOffset.ticksUTC now, TimeSpan.Zero)
      let msg = Message.event Info str |> fun m -> { m with fields = fields; timestamp = now }
      let nowTimeString = nowDto.LocalDateTime.ToString("HH:mm:ss")
      let context = { LiterateContext.Create() with PrintTemplateFieldNames = false }
      let colourParts = Formatting.literateTokenizer context msg
      Expect.equal colourParts
                    [ "[",                    Punctuation
                      nowTimeString,          Subtext
                      " ",                    Subtext
                      "INF",                  LevelInfo
                      "] ",                   Punctuation
                      "Added ",               Text
                      "TicTacs",              StringSymbol
                      " to cart ",            Text
                      "{cartId:X}",           LevelFatal
                      " for ",                Text
                      "{loginUserId}",        LevelFatal
                      " who now has total $", Text
                      cartTotal.ToString(),   NumericSymbol ]
                    "literate tokenized parts must be correct"

    testCase "literate tokenizes without field names correctly" <| fun _ ->
      let str = "Added {item} to cart {cartId} for {loginUserId} who now has total ${cartTotal}"
      let itemName, cartId, loginUserId, cartTotal = "TicTacs", Guid.NewGuid(), "AdamC", 123.45M
      let fields = Map [ "item", box itemName
                         "cartId", box cartId
                         "loginUserId", box loginUserId
                         "cartTotal", box cartTotal ]
      let now = Global.timestamp ()
      let nowDto = DateTimeOffset(DateTimeOffset.ticksUTC now, TimeSpan.Zero)
      let msg = Message.event Info str |> fun m -> { m with fields = fields; timestamp = now }
      let nowTimeString = nowDto.LocalDateTime.ToString("HH:mm:ss")
      let context = { LiterateContext.Create() with PrintTemplateFieldNames = false }
      let colourParts = Formatting.literateTokenizer context msg
      Expect.equal colourParts
                    [ "[",                    Punctuation
                      nowTimeString,          Subtext
                      " ",                    Subtext
                      "INF",                  LevelInfo
                      "] ",                   Punctuation
                      "Added ",               Text
                      "TicTacs",              StringSymbol
                      " to cart ",            Text
                      cartId.ToString(),      OtherSymbol
                      " for ",                Text
                      loginUserId,            StringSymbol
                      " who now has total $", Text
                      cartTotal.ToString(),   NumericSymbol ]
                    "literate tokenized parts must be correct"


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
