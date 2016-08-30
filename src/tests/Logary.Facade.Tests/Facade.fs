module Logary.Facade.Tests

open System
open Fuchu
open Logary.Facade
open ExpectoPatronum

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
//
//    testCase "extract matches" <| fun _ ->
//      let str = "Hi {ho}, we're { something going on  } special"
//      let fields = Map [ "ho", box "hola"; "something going on", box "very" ]
//      let subject = Formatting.extractMatches fields str
//      Expect.equal subject.Length 2 "Should match two things"
//      Expect.equal (subject |> List.map (fun (name, _, _) -> name))
//                   ["ho"; "something going on"]
//                   "Should extract correct names"
//      Expect.equal (subject |> List.map (fun (_, _, m) -> m.Value))
//                   ["{ho}"; "{ something going on  }"]
//                   "Should extract correct match values"

//    testCase "extract matches evil" <| fun _ ->
//      let strEvil = "{a}{b}{  c}{d  } {e} fghij {k}} {l} m {{}}"
//      let aThroughL =['a'..'l'] |> List.map string
//      let fields = aThroughL |> List.map (fun s -> s, s) |> Map.ofList
//      let strEvilRes = "abcd e fghij k} l m {{}}"
//      let subject = Formatting.extractMatches fields strEvil
//      Expect.equal subject.Length 7 "Should match 7 things, a-l, not m and not {}"
//      Expect.equal (subject |> List.map (fun (name, value, _) -> name))
//                   ["a"; "b"; "c"; "d"; "e"; "k"; "l"]
//                   "Should extract relevant names"
//      Expect.equal (subject |> List.map (fun (_, _, m) -> m.Index))
//                   [0; 3; 6; 11; 17; 27; 32]
//                   "Should extract correct indicies"

    testCase "colourises using literate theme correctly" <| fun _ ->
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
      let theme = LiterateTheme.Default
      let colourParts = Formatting.literateColorizer theme msg true
      Expect.equal colourParts
                   [
                    "[",                    theme.Punctuation
                    nowTimeString,          theme.Subtext
                    " ",                    theme.Subtext
                    "INF",                  theme.LevelInfo
                    "] ",                   theme.Punctuation
                    "Hi ",                  theme.Text
                    "Added ",               theme.Text
                    "[item] ",              theme.Subtext
                    "TicTacs",              theme.StringSymbol
                    " to cart ",            theme.Text
                    "[cartId] ",            theme.Subtext
                    cartId.ToString(),      theme.OtherSymbol
                    " for ",                theme.Text
                    "[loginUserId] ",       theme.Subtext
                    loginUserId,            theme.StringSymbol
                    " who now has total $", theme.Text
                    "[cartTotal] ",         theme.Subtext
                    cartTotal.ToString(),   theme.NumericSymbol
                   ]
                   "Should format colours correctly"

    testCase "format template properly" <| fun _ ->
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
                   (sprintf "[I] %s: Hi hola, we're very special [Logary.Facade.Tests]" (nowDto.ToString("o")))
                   "Should format correctly"
  ]
