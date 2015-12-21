module Logary.Tests.Rule

open Fuchu
open Hopac
open Hopac.Infixes
open Swensen.Unquote
open TestDSL
open Fac
open System
open System.Text.RegularExpressions
open NodaTime
open Logary
open Logary.Configuration
open Logary.Targets

[<Tests>]
let tests =
  testList "LogLevel" [

    yield testCase "retrieving rule for name" <| fun _ ->
      let rules = [] : Rule list
      let found = rules |> Rule.matching (pnp "a.b.c")
      found =? []

    yield testCase "retrieving existing rule for name" <| fun _ ->
      let found = [ Fac.emptyRule ] |> Rule.matching (pn "a")
      theSubject found.Head.target
      |> should equal (pn "empty target")
      |> thatsIt

    yield testCase "retrieving rule that doesn't match fails" <| fun _ ->
      let funnyRules = [ { Fac.emptyRule with hiera = Regex("^$") } ]
      let found = funnyRules |> Rule.matching (pn "a")
      found =? []

    yield testCase "retrieving two matching rules" <| fun _ ->
      let rules = [ Fac.emptyRule ; { Fac.emptyRule with hiera = Regex(@"^a\.b") }]
      let found = rules |> Rule.matching (pnp "a.b.c")
      found =? rules

    yield testCase "retrieving two matching rules one non-matching" <| fun _ ->
      let rules =
        [ Fac.emptyRule
          { Fac.emptyRule with hiera = Regex(@"^a\.b") }
          { Fac.emptyRule with hiera = Regex(@"^a\.b\.d") } ]
      rules
      |> Rule.matching (pnp "a.b.c")
      |> List.zip [ Fac.emptyRule; { Fac.emptyRule with hiera =  Regex(@"^a\.b") } ]
      |> List.iter (fun (found, expected) -> found =? expected)

    yield testCase "misconfiguring logary rule/target throws" <| fun _ ->
      let out = Fac.textWriter ()
      (executing "validateLogary with a rule that doesn't have a matching target" <| fun () ->
        confLogary "tests"
        |> withRules
          [ Rule.createForTarget (pn "not-correct-target") ]
        |> withTargets
          [ Target.confTarget (pn "another-target-name") (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]
        |> validate |> ignore)
      |> should' raiseExn<Configuration.ValidationException>
      |> thatsIt

    yield testCase "when getting loggers with rules with different levels" <| fun _ ->
      let out = Fac.textWriter ()
      let rules = [
        // "path.1" will be match by two rules
        // "path.1.extra" will be match by one rule
        Rule.create (Regex("path\.1"))        (pn "t1") (fun _ -> false) Info
        Rule.create (Regex("path\.1\.extra")) (pn "t1") (fun _ -> false) Verbose
        Rule.create (Regex("path\.2"))        (pn "t2") (fun _ -> false) Warn
        Rule.create (Regex("path\.2\.extra")) (pn "t2") (fun _ -> false) LogLevel.Error
      ]
      let targets = [
        Target.confTarget (pn "t1") (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out))
        Target.confTarget (pn "t2") (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out))
      ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      // string = logger name = path
      let get = Registry.getLogger logary.registry >> run

      // when getting targets

      let no1 = pnp "path.1" |> get
      Assert.Equal("path1 should be Info", Info, no1.level)

      let no1 = pnp "path.1.extra" |> get
      Assert.Equal("path.1.extra should be Verbose", Verbose, no1.level) // this one goes downwards

      let no2 = pnp "path.2" |> get
      Assert.Equal("path.2 should be Warn", Warn, no2.level)

      let no2 = pnp "path.2.extra" |> get
      Assert.Equal("path.2.extra should be warn", Warn, no2.level) // this one goes upwards

    yield testCase "multiplexing accept filters from given rules" <| fun _ ->
      // given
      let out = Fac.textWriter ()

      let rules =
        [ { Fac.emptyRule with messageFilter = (fun msg -> msg.name = pn "1") ; target = pn "tw" }
          { Fac.emptyRule with messageFilter = (fun msg -> msg.name = pn "2") ; target = pn "tw" } ]

      let targets =
        [ Target.confTarget (pn "tw") (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]

      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          // when
          let get = Registry.getLogger logary.registry
          let! no1 = pn "1" |> get
          let! no2 = pn "2" |> get
          let! no3 = pn "3" |> get

          // 1 and 2 should go through, not 3
          do! "first"  |> Logger.debug no1
          do! "second" |> Logger.debug no2
          do! "third"  |> Logger.debug no3

          // wait for logging to complete; then
          let! _ = Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000

          (because "it was logged to all three, but rule should stop third" <| fun () ->
            out.ToString())
          |> should contain "first"
          |> should contain "second"
          |> shouldNot contain "third"
          |> should' (fulfil <| fun str -> "only single line 'first'", Regex.Matches(str, "first").Count = 1)
          |> should' (fulfil <| fun str -> "only single line 'second'", Regex.Matches(str, "second").Count = 1)
          |> should' (fulfil <| fun str -> "zero matches for 'third'", Regex.Matches(str, "third").Count = 0)
          |> thatsIt
        } |> run
      finally
        finaliseLogary logary

    yield testCase "multiplexing accept filters from given rules (levels)" <| fun _ ->
      // given
      let out = Fac.textWriter ()

      let rules =
        [ Rule.createForTarget (pn "tw")
            |> Rule.setLevel Info
            |> Rule.setHiera (Regex("a.*"))

          Rule.createForTarget (pn "tw")
            |> Rule.setLevel Info

          Rule.createForTarget (pn "tw")
            |> Rule.setHiera (Regex("b"))
            |> Rule.setLevel Debug ]

      let targets =
        [ Target.confTarget (pn "tw") (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]

      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          // when
          let get = Registry.getLogger logary.registry
          let! lgrA = get (PointName.ofSingle "a")
          let! lgrB = get (pn "b")

          do! "first"   |> Logger.debug lgrA
          do! "second"  |> Logger.info lgrA
          do! "third"   |> Logger.debug lgrB
          do! "fourth"  |> Logger.verbose lgrB
          let! _ = Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000

          because "lgrA matches two rules, lgrB matches only one" (fun _ -> out.ToString())
          |> shouldNot contain "first"
          |> should contain "second"
          |> should contain "third"
          |> shouldNot contain "fourth"
          |> should' (fulfil <| fun str -> "only single line 'first'", Regex.Matches(str, "first").Count = 0)
          |> should' (fulfil <| fun str -> "only single line 'second'", Regex.Matches(str, "second").Count = 1)
          |> should' (fulfil <| fun str -> "only single line 'third'", Regex.Matches(str, "third").Count = 1)
          |> should' (fulfil <| fun str -> "only single line 'fourth'", Regex.Matches(str, "fourth").Count = 0)
          |> thatsIt
        } |> Job.Global.run
      finally
        finaliseLogary logary

    yield testCase "filter should never pass anything through" <| fun _ ->
      let out = Fac.textWriter ()

      let filter msg =
        match msg.value with
        | Event _ -> false
        | _ -> true

      let rules   = [ { hiera  = Regex(".*"); target = pn "tw"; messageFilter = filter; level = Debug } ]
      let targets = [ Target.confTarget (pn "tw") (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          let! logr = Registry.getLogger logary.registry (pnp "my.path.here")
          (because "configured rule 'tw' with level=Debug, should have that level on logger" <| fun _ ->
            logr.level)
          |> should equal LogLevel.Debug
          |> thatsIt

          do! "my message comes here" |> Logger.debug logr
          let! _ = Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000
          (because "it was logged but accept is always returning false" <| fun () ->
            out.ToString())
          |> should equal ""
          |> thatsIt }
        |> Job.Global.run
      finally
        finaliseLogary logary

    yield testCase "filter should only pass through one path" <| fun _ ->
      let out = Fac.textWriter ()

      let filter1 msg =
        match msg.value with
        | Event _ -> false
        | _ -> true

      let filter2 msg =
        match msg.value with
        | Event _ -> msg.name = pnp "a.b.c"
        | _ -> true

      let rules   = [
        { hiera  = Regex(".*"); target = pn "tw"; messageFilter = filter1; level  = Verbose }
        { hiera  = Regex(".*"); target = pn "tw"; messageFilter = filter2; level  = Verbose }
        ]
      let targets = [ Target.confTarget (pn "tw") (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          let! shouldLog = Registry.getLogger logary.registry (pnp "a.b.c")
          do! "this message should go through" |> Logger.debug shouldLog
          let! shouldDrop = Registry.getLogger logary.registry (pnp "a.x.y")
          do! "this message should be dropped" |> Logger.debug shouldDrop
          let! _ = Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000
          (because "we only accept path a.b.c, other never" <| fun () ->
            out.ToString())
          |> should contain "this message should go through"
          |> shouldNot contain "this message should be dropped"
          |> thatsIt }
        |> Job.Global.run
      finally
        finaliseLogary logary

    ]