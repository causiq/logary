module Logary.Tests.Rule

open Fuchu
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
      let found = rules |> Rule.matching "a.b.c"
      Assert.Equal("empty", found, [])

    yield testCase "retrieving existing rule for name" <| fun _ ->
      let found = [ Fac.emptyRule ] |> Rule.matching "a"
      theSubject found.Head.target
      |> should equal "empty target"
      |> thatsIt

    yield testCase "retrieving rule that doesn't match fails" <| fun _ ->
      let funnyRules = [ { Fac.emptyRule with hiera = Regex("^$") } ]
      let found = funnyRules |> Rule.matching "a"
      Assert.Equal("empty", found, [])

    yield testCase "retrieving two matching rules" <| fun _ ->
      let rules = [ Fac.emptyRule ; { Fac.emptyRule with hiera = Regex(@"^a\.b") }]
      let found = rules |> Rule.matching "a.b.c"
      Assert.Equal("eq rules", found, rules)

    yield testCase "retrieving two matching rules one non-matching" <| fun _ ->
      let rules =
        [ Fac.emptyRule
          { Fac.emptyRule with hiera = Regex(@"^a\.b") }
          { Fac.emptyRule with hiera = Regex(@"^a\.b\.d") } ]
      rules
      |> Rule.matching "a.b.c"
      |> List.zip [ Fac.emptyRule; { Fac.emptyRule with hiera =  Regex(@"^a\.b") } ]
      |> List.iter (fun (found, expected) -> Assert.Equal("found eq expected", found, expected))

    yield testCase "misconfiguring logary rule/target throws" <| fun _ ->
      let out = Fac.textWriter ()
      (executing "validateLogary with a rule that doesn't have a matching target" <| fun () ->
        confLogary "tests"
        |> withRules
          [ Rule.createForTarget "not-correct-target" ]
        |> withTargets
          [ Target.confTarget "another-target-name" (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]
        |> validate |> ignore)
      |> should' raiseExn<Configuration.ValidationException>
      |> thatsIt

    yield testCase "when getting loggers with rules with different levels" <| fun _ ->
      let out = Fac.textWriter ()
      let rules = [
        // "path.1" will be match by two rules
        // "path.1.extra" will be match by one rule
        Rule.create (Regex("path\.1"))        "t1" (fun _ -> false) (fun _ -> false) Info
        Rule.create (Regex("path\.1\.extra")) "t1" (fun _ -> false) (fun _ -> false) Verbose
        Rule.create (Regex("path\.2"))        "t2" (fun _ -> false) (fun _ -> false) Warn
        Rule.create (Regex("path\.2\.extra")) "t2" (fun _ -> false) (fun _ -> false) Error
      ]
      let targets = [
        Target.confTarget "t1" (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out))
        Target.confTarget "t2" (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out))
      ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> Async.RunSynchronously
      // string = logger name = path
      let get = Registry.getLogger logary.registry

      // when getting targets

      let no1 = "path.1" |> get |> Async.RunSynchronously
      Assert.Equal("path1 should be Info", Info, no1.Level)

      let no1 = "path.1.extra" |> get |> Async.RunSynchronously
      Assert.Equal("path.1.extra should be Verbose", Verbose, no1.Level) // this one goes downwards

      let no2 = "path.2" |> get |> Async.RunSynchronously
      Assert.Equal("path.2 should be Warn", Warn, no2.Level)

      let no2 = "path.2.extra" |> get |> Async.RunSynchronously
      Assert.Equal("path.2.extra should be warn", Warn, no2.Level) // this one goes upwards

    yield testCase "multiplexing accept filters from given rules" <| fun _ ->
      // given
      let out = Fac.textWriter ()

      let rules =
        [ { Fac.emptyRule with lineFilter = (fun l -> l.path = "1") ; target = "tw" }
          { Fac.emptyRule with lineFilter = (fun l -> l.path = "2") ; target = "tw" } ]

      let targets =
        [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]

      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> Async.RunSynchronously
      try
        async {
          // when
          let get = Registry.getLogger logary.registry
          let! no1 = "1" |> get
          let! no2 = "2" |> get
          let! no3 = "3" |> get

          // 1 and 2 should go through, not 3
          "first"  |> Logger.debug no1
          "second" |> Logger.debug no2
          "third"  |> Logger.debug no3

          // wait for logging to complete; then
          let! _ = Registry.Advanced.flushPending (Duration.FromSeconds(20L)) logary.registry

          (because "it was logged to all three, but rule should stop third" <| fun () ->
            out.ToString())
          |> should contain "first"
          |> should contain "second"
          |> should_not contain "third"
          |> should' (fulfil <| fun str -> "only single line 'first'", Regex.Matches(str, "first").Count = 1)
          |> should' (fulfil <| fun str -> "only single line 'second'", Regex.Matches(str, "second").Count = 1)
          |> should' (fulfil <| fun str -> "zero matches for 'third'", Regex.Matches(str, "third").Count = 0)
          |> thatsIt
        } |> Async.RunSynchronously
      finally
        finaliseLogary logary

    yield testCase "multiplexing accept filters from given rules (levels)" <| fun _ ->
      // given
      let out = Fac.textWriter ()

      let rules =
        [ Rule.createForTarget "tw"
            |> Rule.setLevel Info
            |> Rule.setHiera (Regex("a.*"))

          Rule.createForTarget "tw"
            |> Rule.setLevel Info

          Rule.createForTarget "tw"
            |> Rule.setHiera (Regex("b"))
            |> Rule.setLevel Debug ]

      let targets =
        [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]

      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> Async.RunSynchronously
      try
        async {
          // when
          let get = Registry.getLogger logary.registry
          let! lgrA = "a" |> get
          let! lgrB = "b" |> get

          "first"  |> Logger.debug lgrA
          "second"  |> Logger.info lgrA
          "third"  |> Logger.debug lgrB
          "fourth"  |> Logger.verbose lgrB
          let! _ = Registry.Advanced.flushPending (Duration.FromSeconds(20L)) logary.registry

          because "lgrA matches two rules, lgrB matches only one" (fun _ -> out.ToString())
          |> should_not contain "first"
          |> should contain "second"
          |> should contain "third"
          |> should_not contain "fourth"
          |> should' (fulfil <| fun str -> "only single line 'first'", Regex.Matches(str, "first").Count = 0)
          |> should' (fulfil <| fun str -> "only single line 'second'", Regex.Matches(str, "second").Count = 1)
          |> should' (fulfil <| fun str -> "only single line 'third'", Regex.Matches(str, "third").Count = 1)
          |> should' (fulfil <| fun str -> "only single line 'fourth'", Regex.Matches(str, "fourth").Count = 0)
          |> thatsIt
        } |> Async.RunSynchronously
      finally
        finaliseLogary logary

    yield testCase "filter should never pass anything through" <| fun _ ->
      let out = Fac.textWriter ()
      let rules   = [ { hiera  = Regex(".*"); target = "tw"; lineFilter = (fun line -> false); measureFilter = Rule.allowFilter; level = Debug } ]
      let targets = [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> Async.RunSynchronously
      try
        async {
          let! logr = Registry.getLogger logary.registry "my.path.here"
          (because "configured rule 'tw' with level=Debug, should have that level on logger" <| fun _ ->
            logr.Level)
          |> should equal LogLevel.Debug
          |> thatsIt

          "my message comes here" |> Logger.debug logr
          let! _ = Registry.Advanced.flushPending (Duration.FromSeconds(20L)) logary.registry
          (because "it was logged but accept is always returning false" <| fun () ->
            out.ToString())
          |> should equal ""
          |> thatsIt }
        |> Async.RunSynchronously
      finally
        finaliseLogary logary

    yield testCase "filter should only pass through one path" <| fun _ ->
      let out = Fac.textWriter ()
      let rules   = [
        { hiera  = Regex(".*"); target = "tw"; lineFilter = (fun line -> false); measureFilter = Rule.allowFilter; level  = Verbose }
        { hiera  = Regex(".*"); target = "tw"; lineFilter = (fun line -> line.path = "a.b.c"); measureFilter = Rule.allowFilter; level  = Verbose }
        ]
      let targets = [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.Create(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> Async.RunSynchronously
      try
        async {
          let! shouldLog = Registry.getLogger logary.registry "a.b.c"
          "this message should go through" |> Logger.debug shouldLog
          let! shouldDrop = Registry.getLogger logary.registry "a.x.y"
          "this message should be dropped" |> Logger.debug shouldDrop
          let! _ = Registry.Advanced.flushPending (Duration.FromSeconds(20L)) logary.registry
          (because "we only accept path a.b.c, other never" <| fun () ->
            out.ToString())
          |> should contain "this message should go through"
          |> should_not contain "this message should be dropped"
          |> thatsIt }
        |> Async.RunSynchronously
      finally
        finaliseLogary logary

    ]