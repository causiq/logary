module Logary.Tests.Rule

open Expecto
open Hopac
open Hopac.Infixes
open TestDSL
open Fac
open System
open System.Text.RegularExpressions
open NodaTime
open Logary
open Logary.Message
open Logary.Configuration
open Logary.Targets


[<Tests>]
let tests =
  testList "Rule" [
    yield testCase "retrieving rule for name" <| fun _ ->
      let rules = [] : Rule list
      let found = rules |> Rule.matching (pnp "a.b.c")
      Expect.equal found [] "Shouldn't find anything."

    yield testCase "retrieving existing rule for name" <| fun _ ->
      let found = [ Fac.emptyRule ] |> Rule.matching (PointName.ofSingle "a")
      theSubject found.Head.target
      |> should equal "empty target"
      |> thatsIt

    yield testCase "retrieving rule that doesn't match fails" <| fun _ ->
      let funnyRules = [ { Fac.emptyRule with hiera = Regex("^$") } ]
      let found = funnyRules |> Rule.matching (PointName.ofSingle "a")
      Expect.equal found [] "Shouldn't match anything"

    yield testCase "retrieving two matching rules" <| fun _ ->
      let expected = [ Fac.emptyRule ; { Fac.emptyRule with hiera = Regex(@"^a\.b") }]
      let found = expected |> Rule.matching (pnp "a.b.c")
      Expect.equal found expected "Should equal rules"

    yield testCase "retrieving two matching rules one non-matching" <| fun _ ->
      let rules =
        [ Fac.emptyRule
          { Fac.emptyRule with hiera = Regex(@"^a\.b") }
          { Fac.emptyRule with hiera = Regex(@"^a\.b\.d") } ]
      rules
      |> Rule.matching (pnp "a.b.c")
      |> List.zip [ Fac.emptyRule; { Fac.emptyRule with hiera =  Regex(@"^a\.b") } ]
      |> List.iter (fun (found, expected) ->
        Expect.equal found expected "found eq expected")

    yield testCase "misconfiguring logary rule/target throws" <| fun _ ->
      let out = Fac.textWriter ()
      (executing "validateLogary with a rule that doesn't have a matching target" <| fun () ->
        confLogary "tests"
        |> withRules
          [ Rule.createForTarget "not-correct-target" ]
        |> withTargets
          [ Target.confTarget "another-target-name" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out)) ]
        |> validate |> ignore)
      |> should' raiseExn<Configuration.ValidationException>
      |> thatsIt

    yield testCase "when getting loggers with rules with different levels" <| fun _ ->
      let out = Fac.textWriter ()
      let rules = [
        // "path.1" will be match by two rules
        // "path.1.extra" will be match by one rule
        Rule.create (Regex("path\.1"))        "t1" (fun _ -> false) Info
        Rule.create (Regex("path\.1\.extra")) "t1" (fun _ -> false) Verbose
        Rule.create (Regex("path\.2"))        "t2" (fun _ -> false) Warn
        Rule.create (Regex("path\.2\.extra")) "t2" (fun _ -> false) LogLevel.Error
      ]
      let targets = [
        Target.confTarget "t1" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out))
        Target.confTarget "t2" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out))
      ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      // string = logger name = path
      let get = Registry.getLogger logary.registry >> run

      // when getting targets

      let no1 = pnp "path.1" |> get
      Expect.equal no1.level Info "path1 should be Info"

      let no1 = pnp "path.1.extra" |> get
      Expect.equal no1.level Verbose "path.1.extra should be Verbose" // this one goes downwards

      let no2 = pnp "path.2" |> get
      Expect.equal no2.level Warn "path.2 should be Warn"

      let no2 = pnp "path.2.extra" |> get
      Expect.equal no2.level Warn "path.2.extra should be warn" // this one goes upwards

    yield testCase "middleware" <| fun _ ->
      Tests.skiptest "TODO"
      let noop = fun logaryState next message -> next message
      let hostname = fun logaryState next message -> next (message |> Message.setContext "hostname" (System.Net.Dns.GetHostName()))

      let mids =
        [ hostname
          fun _ next msg -> next msg ]

      let out = Fac.textWriter ()
      let testTarget = Target.confTarget "t1" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out))

      let logary = confLogary "tests" |> withRule (Rule.createForTarget "t1")  |> withTarget testTarget |> validate |> runLogary |> run

      let logger = Registry.getLogger logary.registry (PointName.ofSingle "hi") |> run
      try
        logger.debug (eventX "there") |> run
      finally
        finaliseLogary logary

      Expect.equal (out.ToString()) "there" "LogResult"
      Expect.isTrue (out.ToString().Contains(System.Net.Dns.GetHostName())) "Should contain hostname"

    yield testCase "multiplexing accept filters from given rules" <| fun _ ->
      // given
      let out = Fac.textWriter ()

      let rules =
        [ { Fac.emptyRule with messageFilter = (fun msg -> msg.name = PointName.ofSingle "1") ; target = "tw" }
          { Fac.emptyRule with messageFilter = (fun msg -> msg.name = PointName.ofSingle "2") ; target = "tw" } ]

      let target =
        Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out))

      let logary = confLogary "tests" |> withRules rules |> withTarget target |> validate |> runLogary |> run
      try
        // when
        let get = PointName.parse >> Registry.getLogger logary.registry
        let no1 = "1" |> get |> run
        let no2 = "2" |> get |> run
        let no3 = "3" |> get |> run

        // 1 and 2 should go through, not 3
        no1.debug (eventX "first") |> run
        no2.debug (eventX "second") |> run
        no3.debug (eventX "third") |> run

        // wait for logging to complete; then
        let _ = (Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000) |> run

        (because "it was logged to all three, but rule should stop third" <| fun () ->
          out.ToString())
        |> should contain "first"
        |> should contain "second"
        |> shouldNot contain "third"
        |> should' (fulfil <| fun str -> "only single line 'first'", Regex.Matches(str, "first").Count = 1)
        |> should' (fulfil <| fun str -> "only single line 'second'", Regex.Matches(str, "second").Count = 1)
        |> should' (fulfil <| fun str -> "zero matches for 'third'", Regex.Matches(str, "third").Count = 0)
        |> thatsIt
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
        [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out)) ]

      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          // when
          let get = Registry.getLogger logary.registry
          let! lgrA = get (PointName.ofSingle "a")
          let! lgrB = get (PointName.ofSingle "b")

          do! lgrA.debug (eventX "first")
          do! lgrA.info (eventX "second")
          do! lgrB.debug (eventX "third")
          do! lgrB.verbose (eventX "fourth")
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
        } |> run
      finally
        finaliseLogary logary

    yield testCase "calls message filter" <| fun _ ->
      let out = Fac.textWriter ()
      let catcher = ref None
      let rules =
        [ Rule.createForTarget "tw"
            |> Rule.setMessageFilter (fun m -> catcher := Some m; false)
        ]
      let targets =
        [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out)) ]

      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          // when
          let get = Registry.getLogger logary.registry
          let! lgrA = get (PointName.ofSingle "a")

          do! lgrA.debug (eventX "first")
          let! _ = Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000

          (because "it was logged but filter is always returning false" <| fun () ->
            out.ToString(), !catcher |> Option.isSome)
          |> should equal ("", true)
          |> thatsIt
        } |> run
      finally
        finaliseLogary logary

    yield testCase "filter should never pass anything through" <| fun _ ->
      let out = Fac.textWriter ()

      let filter msg =
        match msg.value with
        | Event _ -> false
        | _ -> true

      let rules   = [ { hiera  = Regex(".*"); target = "tw"; messageFilter = filter; level = Debug } ]
      let targets = [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          let! logr = Registry.getLogger logary.registry (pnp "my.path.here")
          (because "configured rule 'tw' with level=Debug, should have that level on logger" <| fun _ ->
            logr.level)
          |> should equal LogLevel.Debug
          |> thatsIt

          do! logr.debug (eventX "my message comes here")
          let! _ = Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000
          (because "it was logged but accept is always returning false" <| fun () ->
            out.ToString())
          |> should equal ""
          |> thatsIt }
        |> run
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
        { hiera  = Regex(".*"); target = "tw"; messageFilter = filter1; level  = Verbose }
        { hiera  = Regex(".*"); target = "tw"; messageFilter = filter2; level  = Verbose }
        ]
      let targets = [ Target.confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.create(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validate |> runLogary |> run
      try
        job {
          let! shouldLog = Registry.getLogger logary.registry (pnp "a.b.c")
          do! shouldLog.debug (eventX "this message should go through")
          let! shouldDrop = Registry.getLogger logary.registry (pnp "a.x.y")
          do! shouldDrop.debug (eventX "this message should be dropped")
          let! _ = Registry.Advanced.flushPending logary.registry <|> timeOutMillis 20000

          (because "we only accept path a.b.c, other never" <| fun () ->
            out.ToString())
          |> should contain "this message should go through"
          |> shouldNot contain "this message should be dropped"
          |> thatsIt }

        |> run
      finally
        finaliseLogary logary

    ]
