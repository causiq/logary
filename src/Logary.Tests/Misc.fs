module Logary.Tests.Misc

open Swensen.Unquote

open Fuchu

open NodaTime

open System
open System.IO
open System.Text.RegularExpressions

open Logary
open Logary.Target
open Logary.Metric
open Logary.Formatting
open Logary.Logging

open Logary.Targets
open Logary.Configuration.Config
open System.Text.RegularExpressions
open Internals.Tcp

open TestDSL
open Fac

// framework API tests

[<Tests>]
let tests =
  testList "Misc" [

    yield testCase "can compare LogLevels" <| fun _ ->
      Info <? Error
      Error >? Info

    for i in 1..6 do
      let (l1, l2) = i, i
      let l1, l2 = LogLevel.FromInt l1, LogLevel.FromInt l2
      yield testCase "can equate LogLevel structural" <| fun _ ->
        l1 =? l2
      yield testCase "can equate LogLevel IComparable" <| fun _ ->
        (l1 :> IComparable).CompareTo(l2) =? 0
      yield testCase "can equate LogLevel IComparable<LogLevel>" <| fun _ ->
        (l1 :> IComparable<LogLevel>).CompareTo(l2) =? 0

    for i in 1..5 do
      let (l1, l2) = i, (i + 1)
      let l1, l2 = LogLevel.FromInt l1, LogLevel.FromInt l2
      yield testCase "can compare LogLevel less structural" <| fun _ ->
        l1 <? l2
      yield testCase "can compare LogLevel less IComparable" <| fun _ ->
        (l1 :> IComparable).CompareTo(l2) =? -1
      yield testCase "can compare LogLevel less IComparable<LogLevel>" <| fun _ ->
        (l1 :> IComparable<LogLevel>).CompareTo(l2) =? -1

    yield testCase "retrieving rule for name" <| fun _ ->
      let rules = [] : Rule list
      let found = rules |> Rules.matching "a.b.c"
      found =? []

    yield testCase "retrieving existing rule for name" <| fun _ ->
      let found = [ Fac.emptyRule ] |> Rules.matching "a"
      theSubject found.Head.target
      |> should equal "empty target"
      |> thatsIt

    yield testCase "retrieving rule that doesn't match fails" <| fun _ ->
      let funnyRules = [ { Fac.emptyRule with hiera = Regex("^$") } ]
      let found = funnyRules |> Rules.matching "a"
      found =? []

    yield testCase "retrieving two matching rules" <| fun _ ->
      let rules = [ Fac.emptyRule ; { Fac.emptyRule with hiera = Regex(@"^a\.b") }]
      let found = rules |> Rules.matching "a.b.c"
      found =? rules

    yield testCase "retrieving two matching rules one non-matching" <| fun _ ->
      let rules =
        [ Fac.emptyRule
        ; { Fac.emptyRule with hiera = Regex(@"^a\.b") }
        ; { Fac.emptyRule with hiera = Regex(@"^a\.b\.d") } ]
      rules
      |> Rules.matching "a.b.c"
      |> List.zip [ Fac.emptyRule; { Fac.emptyRule with hiera =  Regex(@"^a\.b") } ]
      |> List.iter (fun (found, expected) -> found =? expected)

    yield testCase "misconfiguring logary rule/target throws" <| fun _ ->
      let out = Fac.textWriter ()
      (executing "validateLogary with a rule that doesn't have a matching target" <| fun () ->
        confLogary "tests"
        |> withRules
          [ Rule.Create(Regex(@".*"), "not-correct-target", (fun _ -> true), LogLevel.Verbose) ]
        |> withTargets
          [ confTarget "another-target-name" (TextWriter.create <| TextWriter.TextWriterConf.Default(out, out)) ]
        |> validateLogary |> ignore)
      |> should' raiseExn<Configuration.ValidationException>
      |> thatsIt

    yield testCase "when getting loggers with rules with different levels" <| fun _ ->
      let out = Fac.textWriter ()
      let rules = [
        // "path.1" will be match by two rules
        // "path.1.extra" will be match by one rule
        Rule.Create(Regex("path\.1"), "t1", (fun line -> false), Info)
        Rule.Create(Regex("path\.1\.extra"), "t1", (fun line -> false), Verbose)
        Rule.Create(Regex("path\.2"), "t2", (fun line -> false), Warn)
        Rule.Create(Regex("path\.2\.extra"), "t2", (fun line -> false), Error)
      ]
      let targets = [
        confTarget "t1" (TextWriter.create <| TextWriter.TextWriterConf.Default(out, out))
        confTarget "t2" (TextWriter.create <| TextWriter.TextWriterConf.Default(out, out))
      ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validateLogary |> runLogary
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
        [ { Fac.emptyRule with accept = (fun l -> l.path = "1") ; target = "tw" }
        ; { Fac.emptyRule with accept = (fun l -> l.path = "2") ; target = "tw" } ]

      let targets =
        [ confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.Default(out, out)) ]

      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validateLogary |> runLogary
      try
        async {
          // when
          let get = Registry.getLogger logary.registry
          let! no1 = "1" |> get
          let! no2 = "2" |> get
          let! no3 = "3" |> get

          // 1 and 2 should go through, not 3
          "first"  |> Log.debug no1
          "second" |> Log.debug no2
          "third"  |> Log.debug no3

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

    yield testCase "filter should never pass anything through" <| fun _ ->
      let out = Fac.textWriter ()
      let rules   = [ { hiera  = Regex(".*"); target = "tw"; accept = (fun line -> false); level  = Debug } ]
      let targets = [ confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.Default(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validateLogary |> runLogary
      try
        async {
          let! logr = Registry.getLogger logary.registry "my.path.here"
          (because "configured rule 'tw' with level=Debug, should have that level on logger" <| fun _ ->
            logr.Level)
          |> should equal LogLevel.Debug
          |> thatsIt

          "my message comes here" |> Log.debug logr
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
        { hiera  = Regex(".*"); target = "tw"; accept = (fun line -> false); level  = Verbose }
        { hiera  = Regex(".*"); target = "tw"; accept = (fun line -> line.path = "a.b.c"); level  = Verbose }
        ]
      let targets = [ confTarget "tw" (TextWriter.create <| TextWriter.TextWriterConf.Default(out, out)) ]
      let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validateLogary |> runLogary
      try
        async {
          let! shouldLog = Registry.getLogger logary.registry "a.b.c"
          "this message should go through" |> Log.debug shouldLog
          let! shouldDrop = Registry.getLogger logary.registry "a.x.y"
          "this message should be dropped" |> Log.debug shouldDrop
          let! _ = Registry.Advanced.flushPending (Duration.FromSeconds(20L)) logary.registry
          (because "we only accept path a.b.c, other never" <| fun () ->
            out.ToString())
          |> should contain "this message should go through"
          |> should_not contain "this message should be dropped"
          |> thatsIt }
        |> Async.RunSynchronously
      finally
        finaliseLogary logary

    yield testCase "retrieving logger for name" <| fun _ ->
      Fac.withLogary <| fun logary out err ->
        let logger = "a.b.c.d" |> Registry.getLogger logary.registry |> Async.RunSynchronously
        let logger' = "a.b.c.d" |> (logary |> asLogManager).GetLogger
        (because "logging normally" <| fun () ->
          "Hello world" |> Log.info logger
          "Goodbye cruel world" |> Log.fatal logger'
          logary |> finaliseLogary
          out.ToString(), err.ToString())
        |> theTuple
          (fun first second ->
            first |> should contain "Hello world" |> thatsIt
            second |> should contain "Goodbye cruel world" |> thatsIt)
        |> thatsIt

    yield testCase "after shutting down no logging happens" <| fun _ ->
      Fac.withLogary <| fun logary out err ->
        let logger = "a.b.c.d" |> Registry.getLogger logary.registry |> Async.RunSynchronously
        (because "logging something, then shutting down" <| fun () ->
          "hi there" |> Log.info logger
          logary |> shutdownLogary |> Async.RunSynchronously |> ignore
          "after shutdown" |> Log.info logger
          out.ToString())
        |> should contain "hi there"
        |> should_not contain "after shutdown"
        |> thatsIt

    yield testCase "timing f-n call" <| fun _ ->
      Fac.withLogary <| fun logary stdout stderr ->
        let logger = "a.b.c.d" |> Registry.getLogger logary.registry |> Async.RunSynchronously
        let f = Log.timelvl logger Info (logger.Name) <| fun () ->
          printfn "doing work ..."
          42
        // TODO: assert on Nimrod output
        f =? 42
    ]