namespace Logary

module Tests =
  open Swensen.Unquote
  open NUnit.Framework

  open System
  open System.IO

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

  // framework API tests

  let isTarget name (t : TargetInstance) = t.name =? name

  [<Test>]
  let ``logary lifecycle`` () =
    confLogary "tests"
    |> validateLogary
    |> runLogary
    |> shutdownLogary
    |> Async.RunSynchronously
    |> ignore

  open TextWriter

  [<Test>]
  let ``target lifecycle`` () =
    confTarget "tw" (create (TextWriterConf.Default(Fac.textWriter(), Fac.textWriter())))
    |> validateTarget
    |> initTarget { serviceName = "tests" }
    |> send (Log.debugStr "Hello")
    |> shutdownTarget
    |> Async.RunSynchronously
    |> ignore

  [<Test>]
  let ``can compare LogLevels`` () =
    Info <? Error
    Error >? Info

  [<TestCase(1, 1)>]
  [<TestCase(2, 2)>]
  [<TestCase(3, 3)>]
  [<TestCase(4, 4)>]
  [<TestCase(5, 5)>]
  [<TestCase(6, 6)>]
  let ``can equate LogLevels`` l1 l2 =
    let l1, l2 = LogLevel.FromInt l1, LogLevel.FromInt l2
    l1 =? l2
    (l1 :> IComparable).CompareTo(l2) =? 0
    (l1 :> IComparable<LogLevel>).CompareTo(l2) =? 0

  [<TestCase(1, 2)>]
  [<TestCase(2, 3)>]
  [<TestCase(3, 4)>]
  [<TestCase(4, 5)>]
  [<TestCase(5, 6)>]
  let ``can compare to less`` l1 l2 =
    let l1, l2 = LogLevel.FromInt l1, LogLevel.FromInt l2
    l1 <? l2

  [<Test>]
  let ``retrieving rule for name`` () =
    let rules = [] : Rule list
    let found = rules |> Rules.matching "a.b.c"
    found =? []

  [<Test>]
  let ``retrieving existing rule for name`` () =
    let found = [ Fac.emptyRule ] |> Rules.matching "a"
    theSubject found.Head.target
    |> should equal "empty target"
    |> thatsIt

  [<Test>]
  let ``retrieving rule that doesn't match fails`` () =
    let funnyRules = [ { Fac.emptyRule with hiera = Regex("^$") } ]
    let found = funnyRules |> Rules.matching "a"
    found =? []

  [<Test>]
  let ``retrieving two matching rules`` () =
    let rules = [ Fac.emptyRule ; { Fac.emptyRule with hiera = Regex(@"^a\.b") }]
    let found = rules |> Rules.matching "a.b.c"
    found =? rules

  [<Test>]
  let ``retrieving two matching rules one non-matching`` () =
    let rules =
      [ Fac.emptyRule
      ; { Fac.emptyRule with hiera = Regex(@"^a\.b") }
      ; { Fac.emptyRule with hiera = Regex(@"^a\.b\.d") } ]
    rules
    |> Rules.matching "a.b.c"
    |> List.zip [ Fac.emptyRule; { Fac.emptyRule with hiera =  Regex(@"^a\.b") } ]
    |> List.iter (fun (found, expected) -> found =? expected)

  [<Test>]
  let ``misconfiguring logary rule/target throws`` () =
    let out = Fac.textWriter ()
    (executing "validateLogary with a rule that doesn't have a matching target" <| fun () ->
      confLogary "tests"
      |> withRules
        [ Rule.Create(Regex(@".*"), "not-correct-target", (fun _ -> true), LogLevel.Verbose) ]
      |> withTargets
        [ confTarget "another-target-name" (create <| TextWriterConf.Default(out, out)) ]
      |> validateLogary |> ignore)
    |> should' raiseExn<Configuration.ValidationException>
    |> thatsIt

  let finaliseLogary = shutdownLogary >> fun a ->
    let acks = Async.RunSynchronously(a, 1000)
    (because "finalise should always work" <| fun () ->
      match acks with
      | Nack desc -> failwithf "finaliseLogary failed %A" desc
      | Ack -> ()) |> thatsIt

  open System.Text.RegularExpressions

  [<Test>]
  let ``when getting loggers with rules with different levels`` () =
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
      confTarget "t1" (create <| TextWriterConf.Default(out, out))
      confTarget "t2" (create <| TextWriterConf.Default(out, out))
    ]
    let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validateLogary |> runLogary
    // string = logger name = path
    let get = Registry.getLogger logary.registry

    // when getting targets

    let no1 = "path.1" |> get |> Async.RunSynchronously
    Assert.AreEqual(Info, no1.Level)

    let no1 = "path.1.extra" |> get |> Async.RunSynchronously
    Assert.AreEqual(Verbose, no1.Level) // this one goes downwards

    let no2 = "path.2" |> get |> Async.RunSynchronously
    Assert.AreEqual(Warn, no2.Level)

    let no2 = "path.2.extra" |> get |> Async.RunSynchronously
    Assert.AreEqual(Warn, no2.Level) // this one goes upwards

  [<Test>]
  let ``multiplexing accept filters from given rules`` () =
    // given
    let out = Fac.textWriter ()

    let rules =
      [ { Fac.emptyRule with accept = (fun l -> l.path = "1") ; target = "tw" }
      ; { Fac.emptyRule with accept = (fun l -> l.path = "2") ; target = "tw" } ]

    let targets =
      [ confTarget "tw" (create <| TextWriterConf.Default(out, out)) ]

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
        let! _ = Registry.flushPending logary.registry

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

  [<Test>]
  let ``filter should never pass anything through``() =
    let out = Fac.textWriter ()
    let rules   = [ { hiera  = Regex(".*"); target = "tw"; accept = (fun line -> false); level  = Debug } ]
    let targets = [ confTarget "tw" (create <| TextWriterConf.Default(out, out)) ]
    let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validateLogary |> runLogary
    try
      async {
        let! logr = Registry.getLogger logary.registry "my.path.here"
        (because "configured rule 'tw' with level=Debug, should have that level on logger" <| fun _ ->
          logr.Level)
        |> should equal LogLevel.Debug
        |> thatsIt

        "my message comes here" |> Log.debug logr
        let! _ = Registry.flushPending logary.registry
        (because "it was logged but accept is always returning false" <| fun () ->
          out.ToString())
        |> should equal ""
        |> thatsIt }
      |> Async.RunSynchronously
    finally
      finaliseLogary logary

  [<Test>]
  let ``filter should only pass through one path`` () =
    let out = Fac.textWriter ()
    let rules   = [
      { hiera  = Regex(".*"); target = "tw"; accept = (fun line -> false); level  = Verbose }
      { hiera  = Regex(".*"); target = "tw"; accept = (fun line -> line.path = "a.b.c"); level  = Verbose }
      ]
    let targets = [ confTarget "tw" (create <| TextWriterConf.Default(out, out)) ]
    let logary = confLogary "tests" |> withRules rules |> withTargets targets |> validateLogary |> runLogary
    try
      async {
        let! shouldLog = Registry.getLogger logary.registry "a.b.c"
        "this message should go through" |> Log.debug shouldLog
        let! shouldDrop = Registry.getLogger logary.registry "a.x.y"
        "this message should be dropped" |> Log.debug shouldDrop
        let! _ = Registry.flushPending logary.registry
        (because "we only accept path a.b.c, other never" <| fun () ->
          out.ToString())
        |> should contain "this message should go through"
        |> should_not contain "this message should be dropped"
        |> thatsIt }
      |> Async.RunSynchronously
    finally
      finaliseLogary logary

  [<Test>]
  let ``retrieving logger for name`` () =
    Fac.withLogary <| fun logary out err ->
      let logger = "a.b.c.d" |> Registry.getLogger logary.registry |> Async.RunSynchronously
      let logger' = "a.b.c.d" |> (logary :> LogManager).GetLogger
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

  [<Test>]
  let ``after shutting down no logging happens`` () =
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

  [<Test>]
  let ``timing f-n call`` () =
    Fac.withLogary <| fun logary stdout stderr ->
      let logger = "a.b.c.d" |> Registry.getLogger logary.registry |> Async.RunSynchronously
      let f = Log.timelvl logger Info (logger.Name) <| fun () ->
        printfn "doing work ..."
        42
      // TODO: assert on Nimrod output
      f =? 42

  [<Test; Ignore>]
  let ``counter inc`` () =
    ()

  // TextWriterTarget

  [<Test>]
  let ``initialising TextWriter target`` () =
    let target = create (TextWriterConf.Default(System.Console.Out, System.Console.Error)) "sample console"
    let instance = target.initer { serviceName = "tests" }
    instance.name =? "sample console"

  let finaliseTarget = shutdownTarget >> fun a ->
    let acks = Async.RunSynchronously(a, 1000)
    match acks with
    | FSharp.Actor.RPC.ExperiencedTimeout actor -> failwith "finalising target timeout"
    | FSharp.Actor.RPC.SuccessWith(acks, actor) ->
      match acks with
      | Nack desc -> failwith "would not shut down: %A"
      | Ack -> ()

  [<Test>]
  let ``writing with Console target directly`` () =
    let stdout = Fac.textWriter ()
    let target = create (TextWriterConf.Default(stdout, stdout)) "writing console target"
    let instance = target |> initTarget { serviceName = "tests" }

    (because "logging with info level and then finalising the target" <| fun () ->
      "Hello World!" |> Log.infoStr |> logTarget instance
      instance |> finaliseTarget
      stdout.ToString())
    |> should contain "Hello World!"
    |> thatsIt

  [<Test>]
  let ``error levels should be to error text writer`` () =

    let out, err = Fac.textWriter (), Fac.textWriter ()
    let target = create (TextWriterConf.Default(out, err)) "error writing"
    let subject = target |> initTarget { serviceName = "tests" }

    (because "logging 'Error line' and 'Fatal line' to the target" <| fun () ->
      Log.errorStr "Error line" |> logTarget subject
      Log.fatalStr "Fatal line" |> logTarget subject
      subject |> finaliseTarget
      err.ToString())
    |> should contain "Error line"
    |> should contain "Fatal line"
    |> thatsIt

  // LogStash Target

  [<Test>]
  let ``when logging into logstash`` () =
    let target = Logstash.create (Logstash.LogstashConf.Create("10.0.0.120", 1936us, StubTcp.StubWriterClient.Create)) "logstash-integration"
    let subject = target |> initTarget { serviceName = "tests" }
    (because "logging warning to logstash" <| fun () ->
      Log.warnStrTag "integration" "integration test" |> logTarget subject
      subject |> finaliseTarget
      ())
    |> thatsIt

  open Logary.Internals.Tcp

  let raised_exn msg =
    let e = ref None : exn option ref
    try raise <| ApplicationException(msg)
    with ex -> e := Some ex
    (!e).Value

  [<Test>]
  let ``when logging into logstash should output version one event`` () =
    let e1 = raised_exn "darn"
    let e2 = raised_exn "actual exn"
    // pipe a to onto f and g
    let (-|>) a (f, g) = f a, g a
    let writer  = new StubTcp.StubWriterClient(true)
    let conf    = Logstash.LogstashConf.Create("127.0.0.1", clientFac = fun _ _ -> writer :> WriteClient)
    let target  = Logstash.create conf "logstash-integration"
    let subject = target |> initTarget { serviceName = "tests" }
    let msg     = "integration test message"
    (because "logging warning to logstash" <| fun () ->
      Log.warnStrTag "integration" msg
      |> Log.setPath "a.b.c"
      |> Log.setData "data-key" "data-value"
      |> Log.setData "e" e1
      |> Log.setExn e2
      |> logTarget subject
      subject |> finaliseTarget
      writer.ReadLines() |> Seq.exactlyOne |> Newtonsoft.Json.Linq.JObject.Parse)
    |> should' (fulfil (fun t -> "timestamp should not be null", not(t.["timestamp"] = null)))
    |> should' (fulfil (fun t -> "@version should not be null", not(t.["@version"] = null)))
    |> should' (fulfil (fun t -> "message should not be null", not(t.["message"] = null)))
    |> should' (fulfil (fun t -> "path should not be null", not(t.["path"] = null)))
    |> should' (fulfil (fun t -> "hostname should not be null", not(t.["hostname"] = null)))
    |> should' (fulfil (fun t -> "service should not be null", not(t.["service"] = null)))
    |> should' (fulfil (fun t -> "level should not be null", not(t.["level"] = null)))
    |> should' (fulfil (fun t -> "data-key should not be null", not(t.["data-key"] = null)))
    |> should' (fulfil (fun t -> "e should not be null", not(t.["e"] = null)))
    |> should' (fulfil (fun t -> "exception should not be null", not(t.["exception"] = null)))

    |> should' (fulfil (fun t ->
        (Net.Dns.GetHostName()) -|> (sprintf "hostname should be %s", (=) (string t.["hostname"]))))
    |> should' (fulfil (fun t -> "should have same message", string t.["message"] = msg))
    |> should' (fulfil (fun t -> "should have correct level", string t.["level"] = "warn"))
    |> should' (fulfil (fun t -> sprintf "service, expected 'tests', but was '%s'" (string t.["service"]),
                                 string t.["service"] = "tests"))
    |> should' (fulfil (fun t -> "should have correct path", string t.["path"] = "a.b.c"))
    // and data keys/values should be on the root of the object:
    |> should' (fulfil (fun t -> "should contain data-key", string t.["data-key"] = "data-value"))
    |> should' (fulfil (fun t -> "should contain 'integration' tag", string t.["tags"].[0] = "integration"))
    |> should' (fulfil (fun t -> "should contain 'e' serialized value", string t.["e"].["Message"] = "darn"))
    |> should' (fulfil (fun t -> "should contain 'exception' serialized value", string t.["exception"].["Message"] = "actual exn"))
    |> thatsIt

  // Graphite Target

  open StubTcp

  [<Test>]
  let ``initialising graphite target`` () =
    let client = new StubWriterClient(false)
    let conf = Graphite.GraphiteConf.Create("localhost", clientFac = fun a b -> client :> WriteClient)
    let graphite = Graphite.create conf "graphite-target"
    let instance = graphite.initer { serviceName = "tests" }
    instance.name =? "graphite-target"

    (because "shutting down the target" <| fun () ->
      instance |> finaliseTarget
      client.WasDisposed)
    |> should be true
    |> thatsIt

  [<Test>]
  let ``expecting output in form 'metric_path value timestamp\n'`` () =
    let client = new StubWriterClient(false)
    let conf = Graphite.GraphiteConf.Create("localhost", clientFac = fun a b -> client :> WriteClient)
    let graphite = Graphite.create conf "graphite-target"

    let logary =
      confLogary "tests"
      |> withRules [ graphite |> Rules.forTarget (Regex(".*")) (fun x -> true) Info ]
      |> withTargets [ graphite ]
      |> validateLogary
      |> runLogary

    let gauge = "test-gauge" |> Registry.getGauge logary.registry |> Async.RunSynchronously
    4.2 |> Gauge.putf gauge
    logary |> shutdownLogary |> Async.RunSynchronously |> ignore

    Assert.That(client.ToString(), Is.StringMatching(@"test\-gauge 4\.2 \d+"))

  // Health Checks

  open HealthChecks
  open System.Net.NetworkInformation
  
  let private untilPred maxWait fPred =
    let spy msg = let doubleSpy = printfn "%s: %A" msg in Seq.map doubleSpy
    let sleep = 20
    Seq.unfold (fun s -> if s < maxWait then Some(s+sleep, s+sleep) else printfn "n" ; None) 0
    |> Seq.map (fun x -> System.Threading.Thread.Sleep(sleep) ; x)
    |> spy "sleep"
    |> Seq.skipWhile (not<<fPred)
    |> Seq.isEmpty
    |> not

  [<Test>]
  let ``creating primitive check`` () =
    let a () = async { return Unhealthy <| Failure.Create("Bad bad bad") }
    let h = fromFn "a" a
    let gotUnhealthy = untilPred 1000 (fun i -> match h.Check () with Unhealthy _ -> true | _ -> false)
    match h.Check () with
    | Unhealthy res -> res.Message =? "Bad bad bad"
    | _ -> failwith "expected unhealthy result"

  [<Test; Explicit("does network IO"); Category("Integration")>]
  let ``creating real health check`` () =
    let pingHafSe () =
      async {
        use p = new Ping()
        let awaitPong = Async.AwaitEvent(p.PingCompleted, p.SendAsyncCancel)
        try
          p.SendAsync("haf.se", 1000, obj())
          let! complete = awaitPong
          if complete.Cancelled then
            return Unhealthy <| Failure.Create("Ping was cancelled")
          elif not (complete.Error = null) then
            return Unhealthy <| Failure.Create("Ping got error", Some <| complete.Error)
          else return Healthy
        with e ->
          return Unhealthy <| Failure.Create("Exception when pinging haf.se", Some(e)) }

    let h = fromFn "ping haf.se" pingHafSe
    // TODO: await single check
    let gotUnhealthy = untilPred 10000 <| fun i -> match h.Check () with Unhealthy _ -> true | _ -> false
    gotUnhealthy =? false
    //let gotHealthy = untilPred 10000 <| fun i -> h.Check = Healthy
    //gotHealthy =? true
