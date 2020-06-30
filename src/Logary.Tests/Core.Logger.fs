module Logary.Tests.Logger

open System.Threading
open System.Threading.Tasks
open Expecto
open Expecto.Flip
open System
open Hopac
open Logary
open Logary.Trace
open NodaTime

[<Tests>]
let tests =
  testList "Logger" [
    testList "api" [
      let assertsOnLoggedData (m: _) =
        m.waitForBuffers
          |> Expect.isFalse "`time` does a non-blocking log by default"

        let interfaces = m.message.GetType().GetInterfaces()

        //printfn "interfaces %A" interfaces
        interfaces
          |> Array.exists (fun i -> i.IsAssignableFrom(typeof<Logary.GaugeMessage>))
          |> Expect.isTrue "Has one interface, GaugeMessage, at the very least"

        let gm = m.message :?> GaugeMessage

        gm.name
          |> Expect.equal "Has the right name on the message" (PointName.parse "Core.Logger.api.time")

        gm.labels
          |> Expect.isNonEmpty "Has labels, because it's a Gauge message"

        // has call-site info in the fields
        for field in [ "file"; "site"; "lineNo" ] do // no colNo for .Net
          match m.message.tryGetField field with
          | None ->
            failtestf "Did not find field '%s' in 'fields' dictionary" field
          | Some _ -> ()

        gm.parentSpanId
          |> Expect.isNone "Has no spanId"

      let logger () = StubLogger("Core.Logger.api", Verbose)

      testCase "time" <| fun _ ->
        let logger = logger ()
        let fn input = input * 2
        let timedFn = logger.time(fn, "time")
        let res = timedFn 2
        res |> Expect.equal "Four" 4
        logger.logged.Count |> Expect.equal "Logged one message" 1

        assertsOnLoggedData logger.logged.[0]

      testCaseJob "timeJob" <| job {
        let logger = logger ()
        let! res = logger.timeJob (job { return 4 }, "time")
        res |> Expect.equal "Four" 4
        logger.logged.Count |> Expect.equal "Logged one message" 1

        assertsOnLoggedData logger.logged.[0]
      }

      let assertOutcome expected (m: LogaryMessage) =
        match m.tryGetField "outcome" with
        | None ->
          failtestf "Failed to find field '%s'" "outcome"
        | Some outcome ->
          outcome |> Expect.equal "Has outcome field" (Value.Str expected)

      testCaseJob "timeAlt ACK" <| job {
        let logger = logger ()
        let! res = logger.timeAlt (Alt.once 4, "time")

        res
          |> Expect.equal "Four" 4
        logger.logged.Count
          |> Expect.equal "Logged one message" 1

        assertsOnLoggedData logger.logged.[0]
        assertOutcome "ack" logger.logged.[0].message
      }

      testCaseJob "timeAlt NACK" <| job {
        let logger = logger ()

        let nackIV = IVar ()

        let! resP =
          Alt.choose [ logger.timeAlt (Alt.never (), "time"); nackIV :> _ ]
          |> Promise.start

        do! IVar.fill nackIV 10

        let! res = resP

        res
          |> Expect.equal "Has the value from the nackIV" 10

        let! found = logger.waitForAtLeast 1

        found
          |> Expect.isTrue "Did find at least one message"

        logger.logged.Count
          |> Expect.equal "Logged one message" 1

        assertsOnLoggedData logger.logged.[0]
        assertOutcome "nack" logger.logged.[0].message
      }

      testCaseJob "timeTask" <| job {
        let logger = logger ()
        use cts = new CancellationTokenSource()
        let runnable _ = Task.FromResult 11
        let! res = logger.timeTask(runnable, cts.Token, "time")

        res
          |> Expect.equal "Eleven" 11

        logger.logged.Count
          |> Expect.equal "Logged one message" 1

        assertsOnLoggedData logger.logged.[0]
        assertOutcome "ack" logger.logged.[0].message
      }

      testCase "scoped" <| fun _ ->
        let logger = logger ()

        let inner () =
          use scope = logger.scoped "inner"
          54

        let res = inner ()

        res
          |> Expect.equal "Has result from `inner ()`" 54

        logger.logged
          |> Expect.isNonEmpty "Has the at least one logged message"

        let m = logger.logged.[0].message :?> SpanMessage

        m.label
          |> Expect.equal "Has 'inner' as the label" "inner"

      testCaseJob "scoped follows from" <| job {
        // context
        let stubLogger = logger()

        // let main argv = ... — we start some new parent computation
        use parent = stubLogger.scoped "parent"
        parent.info "I log therefore I am"

        // overload, both Logger and SpanLogger have method `timeAlt`.
        let randomWaitA = Alt.prepareJob (fun () -> Job.Random.bind (fun i -> Job.result (int (abs <| (int64 i) % 150L))) |> Job.map timeOutMillis)
        let xA = parent.timeAlt(randomWaitA, "alt worker")

        // the work you'll schedule repeatedly which might go outside of Parent
        let prodA = parent.timeAltProducer(randomWaitA, "alt producer")

        let shutdownIV = IVar ()

        // the work-loop
        let rec cronJob () =
          Alt.choose [
            prodA |> Alt.afterJob cronJob
            xA |> Alt.afterJob cronJob
            shutdownIV :> Alt<unit>
          ]

        let! loopCompleteP = Promise.start (cronJob () |> Alt.afterFun (parent.finish >> ignore))

        // let the work-loop go for a bit
        do! timeOutMillis 300

        // then shut it down and get the logged messages
        do! IVar.fill shutdownIV ()

        // wait for shutdown
        do! loopCompleteP

        // wait for asynchronously logged messages to appear in stub
        let! wasFound =
          stubLogger.waitForAtLeast(
            3,
            withMatch=fun record -> record.message.tryGetFieldBool "cancelled" = Some true)

        wasFound
          |> Expect.isTrue "Found at least three messages with the 'cancelled=Some true' field"

        let messages =
          stubLogger.logged
            |> Seq.map (fun x -> x.message)
            |> Seq.choose (fun x -> x.tryGetAs<SpanMessage>())
            |> Seq.filter (fun x -> x.label <> "parent")
            |> List.ofSeq

        // from the above, we expect:
        for message in messages do
          let span = message.getAsOrThrow<SpanMessage>()
          span.label
            |> Expect.stringContains "Has 'alt ' in the label" "alt "

          (span.elapsed, Duration.Zero)
            |> Expect.isGreaterThan "Should have an elapsed value ≥ 0."

          span.parentSpanId
            |> Option.get
            |> Expect.equal "That of 'parent'" parent.context.spanId

          span.context.parentSpanId
            |> Option.get
            |> Expect.equal "That of 'parent'" parent.context.spanId

        let fields =
          let formatField (KeyValue (s, v)) = sprintf "%s=%O" s v
          messages
            |> Seq.map (fun m -> sprintf "%s: %s" m.label (m.fields |> Seq.map formatField |> String.concat ", "))
            |> String.concat "\n"

        // there exists two messages that has the outcome/field cancelled=true
        let noCancelled = messages |> Seq.filter (fun m -> match m.tryGetFieldBool "cancelled" with Some x -> x | _ -> false) |> Seq.length
        (noCancelled, 2)
          |> Expect.isGreaterThan (sprintf "Should be at least two cancelled messages. Fields:\n%s" fields)

        let acked =
          messages |> Seq.filter (fun m -> match m.tryGetFieldString "outcome" with Some x when x = "ack" -> true | _ -> false)

        (Seq.length acked, 2)
          |> Expect.isGreaterThan "There are should be more than two acked messages"

        // and there are others, too
        (Seq.length messages, 2)
          |> Expect.isGreaterThan "Has more than two messages"
      }


        // similarly for tasks
        //use cts = new CancellationTokenSource()
        //let workU2T = parent.timeTaskProducer((fun _ -> Task.FromResult 42), cts.Token, "task worker")
        //ignore workU2T
    ]

    testList "Span" [
      testCase ":> SpanMessage" <| fun _ ->
        typeof<SpanMessage>.IsAssignableFrom(typeof<Span>)
          |> Expect.isTrue "Should implement SpanMessage"

      testCase ":> SpanOps" <| fun _ ->
        typeof<SpanOps>.IsAssignableFrom(typeof<Span>)
          |> Expect.isTrue "Should implement SpanOps"
    ]

    testList "SpanLogger" [
      testCase ":> Logger" <| fun _ ->
        typeof<Logger>.IsAssignableFrom(typeof<SpanLogger>)
          |> Expect.isTrue "Should implement Logger"

      testCase ":> LoggerScope" <| fun _ ->
        typeof<LoggerScope>.IsAssignableFrom(typeof<SpanLogger>)
          |> Expect.isTrue "Should implement LoggerScope"

      testCase ":> Span" <| fun _ ->
        typeof<Span>.IsAssignableFrom(typeof<SpanLogger>)
          |> Expect.isTrue "Should implement Span"

      testCase ":> SpanOpsAdvanced" <| fun _ ->
        typeof<SpanOpsAdvanced>.IsAssignableFrom(typeof<SpanLogger>)
          |> Expect.isTrue "Should implement SpanOpsAdvanced"
    ]

    testList "LoggerScope" [
      testCase ":> Logger" <| fun _ ->
        typeof<Logger>.IsAssignableFrom(typeof<LoggerScope>)
          |> Expect.isTrue "Should implement Logger"

      testCase ":> IDisposable" <| fun _ ->
        typeof<IDisposable>.IsAssignableFrom(typeof<LoggerScope>)
          |> Expect.isTrue "Should implement IDisposable"
    ]
  ]
  |> testLabel "logary"
