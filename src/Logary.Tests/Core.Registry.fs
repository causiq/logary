module Logary.Tests.Registry

open Expecto
open System
open Hopac
open Hopac.Infixes
open NodaTime
open Expecto.Flip
open Logary
open Logary.Internals
open Logary.Configuration
open Logary.Trace

let testLogger = Log.create "Logary.Tests.Registry"

let debug (message: string) = testLogger.logBP(Model.EventMessage(message, level=Debug)) ^-> ignore

let timeout = Duration.FromSeconds 1.

let tests = [
  testCaseJob "from Config and Multi shutdown" <| job {
    do! debug "Starting LogManager..."
    let! logm =
      Config.create "svc" "localhost"
      |> Config.ilogger (ILogger.Console Verbose)
      |> Config.build
    do! debug "Started LogManager."

    let ri = logm.runtimeInfo
    ri.service |> Expect.equal "should have service" "svc"
    ri.host |> Expect.equal "should have host" "localhost"

    do! debug "Shutting down LogManager (1)..."
    let! finfo, sinfo = logm.shutdown(timeout, timeout)
    let none = List.empty<string>, List.empty<string>
    finfo |> Expect.equal "Should have no targets" (FlushInfo none)
    sinfo |> Expect.equal "Should have no targets" (ShutdownInfo none)
    do! debug "Shut down LogManager (1)."
    do! debug "Shutting down LogManager (2)."
    do! logm.shutdown ()
    do! debug "Shut down LogManager (2). Flushing LogManager (1)..."
    do! logm.flushPending ()
    do! debug "Flushed LogManager (1). Shutting down LogManager (3)..."
    do! logm.shutdown ()
    do! debug "Shut down LogManager (3). Flushing LogManager (2)..."
    let! finfo = logm.flushPending(timeout)
    do! debug "Flushed LogManager (2)."
    finfo
      |> Expect.equal "FlushPending should notify called that Registry is shut down."
                      (FlushInfo (["registry is closed"],[]))
  }

  testCaseJob "after shutting down no logging happens" <|
    job {
      let! logm, out, error = buildLogManager ()
      let lg = logm.getLogger "logger.test"

      do! lg.eventAck("test info msg", fun m -> m.level <- Info)
      do! lg.errorAckIgnore("test error msg")

      let outStr = out.ToString()
      let errorStr = error.ToString()
      outStr |> Expect.stringContains "should contains" "info msg"
      errorStr |> Expect.stringContains "should contains" "error msg"

      let timeout =  Duration.FromSeconds 3L
      let! _ = logm.shutdown(timeout, timeout)

      do! lg.errorBPIgnore("error after shutdown")

      let errorOutput = error.ToString()
      if errorOutput.Contains("after") then Expecto.Tests.failtestf "should not contains after, actual %s" errorOutput
    }

  testCaseJob "getLogger with middleware" <| job {
    let correlationId = Guid.NewGuid().ToString("N")
    let customMid = Middleware.context "correlationId" (Value.Str correlationId)
    let! logm, out, error = buildLogManagerWith (Config.middleware customMid)
    let lg = logm.getLogger("logger.test")
    do! lg.eventAck "test info msg"
    do! lg.errorAckIgnore "test error msg"

    let outStr = out.ToString()
    let errorStr = error.ToString()
    outStr |> Expect.stringContains "should have svc ctx info" "svc"
    outStr |> Expect.stringContains "should have host ctx info" "localhost"
    errorStr |> Expect.stringContains "should have svc ctx info" "svc"
    errorStr |> Expect.stringContains "should have host ctx info" "localhost"
    outStr |> Expect.stringContains "should have correlationId ctx info" correlationId
    errorStr |> Expect.stringContains "should have correlationId ctx info" correlationId

    do! logm.shutdown ()
  }

  Tests.testSequencedGroup "ambient spans (must run on its own)" <|
  testList "ambient spans" [
    let prepare = job {
      let! manager, out, _  = buildLogManagerWith (fun conf -> conf |> Config.middleware Middleware.ambientSpanId)
      let checkSpanId spanId = job {
        do! manager.flushPending ()
        let output = clearStream out
        let message = sprintf "The output should have spanId, but was:\n%s" output
        output
          |> Expect.stringContains message (spanId.ToString())
      }
      return manager, checkSpanId, out
    }

    let tryWith xs2xJ =
      prepare >>= fun (manager, checkSpanId, out) ->
      Job.tryFinallyJob (xs2xJ (manager, checkSpanId, out)) (manager.shutdown())

    yield testCaseJob "by default has no ambient spans" (job {
      ActiveSpan.getSpan()
        |> Expect.isNone "Initially has no ambient span"
    })

    yield ptestCaseJob "ambientonanza (skipped because we can't trust AsyncLocal) in F#" (tryWith <| fun (manager, checkSpanId, _) -> job {
      let a = manager.getLogger "logger.a"
      let b = manager.getLogger "logger.b"

      let outsideJob () =
        use parentSpan = a.startSpan("parent", enableAmbient=true)
        let active = ActiveSpan.getContext ()
        active
          |> Expect.equal "Has an ambient span from 'parent' span" (Some parentSpan.context)

        let childSpan = parentSpan.startSpan("child", parentSpan)
        ActiveSpan.getSpan()
          |> Expect.equal "Has an ambient span from 'parent' span — 'child' span did not opt in to ambient" (Some parentSpan.context.spanId)

        let grandChildSpan = childSpan.startSpan("grand-child", childSpan, enableAmbient=true)
        ActiveSpan.getSpan()
          |> Expect.equal "Has an ambient span from 'grand-child' span" (Some grandChildSpan.context.spanId)

        // this log message will have the ambient spanId of 'grand-child' span:
        b.info "MAJOR EVENT HAPPENED"

        // reset ambient:
        grandChildSpan.finish() |> ignore

        ActiveSpan.getSpan()
          |> Expect.equal "Has an ambient span from 'parent' span" (Some parentSpan.context.spanId)

        childSpan.finish() |> ignore
        ActiveSpan.getSpan()
          |> Expect.equal "Has an ambient span from 'parent' span" (Some parentSpan.context.spanId)

        parentSpan.finish() |> ignore
        ActiveSpan.getSpan()
          |> Expect.isNone "Does not have an ambient SpanId after finishing the 'parent' Span"

        grandChildSpan.context.spanId

      let expectedSpanId = outsideJob ()

      // ensure that MAJOR EVENT received the ambient spanId
      do! checkSpanId expectedSpanId
    })

    yield testCaseJob "log ambient into span" (tryWith <| fun (manager, assertHasSpanId, _) -> job {
      let a = manager.getLogger "logger.a"
      let b = manager.getLogger "logger.b"
      use parentSpan = a.startSpan("parent", enableAmbient=true)
      use childSpan = b.startSpan("child 2", parentSpan)

      // you can finish the parent before the child according to the specs
      do! parentSpan.finishAck()

      // log via a non-Span attached logger;
      do! b.eventAck("some log message", fun m -> m.spanId <- Some childSpan.context.spanId)

      // now let's finish the child
      do! childSpan.finishAck()

      // assert!
      do! assertHasSpanId childSpan.context.spanId
    })
  ]

  testCaseJob "switch logger level" (job {
    let! logm, out, error = buildLogManager ()
    let lg = logm.getLogger "logger.test"
    do! lg.eventAck("test debug msg", fun m -> m.level <- Debug)
    do! lg.eventAck("test verbose msg", fun m -> m.level <- Verbose)
    do! lg.eventAck("test info msg", fun m -> m.level <- Info)
    do! lg.errorAckIgnore("test error msg")
    do! logm.flushPending ()

    let outStr = clearStream out
    let errorStr = clearStream error
    (outStr.Contains("debug")) |> Expect.isFalse  "should not have debug level msg, since the default rule is .* -> Info"
    (outStr.Contains("verbose")) |> Expect.isFalse "should not have verbose level msg, since the default rule is .* -> Info"
    outStr |> Expect.stringContains "should have info level msg" "info"
    errorStr |> Expect.stringContains "should have error level msg" "error"


    logm.switchLoggerLevel (".*test", LogLevel.Verbose)
    do! lg.eventAck("test debug msg", fun m -> m.level <- Debug)
    do! lg.eventAck("test verbose msg", fun m -> m.level <- Verbose)
    do! lg.eventAck("test info msg", fun m -> m.level <- Info)
    do! logm.flushPending ()
    let outStr = clearStream out
    outStr |> Expect.stringContains "should have debug level msg after switch logger level" "debug"
    outStr |> Expect.stringContains "should have verbose level msg after switch logger level" "verbose"
    outStr |> Expect.stringContains "should have info level msg" "info"

    let lgWithSameNameAfterSwitch = logm.getLogger "logger.test"
    do! lgWithSameNameAfterSwitch.eventAck("test debug msg", fun m -> m.level <- Debug)
    do! lgWithSameNameAfterSwitch.eventAck("test verbose msg", fun m -> m.level <- Verbose)
    do! lgWithSameNameAfterSwitch.eventAck("test info msg", fun m -> m.level <- Info)
    do! logm.flushPending ()
    let outStr = clearStream out
    outStr |> Expect.stringContains "should have debug level msg after switch logger level" "debug"
    outStr |> Expect.stringContains "should have verbose level msg after switch logger level" "verbose"
    outStr |> Expect.stringContains "should have info level msg" "info"

    let lgWithDiffNameAfterSwitch = logm.getLogger "xxx.yyy.zzz.test"
    do! lgWithDiffNameAfterSwitch.eventAck("test debug msg", fun m -> m.level <- Debug)
    do! lgWithDiffNameAfterSwitch.eventAck("test verbose msg", fun m -> m.level <- Verbose)
    do! lgWithDiffNameAfterSwitch.eventAck("test info msg", fun m -> m.level <- Info)
    do! logm.flushPending ()
    let outStr = clearStream out
    (outStr.Contains("debug")) |> Expect.isFalse "should not have debug level msg after switch logger level"
    (outStr.Contains("verbose")) |> Expect.isFalse "should not have verbose level msg after switch logger level"
    outStr |> Expect.stringContains "should have info level msg" "info"
    do! logm.shutdown ()
  })

  testCaseJob "flush/shutdown timeout and logging after shutdown with no blocking" (job {
    let server (api: TargetAPI) =
      let rec loop () =
        Alt.choose [
          RingBuffer.take api.requests ^=> function
          | Flush (ack, nack) ->
            let flushWork = timeOutMillis 200 ^=>. ack *<= ()
            (flushWork <|> nack) >>= loop
          | Log (_, ack) ->
            ack *<= () >>= loop

          api.shutdownCh ^=> fun ack -> timeOutMillis 200 ^=>. ack *<= ()
        ] :> Job<_>

      loop ()

    let mockTarget = TargetConf.createSimple server "mockTarget"

    let! logm =
      Config.create "svc" "host"
      |> Config.target mockTarget
      |> Config.build

    let ms100 =  Duration.FromMilliseconds 100L
    let! (finfo, sinfo) = logm.shutdown (ms100, ms100)
    let (FlushInfo (fack, ftimeout)) = finfo
    let (ShutdownInfo (sack, stimeout)) = sinfo
    fack |> Expect.equal "should have no flush ack target" []
    ftimeout |> Expect.contains "should have flush timeout target" "mockTarget"

    sack |> Expect.equal "should have no shutdown ack target"  []
    stimeout |> Expect.contains "should have shutdown timeout target" "mockTarget"

    let! logm =
        Config.create "svc" "host"
        |> Config.target mockTarget
        |> Config.build

    let ms300 =  Duration.FromMilliseconds 300L
    let! finfo, sinfo = logm.shutdown (ms300, ms300)
    let (FlushInfo (fack, ftimeout)) = finfo
    let (ShutdownInfo (sack, stimeout)) = sinfo
    ftimeout |> Expect.equal "should have no flush timeout target" []
    fack |> Expect.contains "should have flush ack target" "mockTarget"

    stimeout |> Expect.equal "should have no shutdown timeout target" []
    sack |> Expect.contains "should have shutdown ack target" "mockTarget"


    let mockTarget =
      TargetConf.createSimple server "mockTarget"
      |> TargetConf.bufferSize 2us

    let! logm =
      Config.create "svc" "host"
      |> Config.target mockTarget
      |> Config.processing (Events.events |> Events.setTarget "mockTarget")
      |> Config.build

    let lg = logm.getLogger "logger.test"

    let! res = lg.logBP (Model.EventMessage "Can be successfully logged")
    res
      |> Expect.isTrue "Should log successfully with logBP: ...: Alt<bool>-API"

    // AFTER SHUTTING DOWN:
    do! logm.shutdown()

    let! res = lg.logAck (Model.EventMessage "Cannot be logged because the registry from which the logger comes, is closed.")
    res
      |> Expect.isFalse "Should fail to log, asserting on Alt<bool>-API"

    match! lg.logWithAck (true, Model.EventMessage "Second try") with
    | Ok _ ->
      failtestf "Should not be possible to place message in Targets' buffers, but Alt<Ok _> was result from logWithAck"

    | Result.Error m ->
      m.ckind
        |> Expect.equal "Should fail due to the RegistryClosed reason" ControlMessageKind.RegistryClosed

  })
]
