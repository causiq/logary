module Logary.Tests.Registry

open System
open Hopac
open Hopac.Infixes
open NodaTime
open Expecto.Flip
open Logary
open Logary.Internals
open Logary.Message
open Logary.Configuration

let testLogger = Log.create "Logary.Tests.Registry"

let debug message = testLogger.logWithBP Debug (eventX message)

let timeout = Duration.FromSeconds 1.

let tests = [
  testCaseJob "from Config and Multi shutdown" <| job {
    do! debug "Starting LogManager..."
    let! logm =
      Config.create "svc" "localhost"
      |> Config.ilogger (ILogger.LiterateConsole Verbose)
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
      let! (logm, out, error) = Utils.buildLogManager ()
      let lg = logm.getLogger "logger.test"

      do! lg.infoWithAck (eventX "test info msg")
      do! lg.errorWithAck (eventX "test error msg")

      let outStr = out.ToString()
      let errorStr = error.ToString()
      outStr |> Expect.stringContains "should contains" "info msg"
      errorStr |> Expect.stringContains "should contains" "error msg"

      let timeout =  Duration.FromSeconds 3L
      let! _ = logm.shutdown(timeout, timeout)

      do! lg.errorWithBP (eventX "error after shutdown")

      let errorOutput = error.ToString()
      if errorOutput.Contains("after") then Expecto.Tests.failtestf "should not contains after, actual %s" errorOutput
    }

  testCaseJob "getLogger with middleware" <| job {
    let! logm, out, error  = Utils.buildLogManager ()
    let correlationId = Guid.NewGuid().ToString("N")
    let customMid = Middleware.context "correlationId" correlationId
    let lg = logm.getLoggerWithMiddleware ("logger.test", customMid)
    do! lg.infoWithAck (eventX "test info msg")
    do! lg.errorWithAck (eventX "test error msg")

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

  testCaseJob "log with span" (job {
    let! manager, out, _  = Utils.buildLogManagerWith (fun conf -> conf |> Config.middleware Middleware.ambientSpanId)

    let checkSpanId spanId = job {
      do! manager.flushPending ()
      clearStream out
      |> Expect.stringContains "should have spanId" (spanId.ToString())
    }

    let lga = manager.getLogger "logger.a"
    use rootSpan = lga.startSpan "some root span"
    let lgb = manager.getLogger "logger.b"
    use _ = lgb.startSpan "some child span"
    use _ = lgb.startSpan "some grand span"
    do! lgb.infoWithAck (eventX "some log message")
    do! timeOutMillis 100

    clearStream out
    |> Expect.stringContains "should have spanId" "_logary.spanId"

    // use explicitly set style
    use spanFromRoot = lgb.startSpan("some child span", rootSpan)
    do! lgb.infoWithAck (eventX "some log message" >> setSpanId spanFromRoot.info.spanId)
    do! checkSpanId spanFromRoot.info.spanId
    do! manager.shutdown ()
  })

  testCaseJob "switch logger level" (job {
    let! logm, out, error = Utils.buildLogManager ()
    let lg = logm.getLogger "logger.test"
    do! lg.debugWithAck (eventX "test debug msg")
    do! lg.verboseWithAck (eventX "test verbose msg")
    do! lg.infoWithAck (eventX "test info msg")
    do! lg.errorWithAck (eventX "test error msg")
    do! logm.flushPending ()

    let outStr = clearStream out
    let errorStr = clearStream error
    (outStr.Contains("debug")) |> Expect.isFalse  "should not have debug level msg, since the default rule is .* -> Info"
    (outStr.Contains("verbose")) |> Expect.isFalse "should not have verbose level msg, since the default rule is .* -> Info"
    outStr |> Expect.stringContains "should have info level msg" "info"
    errorStr |> Expect.stringContains "should have error level msg" "error"


    logm.switchLoggerLevel (".*test", LogLevel.Verbose)
    do! lg.debugWithAck (eventX "test debug msg")
    do! lg.verboseWithAck (eventX "test verbose msg")
    do! lg.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    outStr |> Expect.stringContains "should have debug level msg after switch logger level" "debug"
    outStr |> Expect.stringContains "should have verbose level msg after switch logger level" "verbose"
    outStr |> Expect.stringContains "should have info level msg" "info"

    let lgWithSameNameAfterSwitch = logm.getLogger "logger.test"
    do! lgWithSameNameAfterSwitch.debugWithAck (eventX "test debug msg")
    do! lgWithSameNameAfterSwitch.verboseWithAck (eventX "test verbose msg")
    do! lgWithSameNameAfterSwitch.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    outStr |> Expect.stringContains "should have debug level msg after switch logger level" "debug"
    outStr |> Expect.stringContains "should have verbose level msg after switch logger level" "verbose"
    outStr |> Expect.stringContains "should have info level msg" "info"

    let lgWithDiffNameAfterSwitch = logm.getLogger "xxx.yyy.zzz.test"
    do! lgWithDiffNameAfterSwitch.debugWithAck (eventX "test debug msg")
    do! lgWithDiffNameAfterSwitch.verboseWithAck (eventX "test verbose msg")
    do! lgWithDiffNameAfterSwitch.infoWithAck (eventX "test info msg")
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
            let flushWork = Hopac.timeOutMillis 200 ^=>. ack *<= ()
            (flushWork <|> nack) >>= loop
          | Log (_, ack) ->
            ack *<= () >>= loop

          api.shutdownCh ^=> fun ack -> Hopac.timeOutMillis 200 ^=>. ack *<= ()
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
      |> Config.processing (Events.events |> Events.sink ["mockTarget"])
      |> Config.build

    let lg = logm.getLogger "logger.test"
    do! lg.infoWithBP (eventX "can be logger with no blocking")
    do! lg.infoWithBP (eventX "can be logger with no blocking")
    do! lg.infoWithBP (eventX "can be logger with no blocking")
    do! logm.shutdown ()
    do! lg.infoWithBP (eventX "can be logger with no blocking because ring buffer")
    do! lg.infoWithBP (eventX "can be logger with no blocking because ring buffer")
    do! lg.infoWithBP (eventX "can be logger with no blocking because check registry closed")
    do! lg.infoWithBP (eventX "can be logger with no blocking because check registry closed")
    do! lg.infoWithBP (eventX "can be logger with no blocking because check registry closed")
  })
]
