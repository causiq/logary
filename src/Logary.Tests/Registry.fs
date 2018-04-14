module Logary.Tests.Registry

open System
open Hopac
open Hopac.Infixes
open NodaTime
open Expecto
open Logary
open Logary.Internals
open Logary.Message
open Logary.Configuration
open Logary.EventProcessing
open System.Text.RegularExpressions

let tests = [
  testCaseJob "from Config and Multi shutdown" <| job {
    let! logm = Config.create "svc" "localhost" |> Config.build
    let ri = logm.runtimeInfo
    Expect.equal ri.service "svc" "should have service"
    Expect.equal ri.host "localhost" "should have host"

    let timeout =  Duration.FromSeconds 3L

    let! (finfo, sinfo) = logm.shutdown (timeout, timeout)
    let none = (List.empty<string>,List.empty<string>)
    Expect.equal finfo (FlushInfo none) "shoule have no targets"
    Expect.equal sinfo (ShutdownInfo none) "shoule have no targets"
    do! logm.shutdown ()
    do! logm.flushPending ()
    do! logm.shutdown ()
    let! (finfo) = logm.flushPending (timeout)
    Expect.equal finfo (FlushInfo (["registry is closed"],[])) "shoule show registry is shutdown"

  }

  testCaseJob "after shutting down no logging happens" <|
    job {
      let! (logm, out, error) = Utils.buildLogManager ()
      let loggername = PointName.parse "logger.test"

      let lg = logm.getLogger loggername

      do! lg.infoWithAck (eventX "test info msg")
      do! lg.errorWithAck (eventX "test error msg")

      let outStr = out.ToString()
      let errorStr = error.ToString()
      Expect.stringContains outStr "info msg" "shoule"
      Expect.stringContains errorStr "error msg" "shoule"

      let timeout =  Duration.FromSeconds 3L
      let! _ = logm.shutdown(timeout, timeout)

      do! lg.errorWithBP (eventX "error after shutdown")

      let errorOutput = error.ToString()
      if errorOutput.Contains("after") then Tests.failtestf "should not contains after, actual %s" errorOutput
    }

  testCaseJob "getlogger with middleware" <| job {
    let! (logm, out, error)  = Utils.buildLogManager ()
    let loggername = PointName.parse "logger.test"
    let correlationId = Guid.NewGuid().ToString("N")
    let customMid = Middleware.context "correlationId" correlationId
    let lg = logm.getLoggerWithMiddleware loggername customMid
    do! lg.infoWithAck (eventX "test info msg")
    do! lg.errorWithAck (eventX "test error msg")

    let outStr = out.ToString()
    let errorStr = error.ToString()
    Expect.stringContains outStr "svc" "shoule have svc ctx info"
    Expect.stringContains outStr "localhost" "shoule have host ctx info"
    Expect.stringContains errorStr "svc" "shoule have svc ctx info"
    Expect.stringContains errorStr "localhost" "shoule have host ctx info"
    Expect.stringContains outStr correlationId "shoule have correlationId ctx info"
    Expect.stringContains errorStr correlationId "shoule have correlationId ctx info"

    do! logm.shutdown ()
  }

  testCaseJob "log with scope" (job {
    let dataSlot = DataSlot.useDefault ()
    let! (logm, out, _)  = Utils.buildLogManagerWith (fun r -> r |> Config.middleware (Middleware.fillWithContextSlot dataSlot) )

    for _ in 1..10 do
      let latch = Latch 4
      let loggername = PointName.parse "logger.test"
      let lg1 = logm.getLogger loggername

      let s1 = dataSlot.push (fun _ -> "scope-1")
      lg1.infoWithBP (eventX "scope1") |> Hopac.startWithActions ignore (latch.Decrement >> start)

      let s2 = dataSlot.push (fun _ -> "scope-2")

      let lg2 = logm.getLogger (PointName.parse "logger.test.another")
      lg2.infoWithBP (eventX "scope2") |> Hopac.startWithActions ignore (latch.Decrement >> start)

      do s2.Dispose ()
      lg2.infoWithBP (eventX "2dispose") |> Hopac.startWithActions ignore (latch.Decrement >> start)

      do s1.Dispose ()
      lg2.infoWithBP (eventX "1dispose") |> Hopac.startWithActions ignore (latch.Decrement >> start)

      do! latch |> Latch.await |> Alt.afterJob logm.flushPending

      let outStr = clearStream out
      Expect.isRegexMatch outStr (new Regex("""scope1.*?_logary.scope => \["scope-1"\]""", RegexOptions.Singleline)) "shoule have scope-1 value"
      Expect.isRegexMatch outStr (new Regex("""scope2.*?_logary.scope => \["scope-2", "scope-1"\]""", RegexOptions.Singleline)) "shoule have scope-1/2 value"
      Expect.isRegexMatch outStr (new Regex("""2dispose.*?_logary.scope => \["scope-1"\]""", RegexOptions.Singleline)) "shoule only have scope-1 value"
      Expect.isRegexMatch outStr (new Regex("1dispose.*?_logary.scope => \[\]", RegexOptions.Singleline)) "shoule have no scope value"

    do! logm.shutdown ()
  })

  testCaseJob "log with span" (job {
    let! (logm, out, _)  = Utils.buildLogManager ()
    let checkSpanId spanId = job {
      do! logm.flushPending ()
      clearStream out
      |> Expecto.Flip.Expect.stringContains "should has spanId" spanId
    }

    let lga = logm.getLogger "logger.a"
    do! job {
      use rootSpan = lga.createSpan "" (Message.eventX "some root span")
      let lgb = logm.getLogger "logger.b"
      use span1 = lgb.createSpan rootSpan.id (Message.eventX "some child span 1")
      let span2 = lgb.createSpan rootSpan.id (Message.eventX "some child span 2")
      do span2.Dispose ()

      do! checkSpanId "#localhost-svc.1.2"

      for i in 1..2 do
        use spani = lgb.createSpan span1.id (fun level -> Message.eventFormat(level, "some grand span {taskId}", i) )
        do! timeOutMillis 500
        do! lgb.infoWithAck (eventX "some log message" >> setSpanId spani.id)
        do! checkSpanId (sprintf "#localhost-svc.1.1.%d" i)
    }

    do! checkSpanId ("#localhost-svc.1")
  })

  testCaseJob "switch logger level" (job {
    let! (logm, out, error)  = Utils.buildLogManager ()
    let loggername = PointName.parse "logger.test"
    let lg = logm.getLogger loggername
    do! lg.debugWithAck (eventX "test debug msg")
    do! lg.verboseWithAck (eventX "test verbose msg")
    do! lg.infoWithAck (eventX "test info msg")
    do! lg.errorWithAck (eventX "test error msg")
    do! logm.flushPending ()

    let outStr = clearStream out
    let errorStr = clearStream error
    Expect.isFalse (outStr.Contains("debug")) "shoule not have debug level msg, since the default rule is .* -> Info"
    Expect.isFalse (outStr.Contains("verbose")) "shoule not have verbose level msg, since the default rule is .* -> Info"
    Expect.stringContains outStr "info" "shoule have info level msg"
    Expect.stringContains errorStr "error" "shoule have error level msg"


    logm.switchLoggerLevel (".*test", LogLevel.Verbose)
    do! lg.debugWithAck (eventX "test debug msg")
    do! lg.verboseWithAck (eventX "test verbose msg")
    do! lg.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    Expect.stringContains outStr "debug" "shoule have debug level msg after switch logger level"
    Expect.stringContains outStr "verbose" "shoule have verbose level msg after switch logger level"
    Expect.stringContains outStr "info" "shoule have info level msg"

    let lgWithSameNameAfterSwitch = logm.getLogger loggername
    do! lgWithSameNameAfterSwitch.debugWithAck (eventX "test debug msg")
    do! lgWithSameNameAfterSwitch.verboseWithAck (eventX "test verbose msg")
    do! lgWithSameNameAfterSwitch.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    Expect.stringContains outStr "debug" "shoule have debug level msg after switch logger level"
    Expect.stringContains outStr "verbose" "shoule have verbose level msg after switch logger level"
    Expect.stringContains outStr "info" "shoule have info level msg"

    let lgWithDiffNameAfterSwitch = logm.getLogger (PointName.parse "xxx.yyy.zzz.test")
    do! lgWithDiffNameAfterSwitch.debugWithAck (eventX "test debug msg")
    do! lgWithDiffNameAfterSwitch.verboseWithAck (eventX "test verbose msg")
    do! lgWithDiffNameAfterSwitch.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    Expect.isFalse (outStr.Contains("debug")) "shoule not have debug level msg after switch logger level"
    Expect.isFalse (outStr.Contains("verbose")) "shoule not have verbose level msg after switch logger level"
    Expect.stringContains outStr "info" "shoule have info level msg"

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
          | Log (msg, ack) ->
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
    Expect.equal fack [] "should have no flush ack target"
    Expect.contains ftimeout "mockTarget" "should have flush timeout target"

    Expect.equal sack [] "should have no shutdown ack target"
    Expect.contains stimeout "mockTarget" "should have shutdown timeout target"

    let! logm =
        Config.create "svc" "host"
        |> Config.target mockTarget
        |> Config.build

    let ms300 =  Duration.FromMilliseconds 300L
    let! (finfo, sinfo) = logm.shutdown (ms300, ms300)
    let (FlushInfo (fack, ftimeout)) = finfo
    let (ShutdownInfo (sack, stimeout)) = sinfo
    Expect.equal ftimeout [] "should have no flush timeout target"
    Expect.contains fack "mockTarget" "should have flush ack target"

    Expect.equal stimeout [] "should have no shutdown timeout target"
    Expect.contains sack "mockTarget" "should have shutdown ack target"


    let mockTarget =
      TargetConf.createSimple server "mockTarget"
      |> TargetConf.bufferSize 2us

    let! logm =
      Config.create "svc" "host"
      |> Config.target mockTarget
      |> Config.processing (Events.events |> Events.sink ["mockTarget"])
      |> Config.build

    let lg = logm.getLogger (PointName.parse "logger.test")
    do! lg.infoWithBP (eventX "can be logger with no blocking")
    do! lg.infoWithBP (eventX "can be logger with no blocking")
    do! lg.infoWithBP (eventX "can be logger with no blocking")
    do! logm.shutdown ()
    do! lg.infoWithBP (eventX "can be logger with no blocking beacuse ring buffer")
    do! lg.infoWithBP (eventX "can be logger with no blocking beacuse ring buffer")
    do! lg.infoWithBP (eventX "can be logger with no blocking beacuse check registry closed")
    do! lg.infoWithBP (eventX "can be logger with no blocking beacuse check registry closed")
    do! lg.infoWithBP (eventX "can be logger with no blocking beacuse check registry closed")

  })
]
