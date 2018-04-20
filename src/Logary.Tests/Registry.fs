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
open Logary.EventProcessing
open Logary.SpanBuilder

let tests = [
  testCaseJob "from Config and Multi shutdown" <| job {
    let! logm = Config.create "svc" "localhost" |> Config.build
    let ri = logm.runtimeInfo
    ri.service |> Expect.equal "should have service" "svc" 
    ri.host |> Expect.equal "should have host" "localhost" 

    let timeout =  Duration.FromSeconds 3L

    let! (finfo, sinfo) = logm.shutdown (timeout, timeout)
    let none = (List.empty<string>,List.empty<string>)
    finfo |> Expect.equal "shoule have no targets" (FlushInfo none)
    sinfo |> Expect.equal "shoule have no targets" (ShutdownInfo none)
    do! logm.shutdown ()
    do! logm.flushPending ()
    do! logm.shutdown ()
    let! (finfo) = logm.flushPending (timeout)
    finfo |> Expect.equal "shoule show registry is shutdown" (FlushInfo (["registry is closed"],[]))
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
      outStr |> Expect.stringContains "shoule contains" "info msg" 
      errorStr |> Expect.stringContains "shoule contains" "error msg" 

      let timeout =  Duration.FromSeconds 3L
      let! _ = logm.shutdown(timeout, timeout)

      do! lg.errorWithBP (eventX "error after shutdown")

      let errorOutput = error.ToString()
      if errorOutput.Contains("after") then Expecto.Tests.failtestf "should not contains after, actual %s" errorOutput
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
    outStr |> Expect.stringContains "shoule have svc ctx info" "svc" 
    outStr |> Expect.stringContains "shoule have host ctx info" "localhost" 
    errorStr |> Expect.stringContains "shoule have svc ctx info" "svc" 
    errorStr |> Expect.stringContains "shoule have host ctx info" "localhost" 
    outStr |> Expect.stringContains "shoule have correlationId ctx info" correlationId 
    errorStr |> Expect.stringContains "shoule have correlationId ctx info" correlationId

    do! logm.shutdown ()
  }

  testCaseJob "log with span" (job {
    let! logm, out, _  = Utils.buildLogManagerWith (fun conf -> conf |> Config.middleware (Middleware.useAmbientSpanId ()))
    
    let checkSpanId spanId = job {
      do! logm.flushPending ()
      clearStream out
      |> Expect.stringContains "should have spanId" (SpanInfo.formatId spanId)
    }

    let lga = logm.getLogger "logger.a"
    use rootSpan = lga |> create |> setMessage (eventX "some root span") |> start
    let lgb = logm.getLogger "logger.b"
    use _ = lgb |> create |> setMessage (eventX "some child span") |> start
    use _ = lgb |> create |> setMessage (eventX "some grand span") |> start
    do! lgb.infoWithAck (eventX "some log message")
    do! timeOutMillis 100
    clearStream out
    |> Expect.stringContains "should have spanId" "_logary.spanId"

    // use explicitly set style
    use spanFromRoot = lgb |> create |> setParentSpanInfo rootSpan.info |> setMessage (eventX "some child span") |> start
    do! lgb.infoWithAck (eventX "some log message" >> setSpanId spanFromRoot.info.spanId)
    do! checkSpanId spanFromRoot.info.spanId
    do! logm.shutdown ()
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
    (outStr.Contains("debug")) |> Expect.isFalse  "shoule not have debug level msg, since the default rule is .* -> Info"
    (outStr.Contains("verbose")) |> Expect.isFalse "shoule not have verbose level msg, since the default rule is .* -> Info"
    outStr |> Expect.stringContains "shoule have info level msg" "info" 
    errorStr |> Expect.stringContains "shoule have error level msg" "error" 


    logm.switchLoggerLevel (".*test", LogLevel.Verbose)
    do! lg.debugWithAck (eventX "test debug msg")
    do! lg.verboseWithAck (eventX "test verbose msg")
    do! lg.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    outStr |> Expect.stringContains "shoule have debug level msg after switch logger level" "debug" 
    outStr |> Expect.stringContains "shoule have verbose level msg after switch logger level" "verbose" 
    outStr |> Expect.stringContains "shoule have info level msg" "info" 

    let lgWithSameNameAfterSwitch = logm.getLogger loggername
    do! lgWithSameNameAfterSwitch.debugWithAck (eventX "test debug msg")
    do! lgWithSameNameAfterSwitch.verboseWithAck (eventX "test verbose msg")
    do! lgWithSameNameAfterSwitch.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    outStr |> Expect.stringContains "shoule have debug level msg after switch logger level" "debug" 
    outStr |> Expect.stringContains "shoule have verbose level msg after switch logger level" "verbose" 
    outStr |> Expect.stringContains "shoule have info level msg" "info" 

    let lgWithDiffNameAfterSwitch = logm.getLogger (PointName.parse "xxx.yyy.zzz.test")
    do! lgWithDiffNameAfterSwitch.debugWithAck (eventX "test debug msg")
    do! lgWithDiffNameAfterSwitch.verboseWithAck (eventX "test verbose msg")
    do! lgWithDiffNameAfterSwitch.infoWithAck (eventX "test info msg")
    do! logm.flushPending ()
    let outStr = clearStream out
    (outStr.Contains("debug")) |> Expect.isFalse  "shoule not have debug level msg after switch logger level"
    (outStr.Contains("verbose")) |> Expect.isFalse  "shoule not have verbose level msg after switch logger level"
    outStr |> Expect.stringContains "shoule have info level msg" "info"
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
