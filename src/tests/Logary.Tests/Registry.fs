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

let tests = [
  testCaseAsync "from Config" <| (job {
    let! registry = Config.create "svc" "localhost" |> Config.build
    let logm = Registry.toLogManager registry
    let ri = logm.runtimeInfo
    Expect.equal ri.service "svc" "should have service"
    Expect.equal ri.host "localhost" "should have host"

    let timeout =  Duration.FromSeconds 3L
    let! (finfo, sinfo) = logm.shutdown timeout timeout

    let none = (List.empty<string>,List.empty<string>)
    Expect.equal finfo (FlushInfo none) "shoule have no targets"
    Expect.equal sinfo (ShutdownInfo none) "shoule have no targets"
  } |> Job.toAsync)

  testCaseAsync "after shutting down no logging happens" <| (job {
    let! (r, logm, out, error) = Utils.buildLogManager ()
    let loggername = PointName.parse "logger.test"

    let ilg = logm.runtimeInfo.logger
    let lg = logm.getLogger loggername

    do! lg.infoWithAck (eventX "test info msg")
    do! lg.errorWithAck (eventX "test error msg")

    let outStr = out.ToString()
    let errorStr = error.ToString()
    Expect.stringContains outStr "info msg" "shoule"
    Expect.stringContains errorStr "error msg" "shoule"

    do! Registry.shutdown r

    do! lg.errorWithBP (eventX "error after shutdown")

    let errorOutput = error.ToString()
    if errorOutput.Contains("after") then Tests.failtestf "should not contains after, actual %s" errorOutput
  } |> Job.toAsync)

  testCaseAsync "getlogger with middleware" <| (job {
    let! (r, logm, out, error)  = Utils.buildLogManager ()
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

    do! Registry.shutdown r
  } |> Job.toAsync)

  testCaseAsync "flush/shutdown timeout" <| (job {
    let server (ri : RuntimeInfo, api : TargetAPI) =
      let rec loop () =
        Alt.choose [
          RingBuffer.take api.requests ^=> function
          | Flush (ack, nack) ->
            let flushWork = Hopac.timeOutMillis 200 ^=>. ack *<= ()
            (flushWork <|> nack) ^=> loop
          | _ -> Alt.always () ^=> loop

          api.shutdownCh ^=> fun ack -> Hopac.timeOutMillis 200 ^=>. ack *<= ()
        ] :> Job<_>

      loop ()

    let mockTarget = TargetConf.createSimple server "mockTarget"

    let! registry =
        Config.create "svc" "host"
        |> Config.target mockTarget
        |> Config.build

    let ms100 =  Duration.FromMilliseconds 100L
    let! (finfo, sinfo) = Registry.shutdownWithTimeouts registry ms100 ms100
    let (FlushInfo (fack, ftimeout)) = finfo
    let (ShutdownInfo (sack, stimeout)) = sinfo
    Expect.equal fack [] "should have no flush ack target"
    Expect.contains ftimeout "mockTarget" "should have flush timeout target"
    
    Expect.equal sack [] "should have no shutdown ack target"
    Expect.contains stimeout "mockTarget" "should have shutdown timeout target"
    
    let! registry =
        Config.create "svc" "host"
        |> Config.target mockTarget
        |> Config.build

    let ms300 =  Duration.FromMilliseconds 300L
    let! (finfo, sinfo) = Registry.shutdownWithTimeouts registry ms300 ms300
    let (FlushInfo (fack, ftimeout)) = finfo
    let (ShutdownInfo (sack, stimeout)) = sinfo
    Expect.equal ftimeout [] "should have no flush timeout target"
    Expect.contains fack "mockTarget" "should have flush ack target"
    
    Expect.equal stimeout [] "should have no shutdown timeout target"
    Expect.contains sack "mockTarget" "should have shutdown ack target"
  } |> Job.toAsync)
]