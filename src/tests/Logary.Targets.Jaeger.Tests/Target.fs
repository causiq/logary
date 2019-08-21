namespace Logary.Targets.Jaeger.Tests

open Logary.Targets.Jaeger
open Logary.Targets
open Logary.Tests
open Hopac
open Logary
open Expecto
open NodaTime
open Logary.Message

module Target =
  open Jaeger.Thrift

  let shutdown t =
    job {
      let! ack = Target.shutdown t
      do! ack
    }

  let asLogger (target: Target.T) =
    { new Logger with
        member x.name = PointName.parse "jaeger.target.test"

        member x.logWithAck (waitForBuffers, level) messageFactory =
          messageFactory level |> setName x.name |> Target.tryLog target

        member x.level = Verbose
    }

  let testCaseTarget factory name fn =
    testCaseJob name (job {
      let state = new ResizeArray<Jaeger.Thrift.Batch>()
      let jaegerConf = factory Jaeger.empty
      let! ri = emptyRuntime

      let spanSender =
        {
          new SpanSender with
            member x.sendBatch batch =
              Job.lift (fun batch -> state.Add batch) batch
            member x.Dispose() = ()
        }

      let targetConf =
        TargetConf.createSimple (Jaeger.Impl.loop jaegerConf spanSender Jaeger.defaultSpanSizeCounter) "jaeger"
        |> TargetConf.middleware Middleware.ambientSpanId

      let! target = Target.create ri targetConf
      let logger = asLogger target
      let flush = Target.flush target

      do! Job.tryFinallyJob (fn state logger flush) (shutdown target)
    })

  let hasTag k v (tags: #seq<Jaeger.Thrift.Tag>) =
    let hasTag = tags |> Seq.exists (fun t -> t.Key = k && t.VType = TagType.STRING && t.VStr = v)
    Expect.isTrue hasTag (sprintf "Should have tag %s => %s, tags: %A" k v tags)

  [<Tests>]
  let tests =
    testList "jaeger" [
      testCaseTarget id "send log to jaeger span format" (fun state logger flush ->
        job {
          let root = logger.startSpan "root span"

          do! root.infoWithAck (eventX "before some action: {userId}" >> setField "userId" 123)

          // now we do some action (pretending to!)

          // use explicitly setSpanId style, since we use Hopac `do! timeOutMillis 100` before (the ambientSpanId middleware will not be guaranteed)
          do! eventX "after some action: {orderId}" >> setField "orderId" 321 >> setSpanId root.context.spanId |> logger.infoWithBP

          let conStr = "Host=;Database=;Username=;Password=;"

          // use explicitly setParentSpanInfo style, since we use Hopac `do! timeOutMillis 100` before (the ambientSpanId middleware will not be guaranteed)
          let childSpan =
            logger.buildSpan "child span"
              |> Span.withTransform (tag "DB Query" >> tag "Postgresql" >> setContext "conn str" conStr)
              |> Span.withParentSpan root
              |> Span.start

          let sql = "select count(*) from xxx"
          do! eventX "query : {sql}" >> setField "sql" sql >> setTimestamp (Instant.FromUnixTimeSeconds 1L) |> logger.infoWithBP

          childSpan.Dispose()
          root.Dispose()

          Expect.isEmpty state "since batch span size is 1mb as default"

          do! flush
          Expect.equal state.Count 1 "should get 1 batch"

          let batch = state.[0]
          let proc = batch.Process
          let defaultTag = proc.Tags.[0]
          Expect.equal proc.ServiceName "logary-tests" "should equal the default runtime service name"
          Expect.equal defaultTag.Key "host" "Should have default tag"
          Expect.equal defaultTag.VType TagType.STRING "Should have default tag"
          Expect.equal defaultTag.VStr "dev-machine" "Should have default tag"

          let spans = batch.Spans
          Expect.equal spans.Count 2 "Should have two spans"
          let childSpan = spans.[0]
          let rootSpan = spans.[1]

          Expect.equal childSpan.OperationName "child span" "Should equal"
          Expect.equal childSpan.Flags (int JaegerSpanFlags.Sampled) "Should have sampled flag"
          Expect.equal childSpan.ParentSpanId rootSpan.SpanId "Should equal root span's spanId"
          let loggerName = "jaeger.target.test"
          hasTag "logger-level" "info" childSpan.Tags
          hasTag "logger-name" loggerName childSpan.Tags
          hasTag "logger-msg" "child span" childSpan.Tags
          hasTag "tags" "DB Query,Postgresql" childSpan.Tags
          hasTag "conn str" (sprintf "\"%s\"" conStr) childSpan.Tags
          let childLog = childSpan.Logs.[0]
          Expect.equal childLog.Timestamp 1000000L "timestamp unit is Microseconds"
          hasTag "logger-level" "info" childLog.Fields
          hasTag "logger-name" loggerName childLog.Fields
          hasTag "logger-msg" "query : \"select count(*) from xxx\"" childLog.Fields

          Expect.equal rootSpan.OperationName "root span" "Should equal"
          Expect.equal rootSpan.Flags (int JaegerSpanFlags.Sampled) "Should have sampled flag"
          Expect.equal rootSpan.ParentSpanId 0L "Should have no parentSpanId"
          let rootLogs = rootSpan.Logs
          Expect.equal rootLogs.Count 2 "Should have two logs in root span"
          hasTag "logger-msg" "after some action: 321" rootLogs.[0].Fields
          hasTag "logger-msg" "before some action: 123" rootLogs.[1].Fields


        })

      testCaseTarget (fun conf -> { conf with autoFlushInterval = Duration.FromSeconds 2.; })
        "AutoFlush Span"
        (fun state logger _ ->
          job {
            let span = logger.buildSpan "some span" |> Span.start
            do! eventX "before some action: {userId}" >> setField "userId" 123 |> logger.infoWithBP
            span.Dispose()

            Expect.isEmpty state "since batch span size is 1mb as default"

            do! timeOutMillis 2500

            Expect.equal state.Count 1 "should get 1 batch, triggered by auto flush"
          })

      testCaseTarget (fun conf -> { conf with autoFlushInterval = Duration.FromSeconds 2.; retentionTime = Duration.FromSeconds 1. })
        "Auto GC Spans"
        (fun state logger flush ->
          job {

            let span = logger.buildSpan "some span" |> Span.start
            do! eventX "before some action: {userId}" >> setField "userId" 123 |> logger.infoWithBP

            do! flush

            Expect.isEmpty state "since there is not any finished span"

            do! timeOutMillis 2500

            Expect.isEmpty state "since there is not any finished span, and log message should be GC:ed"

            span.Dispose()
            do! flush
            Expect.equal state.Count 1 "should get 1 batch, triggered by flush"
            let spans = state.[0].Spans
            Expect.equal spans.Count 1 "should get 1 span"
            let logs = spans.[0].Logs
            Expect.isEmpty logs "should have no logs in span, since logs have been gc"
          })
    ]
