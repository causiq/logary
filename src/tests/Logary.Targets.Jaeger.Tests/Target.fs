namespace Logary.Targets.Jaeger.Tests

open Logary.Targets.Jaeger
open Logary.Targets
open Logary.Tests
open Hopac
open Expecto
open Expecto.Flip
open Logary
open Logary.Internals
open Logary.Trace
open Logary.Message
open NodaTime

module Target =
  open Jaeger.Thrift

  let shutdown t =
    job {
      let! ack = Target.shutdown t
      do! ack
    }

  let asLogger (target: Target.T) =
    { new Logger with
        member x.level = Verbose
        member x.name = PointName.parse "jaeger.target.test"
        member x.logWithAck (waitForBuffers, level) messageFactory =
          messageFactory level |> setName x.name |> Target.tryLog target
    }

  let testCaseTarget factory name fn =
    testCaseJob name (job {
      let batches = new ResizeArray<Jaeger.Thrift.Batch>()
      let! ri = emptyRuntime

      let client =
        { new Client with
            member x.send batch = batches.Add batch; Alt.unit ()
            member x.Dispose() = () }
      let jaegerConf = factory { Jaeger.empty with client = Some client }

      let targetConf =
        TargetConf.createSimple (Jaeger.Impl.loop jaegerConf) "jaeger"
        |> TargetConf.middleware Middleware.ambientSpanId

      let! target = Target.create ri targetConf
      let logger = asLogger target
      let flush = Target.flush target

      do! Job.tryFinallyJob (fn batches logger flush (ri :> RuntimeInfo)) (shutdown target)
    })

  let hasStringTag k v (tags: #seq<Jaeger.Thrift.Tag>) =
    tags
      |> Seq.exists (fun t -> t.Key = k && t.VType = TagType.STRING && t.VStr = v)
      |> Expect.isTrue (sprintf "Should have tag %s => %s, tags: %A" k v tags)
    tags

  let hasBoolTag k v (tags: #seq<Jaeger.Thrift.Tag>) =
    tags
      |> Seq.exists (fun t -> t.Key = k && t.VType = TagType.BOOL && t.VBool = v)
      |> Expect.isTrue (sprintf "Should have tag %s => %b, tags: %A" k v tags)
    tags

  [<Tests>]
  let tests =
    testList "jaeger" [
      testCaseTarget id "send log to jaeger span format" (fun batches logger flush ri ->
        let connStr = "Host=;Database=;Username=;Password=;"
        let sampleQuery = "select count(*) from orders"
        let expectedMinNs, expectedMaxNs = ri.getTimestamp(), ri.getTimestamp() + Duration.FromSeconds(8L).ToInt64Nanoseconds()

        job {
          let root = logger.startSpan "GET /orders"

          do! eventX "before some action: {userId}" >> setField "userId" 123 |> root.infoWithBP
          do! eventX "after some action: {orderId}" >> setField "orderId" 321 >> setSpanId root.context.spanId |> logger.infoWithBP

          let child = logger.startSpan("sql.Query", root, tag "pgsql" >> setContext "connStr" connStr)
          do! eventX "query={query}" >> setField "query" sampleQuery |> logger.infoWithBP

          child.finish()
          root.finish() // TODO: finish only does Hopac.queue, so it may not be enough with the flow below
          do! flush

          batches.Count
            |> Expect.equal "Should have one Batch logged" 1
          batches.[0].Spans.Count
            |> Expect.equal "This batch has two Spans" 2

          let batch = batches.[0]
          batch.Process.ServiceName
            |> Expect.equal "should equal the default runtime service name" "logary-tests"

          let defaultTag = batch.Process.Tags.[0]
          defaultTag.Key
            |> Expect.equal "Should have default tag" "hostname"
          defaultTag.VType
            |> Expect.equal "Should have default tag" TagType.STRING
          defaultTag.VStr
            |> Expect.equal  "Should have default tag" "dev-machine"

          let childSpan, rootSpan = batch.Spans.[0], batch.Spans.[1]

          childSpan.OperationName
            |> Expect.equal "Innermost Span should eq query-SpanLogger's label" child.label
          enum<SpanFlags>(childSpan.Flags)
            |> Expect.equal "Should equal sampled flag" SpanFlags.Sampled
          childSpan.ParentSpanId
            |> Expect.equal "Should equal root span's spanId" rootSpan.SpanId

          let loggerName = "jaeger.target.test"
          childSpan.Tags
            |> hasStringTag "level" "info"
            |> hasStringTag "component" loggerName
            |> hasStringTag "event" "sql.Query"
            |> hasBoolTag "pgsql" true
            |> hasStringTag "connStr" (sprintf "\"%s\"" connStr)
            |> ignore

          let childLog = childSpan.Logs.[0]
          Expect.isGreaterThan "timestamp unit is µs (micro)" (childLog.Timestamp, expectedMinNs / 1000L)
          Expect.isLessThan "timestamp unit is µs (micro)" (childLog.Timestamp, expectedMaxNs / 1000L)

          childLog.Fields
            |> hasStringTag "level" "info"
            |> hasStringTag "component" loggerName
            |> hasStringTag "event" (sprintf "query=\"%s\"" sampleQuery)
            |> ignore

          rootSpan.OperationName
            |> Expect.equal "Has root span name" root.label

          Expect.isGreaterThan "Is sampled" (int rootSpan.Flags, 0)
          rootSpan.ParentSpanId
            |> Expect.equal "Root has no parent" 0L

          rootSpan.Logs.Count
            |> Expect.equal "Should have two logs in root span" 2

          rootSpan.Logs.[0].Fields
            |> hasStringTag "event" "before some action: 123"
            |> ignore

          rootSpan.Logs.[1].Fields
            |> hasStringTag "event" "after some action: 321"
            |> ignore
        })
    ]
