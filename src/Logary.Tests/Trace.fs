module Logary.Tests.Trace

open System
open Expecto
open Expecto.Flip
open Logary
open Logary.Trace
open Logary.Trace.Sampling
open Logary.Trace.Propagation
open NodaTime

module List =
  let findByKey (key: 'a) =
    List.find (fun (k, _) -> k = key)
    >> function (_, v) -> v

[<Tests>]
let tests =
  let logger = Log.create "Logary.Tests.Trace"
  [
    testList "span (builder)" [
      testCase "build, start, finish span" <| fun () ->
        let spanLogger =
          SpanBuilder(logger, "new_span")
            .setAttribute("test", true)
            .debug()
            .start()

        spanLogger.started
          |> Expect.notEqual "Should be non-zero" 0L

        (spanLogger.elapsed, Duration.Zero)
          |> Expect.isGreaterThan "Should have a >= 0 ms elapsed Duration value"

        spanLogger.finished
          |> Expect.isNone "Should not have finished yet"

        spanLogger.finish()
    ]

    testList "sampling" [
      let sampleSpanBuilder: SpanBuilder = SpanBuilder(logger, "sampling_span").setAttribute("test", true).debug()
      let sampleSpan: SpanLogger = sampleSpanBuilder.start()

      yield testList "const" [
        testCase "always sample param" <| fun () ->
          let sampler = new ConstSampler(true) :> Sampler
          let result = sampler.shouldSample(sampleSpan)

          let attrs =
            result
              |> Expect.isOkX "Should have sampled"

          attrs
            |> List.findByKey Constants.SamplerParam
            |> Expect.equal "Should eq 'true'" (SpanAttrValue.B true)

          attrs
            |> List.findByKey Constants.SamplerType
            |> Expect.equal "Should eq 'const'" (SpanAttrValue.S "const")
      ]

      yield testList "probabilistic" [
        let traceIdHigh = 0x3fffffffffffffffL
        yield testCase "TraceId.high" <| fun () ->
          traceIdHigh + 1L
            |> Expect.equal "int64 (float Int64.MaxValue * 0.5)" (int64 (float Int64.MaxValue * 0.5))

        let traceIdLow = -0x4000000000000000L
        yield testCase "TraceId.low" <| fun () ->
          traceIdLow
            |> Expect.equal "int64 (float Int64.MinValue * 0.5)" (int64 (float Int64.MinValue * 0.5))


        let sampler = new ProbabilisticSampler(samplingRate=0.5) :> Sampler
        yield testCase "not sampling" <| fun () ->
          let nonSampledSpan = sampleSpanBuilder.withParent(TraceId.create(traceIdHigh + 1L), SpanId.create()).start()
          sampler.shouldSample nonSampledSpan
            |> Expect.isError "Should not sample"

        let sampledSpan = sampleSpanBuilder.withParent(TraceId.create(traceIdHigh), SpanId.create()).start()
        yield testCase "sampling" <| fun () ->
          sampler.shouldSample sampledSpan |> Expect.isOk "Should sample"

        yield testCase "attrs" <| fun () ->
          let attrs =
            sampler.shouldSample sampledSpan
              |> Expect.isOkX "Should sample"
          attrs
            |> List.findByKey Constants.SamplerParam
            |> Expect.equal "Equals 0.5" (SpanAttrValue.V (Float 0.5))

          attrs
            |> List.findByKey Constants.SamplerType
            |> Expect.equal "Equals 0.5" (SpanAttrValue.S "probabilistic")
      ]
    ]

    testList "propagation" [
      testList "Jaeger" [
        yield! [ // example uber-trace-id header values
          "abcdef1234567890abcdef1234567890:1234567890fedcba:0:3"
          "abcdef1234567890abcdef1234567890:1234567890fedcba:0:2"
          "abcdef1234567890abcdef1234567890:1234567890fedcba:0:1"
          "abcdef1234567890abcdef1234567890:1234567890fedcba:0:0"
          "abcdef1234567890abcdef1234567890:1234567890fedcba:12345fedcba67890:3"
          "abcdef1234567890abcdef1234567890:1234567890fedcba:12345fedcba67890:2"
          "abcdef1234567890abcdef1234567890:1234567890fedcba:12345fedcba67890:1"
          "abcdef1234567890abcdef1234567890:1234567890fedcba:12345fedcba67890:0"
          "abcdef1234567890:1234567890fedcba:0:0"
          "ABCDEF1234567890ABCDEF1234567890:1234567890FEDCBA:0:3"
          "ABCDEF1234567890ABCDEF1234567890:1234567890FEDCBA:12345FEDCBA67890:0"
          "ABCDEF1234567890:1234567890FEDCBA:0:0"
        ]
          |> List.map (fun example -> testCase (sprintf "extract Regex trace-id from '%s'" example) <| fun () ->
            match example with
            | Jaeger.Regex Jaeger.ExtractRegex [ traceId; spanId; _; _ ] ->
              TraceId.ofString traceId |> Expect.notEqual "Non-zero TraceId" TraceId.Zero
              SpanId.ofString spanId |> Expect.notEqual "Non-zero SpanId" SpanId.Zero
            | _ ->
              failtestf "Couldn't extract from '%s'" example
            )

        yield testCase "extract from map" <| fun () ->
          let m = Map [
            "uber-trace-id", "abcdef1234567890abcdef1234567890:1234567890fedcba:0:3"
            "uberctx-userId", "haf"
            "jaeger-debug-id", "37337"
          ]

          let spanAttrs, contextO = Jaeger.extract Extract.mapWithSingle m

          spanAttrs
            |> Expect.contains "Has a jaeger-debug-id value" ("jaeger-debug-id", SpanAttrValue.S "37337")

          contextO
            |> Expect.isSome "Has a previous context"

          let context = Option.get contextO

          context.traceState
            |> Map.tryFind "userId"
            |> Expect.equal "TraceState/baggage a 'userId' key with a 'haf' value" (Some "haf")

          context.traceId
            |> Expect.equal "Has correct TraceId" (TraceId.ofString "abcdef1234567890abcdef1234567890")

          context.parentSpanId
            |> Expect.isNone "Has no parentSpanId in SpanContext, since it's 0"

          context.spanId
            |> Expect.equal "Has a SpanId" (SpanId.ofString "1234567890fedcba")

          context.isDebug
            |> Expect.isTrue "Since Debug|||Sampled = 3 = flags"

          context.isSampled
            |> Expect.isTrue "Since Debug|||Sampled = 3 = flags"

          context.isRecorded
            |> Expect.isTrue "Since Debug|||Sampled = 3 = flags"

        let injectExtractProperty (ctx: SpanContext) =
          // no zero contexts
          if ctx.isZero then () else
          // no null keys
          if ctx.traceState |> Map.containsKey null then () else
          // null value -> ""
          let ctx =
            ctx.traceState
              |> Map.map (fun _ v -> if isNull v then "" else v)
              |> ctx.withState
          // otherwise:

          let _, resCtx =
            Map.empty
              |> Jaeger.inject Inject.mapWithList ctx
              |> Jaeger.extract Extract.mapWithList

          resCtx
            |> Option.get
            |> fun x -> x.traceState
            |> Expect.sequenceEqual "ContextAttr extraction matches `ctx.traceState` input" ctx.traceState

          resCtx
            |> Expect.isSome "Has resCtx"

          resCtx
            |> Option.get
            |> Expect.equal "resCtx = input ctx" ctx

        yield testProperty "roundtrip inject and extract" injectExtractProperty
      ]
    ]
  ]