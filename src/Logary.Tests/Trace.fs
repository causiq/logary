module Logary.Tests.Trace

open System
open Expecto
open Expecto.Flip
open Logary
open Logary.Trace
open Logary.Trace.Sampling
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

        testList "probabilistic" [
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
    ]
  ]