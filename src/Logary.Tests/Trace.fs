module Logary.Tests.Trace

open System
open Expecto
open Expecto.Flip
open Logary
open Logary.Internals.Regex
open Logary.Trace
open Logary.Trace.Sampling
open Logary.Trace.Propagation
open NodaTime

module List =
  let findByKey (key: 'a) =
    List.find (fun (k, _) -> k = key)
    >> function (_, v) -> v

let injectExtractProperty (inject: Setter<Map<string, string list>> -> SpanContext -> Map<string, string list> -> Map<string, string list>,
                           extract: Getter<Map<string, string list>> -> Map<string, string list> -> SpanAttr list * SpanContext option)
                          (ctx: SpanContext) =
  // no zero contexts
  if ctx.isZero then () else
  // no null keys
  if ctx.traceState |> Map.containsKey null then () else
  // null value -> ""
  let newCtx =
    ctx.traceState
      |> Map.toSeq
      // cannot pass whitespace keys, since they are all string based
      |> Seq.filter (fun (k, _) -> not (String.IsNullOrWhiteSpace k))
      // cannot pass null values, since they are all string based and there's no null available
      |> Seq.map (fun (k, v) -> k, if isNull v then "" else v)
      |> Map.ofSeq
      |> ctx.withState
      // fix generated flags: Debug (implies) -> Sampled
      |> fun newCtx -> if ctx.isDebug then newCtx.withFlags (newCtx.flags ||| SpanFlags.Sampled) else newCtx
  // otherwise:

  let injected =
    Map.empty
      |> inject Inject.mapWithList newCtx

  let _, resCtx =
    injected
      |> extract Extract.mapWithList

  resCtx
    |> Option.get
    |> fun x -> x.traceState
    |> Expect.sequenceEqual "ContextAttr extraction matches `ctx.traceState` input" newCtx.traceState

  resCtx
    |> Expect.isSome "Has resCtx"

  resCtx
    |> Option.get
    |> Expect.equal "resCtx = input ctx" newCtx


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
      testList "B3" [
        yield testList "single" [
          yield! [
            "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1-05e3ac9a4f6e3b90"
            "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1"
            "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1"
            "80F198EE56343BA864FE8B2A57D3EFF7-E457B5A2E4D86BD1-1-05E3AC9A4F6E3B90"
            "80F198EE56343BA864FE8B2A57D3EFF7-E457B5A2E4D86BD1-1"
            "80F198EE56343BA864FE8B2A57D3EFF7-E457B5A2E4D86BD1"
            ]
            |> List.map (fun example -> testCase (sprintf "Regex <pattern> '%s' => SpanContext" example) <| fun () ->
              match example with
              | Regex B3.ExtractRegex [ traceId; spanId; sampled; parentSpanId ] ->
                traceId |> Expect.notEqual "Has a trace id if matched" ""
                spanId |> Expect.notEqual "Has a span id if matched" ""
                sampled |> Expect.notEqual "Has a sampled value if matched" ""
                parentSpanId |> Expect.notEqual "Has a parent span if matched" ""

              | Regex B3.ExtractRegex [ traceId; spanId; sampled ] ->
                traceId |> Expect.notEqual "Has a trace id if matched" ""
                spanId |> Expect.notEqual "Has a span id if matched" ""
                sampled |> Expect.notEqual "Has a sampled value if matched" ""

              | Regex B3.ExtractRegex [ traceId; spanId ] ->
                traceId |> Expect.notEqual "Has a trace id if matched" ""
                spanId |> Expect.notEqual "Has a span id if matched" ""

              | _ ->
                failtestf "Failed to extract B3 headers from '%s'" example
              )
        ]

        yield! [
          "single",
            Map [
              "b3", "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-d-05e3ac9a4f6e3b90"
              "uberctx-userId", "haf"
            ]
          "multi",
            Map [
              "X-B3-TraceId", "80f198ee56343ba864fe8b2a57d3eff7"
              "X-B3-ParentSpanId", "05e3ac9a4f6e3b90"
              "X-B3-SpanId", "e457b5a2e4d86bd1"
              "uberctx-userId", "haf"
              "x-b3-flags", "1" // same as 'd' (otherwise x-b3-sampled: 1)
            ]
        ] |> List.map (fun (variant, m) ->
          testCase (sprintf "extract from map %s" variant) <| fun () ->
            let spanAttrs, contextO = B3.extract Extract.mapWithSingle m

            spanAttrs
              |> Expect.isEmpty "No attributes in Zipkin"

            contextO
              |> Expect.isSome "Extracts values successfully"

            let context = Option.get contextO

            context.traceState
              |> Map.tryFind "userId"
              |> Expect.equal "TraceState/baggage a 'userId' key with a 'haf' value" (Some "haf")

            context.traceId
              |> Expect.equal "Has correct TraceId" (TraceId.ofString "80f198ee56343ba864fe8b2a57d3eff7")

            context.parentSpanId
              |> Expect.equal "Has parentSpanId in SpanContext" (Some (SpanId.ofString "05e3ac9a4f6e3b90"))

            context.spanId
              |> Expect.equal "Has a SpanId" (SpanId.ofString "e457b5a2e4d86bd1")

            context.isDebug
              |> Expect.isTrue "is debug"

            context.isSampled
              |> Expect.isTrue "is sampled"

            context.isRecorded
              |> Expect.isTrue "is recorded"
          )


        let createSubject flag =
          let input = Map [ "b3", (sprintf "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-%s-05e3ac9a4f6e3b90" flag) ]
          let _, contextO = B3.extract Extract.mapWithSingle input
          contextO |> Expect.isSome "Extracts values successfully"
          Option.get contextO

        yield testCase "extract debug" <| fun () ->
          let subject = createSubject "d"
          subject.isDebug |> Expect.isTrue "is debug"
          subject.isSampled |> Expect.isTrue "is therefore sampled"
          subject.flags |> Expect.equal "Eq Debug XOR Sampled" (SpanFlags.Debug ^^^ SpanFlags.Sampled)

        let createHeaders flag =
          let origin = createSubject flag
          let headers = Map.empty |> B3.inject Inject.mapWithList origin
          headers.["b3"]

        yield testCase "inject debug" <| fun () ->
          let subject = createHeaders "d"
          subject
            |> List.head
            |> Expect.stringContains "Has -d- inside it" "-d-"


        yield testCase "extract sampled" <| fun () ->
          let subject = createSubject "1"
          subject.isDebug |> Expect.isFalse "is not debug"
          subject.isSampled |> Expect.isTrue "but is sampled"
          subject.flags |> Expect.equal "Eq Sampled" SpanFlags.Sampled

        yield testCase "inject sampled" <| fun () ->
          let subject = createHeaders "1"
          subject
            |> List.head
            |> Expect.stringContains "Has -1- inside it" "-1-"


        yield testCase "extract not sampled" <| fun () ->
          let subject = createSubject "0"
          subject.isDebug |> Expect.isFalse "is not debug"
          subject.isSampled |> Expect.isFalse "is not sampled"
          subject.flags |> Expect.equal "Eq None" SpanFlags.None

        yield testCase "inject NOT sampled" <| fun () ->
          let subject = createHeaders "0"
          subject
            |> List.head
            |> Expect.stringContains "Has -0- inside it" "-0-"


        yield testProperty "roundtrip multi" (injectExtractProperty (B3.injectMulti, B3.extractMulti))
        yield testProperty "roundtrip single" (injectExtractProperty (B3.injectSingle, B3.extractSingle))
        yield testProperty "roundtrip combined" (injectExtractProperty (B3.inject, B3.extract))
      ]

      testList "W3C" [
        // TODO
      ]

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
        ] |> List.map (fun example -> testCase (sprintf "Regex <pattern> '%s' => SpanContext" example) <| fun () ->
            match example with
            | Regex Jaeger.ExtractRegex [ traceId; spanId; _; _ ] ->
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
            |> Expect.isSome "Extracts values successfully"

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

        yield testProperty "roundtrip inject and extract" (injectExtractProperty (Jaeger.inject, Jaeger.extract))
      ]
    ]
  ]