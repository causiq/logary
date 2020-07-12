module Logary.Tests.Trace

open System
open System.Collections.Generic
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
    List.find (fun (KeyValue (k, _)) -> k = key)
    >> function (KeyValue (_, v)) -> v

let injectExtractProperty (inject: Setter<Map<string, string list>> -> SpanContext -> Map<string, string list> -> Map<string, string list>,
                           extract: Getter<Map<string, string list>> -> Map<string, string list> -> SpanAttr list * SpanContext option)
                          (ctx: SpanContext) =
  let cleanMap (m: IReadOnlyDictionary<string, string>) =
    m // cannot pass whitespace keys, since they are all string based
      |> Seq.filter (fun (KeyValue (k, _)) -> not (String.IsNullOrWhiteSpace k))
      // cannot pass null values, since they are all string based and there's no null available
      |> Seq.map (fun (KeyValue (k, v)) -> k, if isNull v then "" else v)
      |> Map.ofSeq

  // no zero contexts
  if ctx.isZero then () else
  // no null keys
  let newCtx =
    ctx.withState(TraceState.empty (* partial *)).withContext(cleanMap ctx.traceContext)
    // fix generated flags: Debug (implies) -> Sampled
    |> fun newCtx -> if ctx.isDebug then newCtx.withFlags (newCtx.flags ||| SpanFlags.Sampled) else newCtx

  let injected =
    Map.empty
      |> inject Inject.mapWithList newCtx

  let _, resCtx =
    injected
      |> extract Extract.mapWithList

  resCtx
    |> Expect.isSome (sprintf "`resCtx` has a value after `extract`; input map %A" injected)

  resCtx
    |> Option.get
    |> fun x -> x.traceContext
    |> Expect.sequenceEqual "ContextAttr extraction matches `ctx.traceContext` input" newCtx.traceContext

  resCtx
    |> Expect.isSome "Has resCtx"

  resCtx
    |> Option.get
    |> Expect.equal "resCtx = input ctx" newCtx


let injectExtractPropertyW3C (inject, extract) (ctx: SpanContext) =
  let nextCtx = SpanContext(ctx.traceId, ctx.spanId, None, ctx.flags, traceState=ctx.traceState)
  injectExtractProperty (inject, extract) nextCtx


[<Tests>]
let tests =
  let logger = Log.create "Logary.Tests.Trace"
  testList "trace" [
    testList "span (builder)" [
      testCase "build, start, finish span" <| fun () ->
        let spanLogger =
          SpanBuilder("new_span")
            .setAttribute("test", true)
            .debug()
            .start()

        spanLogger.started
          |> Expect.notEqual "Should be non-zero" 0L

        (spanLogger.elapsed, Duration.Zero)
          |> Expect.isGreaterThan "Should have a >= 0 ms elapsed Duration value"

        spanLogger.finished
          |> Expect.isNone "Should not have finished yet"

        let res = spanLogger.finish()

        spanLogger.finished
          |> Expect.isSome "Should have finished now"

        (res.finished, 1L)
          |> Expect.isGreaterThan "Should have finished; finished > 1L"
    ]

    testList "sampling" [
      let sampleSpanBuilder: SpanBuilder = SpanBuilder("sampling_span").setAttribute("test", true).debug()
      let sampleSpan: SpanLogger = sampleSpanBuilder.startWith logger

      yield testList "const" [
        testCase "always sample param" <| fun () ->
          let sampler = new ConstSampler(true) :> Sampler
          let result = sampler.shouldSample(sampleSpan)

          let attrs =
            result
              |> Expect.isOkX "Should have sampled"

          attrs
            |> List.findByKey Constants.SamplerParam
            |> Expect.equal "Should eq 'true'" (Value.Bool true)

          attrs
            |> List.findByKey Constants.SamplerType
            |> Expect.equal "Should eq 'const'" (Value.Str "const")
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
            |> Expect.equal "Equals 0.5" (Value.Float 0.5)

          attrs
            |> List.findByKey Constants.SamplerType
            |> Expect.equal "Equals 0.5" (Value.Str "probabilistic")
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

            context.traceState.isZero
              |> Expect.isTrue "No TraceState in the B3 format"

            match context.traceContext.TryGetValue "userId" with
            | false, _ -> failtestf "Failed to find 'userId' context val"
            | true, uid ->
              uid
                |> Expect.equal "TraceState/baggage a 'userId' key with a 'haf' value"
                                "haf"

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


        yield testPropertyWithConfig fsc "roundtrip multi" (injectExtractProperty (B3.injectMulti, B3.extractMulti))
        yield testPropertyWithConfig fsc "roundtrip single" (injectExtractProperty (B3.injectSingle, B3.extractSingle))
        yield testPropertyWithConfig fsc "roundtrip combined" (injectExtractProperty (B3.inject, B3.extract))
      ]

      testList "W3C" [
        yield testList "traceparent" [
          yield! [
            "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
            "00-0af7651916cd43dd8448eb211c80319c-00f067aa0ba902b7-01"
            "00-0af7651916cd43dd8448eb211c80319c-00f067aa0ba902b7-00"
          ] |> List.map (fun example ->
            testCase (sprintf "traceparent='%s'" example) <| fun _ ->
              let m = Map [ "traceparent", example ]
              let _, ctx = W3C.extract Extract.mapWithSingle m
              ctx
                |> Expect.isSome "Returns a context"

              ctx.Value.traceId
                |> Expect.equal "Has TraceId" (TraceId.ofString "0af7651916cd43dd8448eb211c80319c")

              ctx.Value.spanId.isZero
                |> Expect.isFalse "SpanId is non-zero"

              if example.EndsWith "01" then
                ctx.Value.flags &&& SpanFlags.Sampled = SpanFlags.Sampled
                  |> Expect.isTrue "Has the Sampled flag"
              elif example.EndsWith "00" then
                ctx.Value.flags &&& SpanFlags.Sampled = SpanFlags.Sampled
                  |> Expect.isFalse "Has the Sampled flag"
            )
        ]

        yield testList "tracestate" [
          yield! [
            "a@b", TraceStateKey ("a", Some "b")
            "b", TraceStateKey ("b", None)
            "a_-_*/@vnd_11", TraceStateKey ("a_-_*/", Some "vnd_11")
          ] |> List.map (fun (key, expected) ->
            testCase (sprintf "key='%s'" key) <| fun () ->
              match FParsec.CharParsers.run (W3C.TraceState.keyP) key with
              | FParsec.CharParsers.ParserResult.Success (res, _, _) ->
                res |> Expect.equal "Should parse to TraceStateKey value" expected
              | FParsec.CharParsers.ParserResult.Failure (res, _, _) ->
                failtestf "%A" res
            )

          yield testCase "many tracestate headers are concatenated" <| fun () ->
            let subject = [
              "traceparent", [ "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01" ]
              "tracestate", [ "congo=ucfJifl5GOE" ]
              "tracestate", [ "rojo=00f067aa0ba902b7" ]
            ]
            let _, ctx = W3C.extract Extract.listWithList subject
            ctx.Value.traceState
              |> Expect.equal "Has correct trace state value" (TraceState.ofList [
                TraceStateKey ("congo", None), "ucfJifl5GOE"
                TraceStateKey ("rojo", None), "00f067aa0ba902b7"
              ])

          yield! [
            [ "congo=t61rcWkgMzE" ], [
              TraceStateKey ("congo", None), "t61rcWkgMzE"
            ]

            [ "congo=ucfJifl5GOE,rojo=00f067aa0ba902b7" ], [
              TraceStateKey ("congo", None), "ucfJifl5GOE"
              TraceStateKey ("rojo", None), "00f067aa0ba902b7"
            ]

            [ "congo=ucfJifl5GOE"
              "rojo=00f067aa0ba902b7" ], [
              TraceStateKey ("congo", None), "ucfJifl5GOE"
              TraceStateKey ("rojo", None), "00f067aa0ba902b7"
            ]

            [ "rojo=00f067aa0ba902b7"
              "congo=ucfJifl5GOE" ], [
              TraceStateKey ("rojo", None), "00f067aa0ba902b7"
              TraceStateKey ("congo", None), "ucfJifl5GOE"
            ]

            [ "congo=ucfJifl5GOE,rojo@vnd1=00f067aa0ba902b7" ], [
              TraceStateKey ("congo", None), "ucfJifl5GOE"
              TraceStateKey ("rojo", Some "vnd1"), "00f067aa0ba902b7"
            ]
          ] |> List.map (fun (examples, values) ->
            testCase (sprintf "tracestate='%s' %i keys, %i headers" (String.concat "," examples) values.Length examples.Length) <| fun _ ->
              // most realistic variant, a list headers, each with a list of values:
              let headers = [
                yield "traceparent", [ "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01" ]
                for example in examples do
                  yield "tracestate", [ example ]
              ]

              let _, ctx = W3C.extract Extract.listWithList headers
              let ctx = Option.get ctx

              ctx.traceId
                |> Expect.equal "Has TraceId" (TraceId.ofString "0af7651916cd43dd8448eb211c80319c")

              ctx.spanId
                |> Expect.equal "Has SpanId" (SpanId.ofString "b7ad6b7169203331")

              int (ctx.flags &&& SpanFlags.Sampled) > 0
                |> Expect.isTrue "Has the Sampled flag"

              ctx.traceState.isZero
                |> Expect.isFalse "Has non-zero traceState value"

              ctx.traceState.value
                |> Expect.sequenceEqual "Has the ordered sequence of expected values in traceState" values
            )
        ]

        yield testList "correlation-context" [
        ]

        yield testPropertyWithConfig fsc "roundtrip combined" (injectExtractPropertyW3C (W3C.inject, W3C.extract))
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
            |> Expect.contains "Has a jaeger-debug-id value" (KeyValuePair<_,_>("jaeger-debug-id", Value.Str "37337"))

          contextO
            |> Expect.isSome "Extracts values successfully"

          let context = Option.get contextO

          match context.traceContext.TryGetValue "userId" with
          | false, _ -> failtestf "Failed to find 'userId' context val"
          | true, uid ->
            uid
              |> Expect.equal "TraceState/baggage a 'userId' key with a 'haf' value"
                              "haf"
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

        yield testPropertyWithConfig fsc "roundtrip inject and extract" (injectExtractProperty (Jaeger.inject, Jaeger.extract))
      ]
    ]
  ]
  |> testLabel "logary"