module Logary.Tests.Metric

open System
open Expecto
open Expecto.Flip
open Logary
open Logary.Metric

/// This configuration has no explicit label names
let private basicConf = MetricConf.create("items.in.queue", "items in queue", U.Scalar)
/// This configuration has no explicit label names
let private gaugeConf = GaugeConf.create basicConf

#nowarn "25"

[<Tests>]
let tests =
  testList "metric" [
    testCase "registry.getOrCreate happy path" <| fun _ ->
      let registry = MetricRegistry()
      let gauge = registry.getOrCreate gaugeConf
      gauge.unlabelled.inc 5.

      let basicInfo, MetricInfo.Gauge gaugeInfo = gauge.unlabelled.explore()
      basicInfo.name
        |> Expect.equal "should have same name" gaugeConf.metricConf.name
      basicInfo.description
        |> Expect.equal "should have same description" gaugeConf.metricConf.description
      gaugeInfo.labels
        |> Expect.equal "should have empty labels" Map.empty
      gaugeInfo.value
        |> Expect.equal "should have same gauge value" 5.

    testCase "multiple getOrCreate calls" <| fun _ ->
      let registry = MetricRegistry()

      let gauge = registry.getOrCreate gaugeConf
      gauge.unlabelled.inc 15.

      let gauge = registry.getOrCreate gaugeConf
      gauge.unlabelled.dec 10.

      let _, MetricInfo.Gauge gaugeInfo = gauge.unlabelled.explore()
      gaugeInfo.value
        |> Expect.equal "should reuse the same one" 5.

      Expect.throws "should throws when register metric with same name but different basic conf " <| fun () ->
        GaugeConf.create {basicConf with labelNames = [| "with some label" |]} |> registry.getOrCreate |> ignore

    testCase "unlabelled" <| fun _ ->
      let registry = MetricRegistry()
      let gauge = registry.getOrCreateUnlabelled gaugeConf
      gauge.inc 10.

      let gauge = registry.getOrCreate gaugeConf
      gauge.unlabelled.inc 10.

      let _, MetricInfo.Gauge gaugeInfo = gauge.unlabelled.explore()
      gaugeInfo.value
        |> Expect.equal ".getOrCreateUnlabelled is just a shortcut for .getOrCreate(gaugeConf).unlabelled" 20.

    testList "erroneous usage" [
      testCase "gauge labels keys are missing their values" <| fun () ->
        Expect.throws "an exception" <| fun () ->
          let registry = MetricRegistry()
          let gauge = registry.getOrCreate gaugeConf
          gauge.withLabelValues([| "some queue name" |]).set 10.

      testCase "different label lengths — more names in with-call" <| fun () ->
        Expect.throws "an exception" <| fun () ->
          let registry = MetricRegistry()
          let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
          gauge.withLabelValues([| "some queue name"; "some other value" |]).set 10.

      testCase "different label lengths — empty with-call" <| fun () ->
        Expect.throws "should throw when label value's length not equal to the label name's length" <| fun () ->
          let registry = MetricRegistry()
          let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
          gauge.withLabelValues([||]).set 10.
    ]

    testCase "labelling by ordinal position" <| fun _ ->
      let registry = MetricRegistry()
      let gauge = { basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
      gauge.withLabelValues([| "some queue name" |]).set 10.
      gauge.unlabelled.set 20.

      let basicAndMetricInfos = registry.exportAll() |> Array.ofSeq
      basicAndMetricInfos.Length
        |> Expect.equal "should have only one metric" 1

      let _, metricInfos = basicAndMetricInfos.[0]

      let mutable detailCount = 0
      for info in metricInfos do
        match info with
        | MetricInfo.Gauge gaugeInfo ->
          match gaugeInfo with
          | gaugeInfo when gaugeInfo.labels = Map.empty ->
            gaugeInfo.value
              |> Expect.equal "the unlabelled metric should have value 20." 20.
          | _ ->
            gaugeInfo.value
              |> Expect.equal "the labelled metric, should have value 10." 10.

        | _ ->
          failtest "should not occur"

        detailCount <- detailCount + 1

      detailCount
        |> Expect.equal "should have two metric infos, one with a label name, one unlabelled" 2

    testCase "get metric info from metric registry" <| fun _ ->
      let registry = MetricRegistry()
      let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
      let metricInfos = registry.exportAll () |> Array.ofSeq
      let basicInfo, metricInfoDetails = metricInfos.[0]

      basicInfo.name
        |> Expect.equal "should have same name" gaugeConf.metricConf.name
      basicInfo.description
        |> Expect.equal "should have same description"  gaugeConf.metricConf.description
      metricInfoDetails
        |> Expect.isEmpty "should have empty metric detail since there's been no calls to gauge"

      gauge.withLabelValues([| "some queue name" |]).set 10.

      let _, metricInfoDetails = registry.exportAll () |> Seq.head
      metricInfoDetails
        |> Expect.sequenceEqual "should have one gauge detail"
            [ MetricInfo.Gauge { labels=Map ["queue name", "some queue name"]; value=10.; unit=U.Scalar } ]

    testCase "gauge" <| fun () ->
      let registry = MetricRegistry()
      let gauge = gaugeConf |> registry.getOrCreateUnlabelled
      gauge.set 200.
      gauge.inc 100.
      gauge.inc -50.
      gauge.dec 50.
      gauge.dec -50.
      let _, MetricInfo.Gauge gaugeInfo = gauge.explore ()
      gaugeInfo.value
        |> Expect.equal "gauge value should be 250" 250.

    testList "gauge with histogram" [
      let testBuckets = [| 5.; 10.; 50.; 100.; |]
      let registry = MetricRegistry()
      let gauge =
        gaugeConf
          |> GaugeConf.withHistogram testBuckets
          |> registry.getOrCreateUnlabelled

      gauge.set 200.
      gauge.dec 140. // 60
      gauge.dec 15.  // 45
      gauge.dec 5.   // 40
      gauge.dec 15.  // 25
      gauge.dec 10.  // 15
      gauge.dec 8.   // 7
      gauge.inc 18.  // 25

      let metricInfos = registry.exportAll () |> Array.ofSeq

      yield testCase "gauge detail" <| fun () ->
        let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = gaugeConf.metricConf.name)
        metricDetails
          |> Expect.sequenceEqual "should have one gauge detail"
              [ MetricInfo.Gauge { labels= Map.empty ; value = 25.; unit=U.Scalar } ]

      yield testCase "histogram detail" <| fun () ->
        let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = sprintf "%s_histogram" gaugeConf.metricConf.name)

        let bucketsInfo =
          [(5.,0.); (10.,1.); (50., 5.); (100., 1.); (Double.PositiveInfinity, 1.)]
            |> Map.ofSeq

        metricDetails
          |> Expect.sequenceEqual "should have histogram detail"
              [ MetricInfo.Histogram { labels=Map.empty; buckets=bucketsInfo; sum=float(200+60+45+40+25+15+7+25); unit=U.Scalar } ]
    ]


    let testBuckets = [| 5.; 10.; 50.; 100.; Double.PositiveInfinity |]

    let whenUsingGaugeWithHistogram act =
      let registry = MetricRegistry()
      let histogramConf =
        HistogramConf.create("some histogram", "some histogram", U.Scalar)
          |> HistogramConf.buckets testBuckets

      let gauge =
        gaugeConf
          |> GaugeConf.enableHistogram histogramConf
          |> registry.getOrCreateUnlabelled

      act gauge

      let allMetrics = registry.exportAll() |> Array.ofSeq

      let _, gaugeDetails =
        allMetrics |> Array.find (fun (basicInfo , _) -> basicInfo.name = gaugeConf.metricConf.name)

      let _, histDetails =
        allMetrics |> Array.find (fun (basicInfo , _) -> basicInfo.name = histogramConf.metricConf.name)

      gaugeDetails, histDetails

    testCase "unused gauge with histogram has empty result" <| fun () ->
      let gaugeDetails, histDetails = whenUsingGaugeWithHistogram ignore

      // gauge metric
      gaugeDetails
        |> Expect.sequenceEqual "should have one empty gauge details"
            [ MetricInfo.Gauge { labels=Map.empty; value=0.; unit=U.Scalar } ]

      // histogram metric
      let expectedHistogram = testBuckets |> Array.map (fun i -> i, 0.) |> Map.ofSeq

      histDetails
        |> Expect.sequenceEqual "should have empty histogram detail"
            [ MetricInfo.Histogram { labels=Map.empty; buckets=expectedHistogram; sum=0.; unit=U.Scalar } ]

    testCase "gauge with histogram has non-empty result" <| fun () ->
      // TODO: exact expectations on how Gauge maps to Histogram; verify this test

      let gaugeDetails, histDetails = whenUsingGaugeWithHistogram <| fun g ->
        g.inc 10.
        g.inc 5.
        g.inc 15.

      // gauge metric
      gaugeDetails
        |> Expect.sequenceEqual "should have one empty gauge details"
            [ MetricInfo.Gauge { labels=Map.empty; value=30.; unit=U.Scalar } ]

      // histogram metric
      let expectedHistogram =
        testBuckets
          |> Array.mapi (fun i bucket ->
            if i = 0 then bucket, 5.
            elif i = 1 then bucket, 10.
            elif i = 2 then bucket, 15.
            else bucket, 0.)
          |> Map.ofSeq

      histDetails
        |> Expect.sequenceEqual "should have empty histogram detail"
            [ MetricInfo.Histogram { labels=Map.empty; buckets=expectedHistogram; sum=10.; unit=U.Scalar } ]


    testCase "histogram with label" <| fun () ->
      let testBuckets = [| 5.; 10.; 50.; 100.; |]
      let registry = MetricRegistry()

      let histogramConf =
        MetricConf.create("some histogram", "some histogram desc", U.Scalar)
        |> MetricConf.labelNames [| "queue name" |]
        |> HistogramConf.create
        |> HistogramConf.buckets testBuckets

      let histogram = histogramConf |> registry.getOrCreate

      (histogram.withLabelValues [| "queue a" |]).observe 15.
      (histogram.withLabelValues [| "queue b" |]).observe 85.

      let metricInfos = registry.exportAll() |> Array.ofSeq

      let _, metricDetails =
        metricInfos
          |> Array.find (fun (basicInfo , _) -> basicInfo.name = histogramConf.metricConf.name)

      let bucketsInfo =
        testBuckets
          |> Array.map (fun i -> (i, 0.))
          |> Map.ofSeq
          |> Map.add Double.PositiveInfinity 0.

      metricDetails |> Expect.containsAll "should contains histogram detail" [
          MetricInfo.Histogram { labels=Map ["queue name", "queue a"]; buckets=bucketsInfo |> Map.add 50. 1.; sum=15.; unit=U.Scalar };
          MetricInfo.Histogram { labels=Map ["queue name", "queue b"]; buckets=bucketsInfo  |> Map.add 100. 1.; sum=85.; unit=U.Scalar }
        ]


    testCase "histogram default buckets" <| fun _ ->
      let histogramConf = HistogramConf.create("some histogram", "some histogram", U.Seconds)
      histogramConf.buckets
        |> Expect.sequenceEqual "should have default buckets" HistogramConf.defaultBuckets


    testCase "histogram get linearBuckets" <| fun () ->
      let histogramConf =
        HistogramConf.create("some histogram", "some histogram", U.Seconds)
          |> HistogramConf.linearBuckets 5. 5. 5

      histogramConf.buckets
        |> Expect.sequenceEqual "should have buckets" [ 5.; 10.; 15.; 20.; 25.; ]

    testCase "histogram get exponentialBuckets" <| fun () ->
      let histogramConf =
        HistogramConf.create("some histogram", "some histogram", U.Seconds)
          |> HistogramConf.exponentialBuckets 5. 5. 5

      histogramConf.buckets
        |> Expect.sequenceEqual "should have buckets" [ 5.; 25.; 125.; 625.; 3125. ]


    testCase "avoid high cardinality throw strategy" <| fun () ->
      let registry = MetricRegistry()
      let gauge =
        basicConf
          |> MetricConf.labelNames [| "user_id" |]
          |> MetricConf.failByThrowing
          |> GaugeConf.create
          |> registry.getOrCreate

      for userId in 1..150 do
        gauge.withLabelValues [| string userId |] |> ignore

      Expect.throws "should throw if reach the high cardinality limit" <| fun _ ->
        gauge.withLabelValues [| "151" |] |> ignore


    testCase "avoiding high cardinality and changing the registry's fail strategy" <| fun _ ->
      let registry = MetricRegistry()
      let gauge = registry.getOrCreate(GaugeConf.create("signins", "total_signins", U.Scalar))

      // these work (in fact, we're creating 150 separate metrics, each tracking a different user):
      for userId in 1..150 do
        let g = Map [ "user_id", string userId ] |> gauge.withLabels
        g.inc 1.

      // then the 151'th should fail:
      Expect.throws "throws before changing the failWith strategy" <| fun _ ->
        let g = Map [ "user_id", "151" ] |> gauge.withLabels
        g.inc 1.

      let logger = StubLogger("cardinality test")
      // this ensures the metric registry uses our logger for warnings, avoiding to fail and only warning with the logger
      registry.setFailWith logger.warn

      // do the 151'th again
      let g = Map [ "user_id", "151" ] |> gauge.withLabels
      g.inc 5.

      // asserting the logger was called, on its warn-message:
      let message = logger.logged.[0].message
      let m = message.getAsOrThrow<EventMessage>()

      m.event
        |> Expect.equal
            "should send a warn message with information about error"
            "Do not use labels with high cardinality (more than 150 label values), such as UserId:s, e-mail addresses, or other unbounded sets of values."

      message.level
        |> Expect.equal "should have warn level" Warn
  ]
  |> testLabel "logary"