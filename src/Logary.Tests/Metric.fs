module Logary.Tests.Metric
open System
open Expecto
open Logary
open Logary.Metric
open Expecto.Flip

let private basicConf = MetricConf.create("items.in.queue", "items in queue")
let private gaugeConf = GaugeConf.create basicConf

#nowarn "25"

let tests = [
  testCase "register metric" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.getOrCreate
    gauge.unlabelled.inc 5.
    let basicInfo, MetricInfo.Gauge gaugeInfo = gauge.unlabelled.explore ()
    basicInfo.name |> Expect.equal "should have same name" gaugeConf.metricConf.name
    basicInfo.description |> Expect.equal "should have same description" gaugeConf.metricConf.description
    gaugeInfo.labels |> Expect.equal "should have empty labels" Map.empty
    gaugeInfo.value  |> Expect.equal  "should have same gauge value" 5.

  testCase "register metric multiple times" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.getOrCreate
    gauge.unlabelled.inc 15.
    let gauge = gaugeConf |> registry.getOrCreate
    gauge.unlabelled.dec 10.

    let _, MetricInfo.Gauge gaugeInfo = gauge.unlabelled.explore ()
    gaugeInfo.value |> Expect.equal "should reuse the same one" 5.

    Expect.throws "should throws when register metric with same name but different basic conf " <| fun () ->
      GaugeConf.create {basicConf with labelNames = [| "with some label" |]} |> registry.getOrCreate |> ignore

  testCase "register metric with no labels" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.getOrCreateUnlabelled
    gauge.inc 10.
    let gauge = gaugeConf |> registry.getOrCreate
    gauge.unlabelled.inc 10.
    let _, MetricInfo.Gauge gaugeInfo = gauge.unlabelled.explore ()
    gaugeInfo.value |> Expect.equal "registerMetricWithNoLabels is just a shotcut for registerMetric(gaugeConf).noLabels" 20.


  testCase "register metric with labels" <| fun () ->

    Expect.throws "should throws when gauge with label value but no label names set up" <| fun () ->
      let registry = new MetricRegistry()
      let gauge = gaugeConf |> registry.getOrCreate
      (gauge.withLabelValues [| "some queue name" |]).set 10.

    Expect.throws "should throws when label values's length not equal label names's length" <| fun () ->
      let registry = new MetricRegistry()
      let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
      (gauge.withLabelValues [| "some queue name"; "some other value" |]).set 10.

    Expect.throws "should throws when label values's length not equal label names's length" <| fun () ->
      let registry = new MetricRegistry()
      let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
      (gauge.withLabelValues [||]).set 10.

    let registry = new MetricRegistry()
    let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
    (gauge.withLabelValues [| "some queue name" |]).set 10.
    gauge.unlabelled.set 20.

    let metricInfos = registry.exportAll () |> Array.ofSeq
    metricInfos.Length |> Expect.equal "should have only one metric" 1

    let _, metricInfoDetails = metricInfos.[0]
    let mutable metricInfoDetailsLength = 0
    for metricInfoDetail in metricInfoDetails do
      match metricInfoDetail with
      | MetricInfo.Gauge gaugeInfo ->
        match gaugeInfo with
        | gaugeInfo when gaugeInfo.labels = Map.empty ->
          gaugeInfo.value  |> Expect.equal "no label name metric should have gauge value 20." 20.
        | _ ->  gaugeInfo.value  |> Expect.equal "metric with label name should have gauge value 10." 10.
      | _ -> failtest "should not occur"
      metricInfoDetailsLength <- metricInfoDetailsLength + 1

    metricInfoDetailsLength |> Expect.equal "should have two metric info, one with label name, one with no labels" 2

  testCase "get metric info from metric registry" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.getOrCreate
    let metricInfos = registry.exportAll () |> Array.ofSeq
    let basicInfo, metricInfoDetails = metricInfos.[0]
    basicInfo.name |> Expect.equal "should have same name"  gaugeConf.metricConf.name
    basicInfo.description |> Expect.equal "should have same description"  gaugeConf.metricConf.description
    metricInfoDetails |> Expect.isEmpty "should have empty metric detail since there's no gauge operation"

    (gauge.withLabelValues [| "some queue name" |]).set 10.
    let _, metricInfoDetails = registry.exportAll () |> Seq.head
    metricInfoDetails |> Expect.sequenceEqual "should have one gauge detail" [ MetricInfo.Gauge { labels= [("queue name", "some queue name")] |> Map.ofSeq ; value = 10. } ]

  testCase "gauge" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.getOrCreateUnlabelled
    gauge.set 200.
    gauge.inc 100.
    gauge.inc -50.
    gauge.dec 50.
    gauge.dec -50.
    let _, MetricInfo.Gauge gaugeInfo = gauge.explore ()
    gaugeInfo.value |> Expect.equal "gauge value should be 250" 250.

  testList "gauge with histogram" [
    let testBuckets = [| 5.; 10.; 50.; 100.; |]
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> GaugeConf.withHistogram testBuckets |> registry.getOrCreateUnlabelled

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
      metricDetails |> Expect.sequenceEqual "should have one gauge detail" [ MetricInfo.Gauge { labels= Map.empty ; value = 25. } ]

    yield testCase "histogram detail" <| fun () ->
      let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = sprintf "%s_histogram" gaugeConf.metricConf.name)
      let bucketsInfo = [(5.,0.); (10.,1.); (50., 5.); (100., 1.); (Double.PositiveInfinity, 1.)] |> Map.ofSeq
      metricDetails |> Expect.sequenceEqual "should have histogram detail" [ MetricInfo.Histogram { labels= Map.empty ; buckets = bucketsInfo; sum = float(200+60+45+40+25+15+7+25) } ]
  ]

  testCase "gauge with histogram but no gauge occur, should exported a empty result" <| fun () ->
    let testBuckets = [| 5.; 10.; 50.; 100.; |]
    let registry = new MetricRegistry()
    let histogramConf = HistogramConf.create("some histogram", "some histogram") |> HistogramConf.buckets testBuckets
    let gauge = gaugeConf |> GaugeConf.enableHistogram histogramConf |> registry.getOrCreateUnlabelled

    let metricInfos = registry.exportAll () |> Array.ofSeq
    let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = gaugeConf.metricConf.name)
    metricDetails |> Expect.sequenceEqual "should have one empty gauge details" [ MetricInfo.Gauge { labels= Map.empty ; value = 0. } ]

    let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = histogramConf.metricConf.name)
    let bucketsInfo = testBuckets |> Array.map (fun i -> (i, 0.)) |> Map.ofSeq |> Map.add Double.PositiveInfinity 0.
    metricDetails |> Expect.sequenceEqual "should have empty histogram detail" [ MetricInfo.Histogram { labels= Map.empty ; buckets = bucketsInfo; sum = 0. } ]


  testCase "histogram with label" <| fun () ->
    let testBuckets = [| 5.; 10.; 50.; 100.; |]
    let registry = new MetricRegistry()

    let histogramConf =
      MetricConf.create("some histogram", "some histogram desc")
      |> BasicConf.labelNames [| "queue name" |]
      |> HistogramConf.create
      |> HistogramConf.buckets testBuckets

    let histogram = histogramConf |> registry.getOrCreate

    (histogram.withLabelValues [| "queue a" |]).observe 15.
    (histogram.withLabelValues [| "queue b" |]).observe 85.

    let metricInfos = registry.exportAll () |> Array.ofSeq
    let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = histogramConf.metricConf.name)
    let bucketsInfo = testBuckets |> Array.map (fun i -> (i, 0.)) |> Map.ofSeq |> Map.add Double.PositiveInfinity 0.
    metricDetails |> Expect.containsAll "should contains histogram detail" [
        MetricInfo.Histogram { labels= [("queue name", "queue a")] |> Map.ofSeq ; buckets = bucketsInfo |> Map.add 50. 1. ; sum = 15. };
        MetricInfo.Histogram { labels= [("queue name", "queue b")] |> Map.ofSeq ; buckets = bucketsInfo  |> Map.add 100. 1. ; sum = 85. }
      ]


  testCase "histogram get linearBuckets" <| fun () ->
    let histogramConf = HistogramConf.create("some histogram", "some histogram")
    histogramConf.buckets |> Expect.sequenceEqual "should have default buckets" HistogramConf.defaultBuckets
    let histogramConf = histogramConf |> HistogramConf.linearBuckets 5. 5. 5
    histogramConf.buckets |> Expect.sequenceEqual "should have buckets" [ 5.; 10.; 15.; 20.; 25.; ]

  testCase "histogram get exponentialBuckets" <| fun () ->
    let histogramConf = HistogramConf.create("some histogram", "some histogram") |> HistogramConf.exponentialBuckets 5. 5. 5
    histogramConf.buckets |> Expect.sequenceEqual "should have buckets" [ 5.; 25.; 125.; 625.; 3125.; ]

  testCase "avoid high cardinality throw strategy" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = basicConf |> BasicConf.labelNames [| "user_id" |] |> BasicConf.throwWhenFail |> GaugeConf.create |> registry.getOrCreate
    for userId in 1..150 do
      gauge.withLabelValues [| string userId |] |> ignore

    Expect.throws "should throw if reach the high cardinality limit" (fun () ->
      gauge.withLabelValues [| "151" |] |> ignore
    )

  testCase "avoid high cardinality change default fail strategy" <| fun () ->
    let registry = new MetricRegistry()
    let testLogger = {
      new Logger with
        member x.name = PointName [| "test" |]
        member x.level = Info
        member x.logWithAck (waitForBuffer, level) factory =
          let msg = factory level
          msg.value |> Expect.equal "should warn message with info" "Do not use labels with high cardinality (more than 150 label values), such as UserId:s, e-mail addresses, or other unbounded sets of values."
          msg.level |> Expect.equal "should have warn level" Warn
          LogResult.success
    }

    let warnBehavior = Message.Model.EventMessage >> testLogger.warn
    let gauge = {basicConf with labelNames = [| "user_id" |]} |> GaugeConf.create |> registry.getOrCreate

    for userId in 1..150 do
      gauge.withLabelValues [| string userId |] |> ignore

    Expect.throws "should throw if not change default fail behavior" (fun () ->
      gauge.withLabelValues [| "151" |] |> ignore
    )

    registry.setFailWith warnBehavior

    gauge.withLabelValues [| "152" |] |> ignore

]