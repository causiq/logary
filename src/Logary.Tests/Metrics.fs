module Logary.Tests.Metrics
open System
open Expecto
open Logary
open Logary.Metric
open Expecto.Flip
open Logary.Metric

let private basicConf = BasicConf.create "items.in.queue" "items in queue"
let private gaugeConf = GaugeConf.create basicConf

let tests = [

  testCase "register metric" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.registerMetric
    gauge.noLabels.inc 5.
    let basicInfo, MetricInfo.Gauge gaugeInfo = gauge.noLabels.explore ()
    basicInfo.name |> Expect.equal "should have same name"  gaugeConf.basicInfo.name
    basicInfo.description |> Expect.equal "should have same description"  gaugeConf.basicInfo.description
    gaugeInfo.labels |> Expect.equal "should have empty labels" Map.empty
    gaugeInfo.gaugeValue  |> Expect.equal  "should have same gauge value" 5.

  testCase "register metric multi times" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.registerMetric
    gauge.noLabels.inc 15.
    let gauge = gaugeConf |> registry.registerMetric
    gauge.noLabels.dec 10.

    let _, MetricInfo.Gauge gaugeInfo = gauge.noLabels.explore ()
    gaugeInfo.gaugeValue |> Expect.equal "should reuse the same one" 5.

    Expect.throws "should throws when register metric with same name but different basic conf " <| fun () ->
      GaugeConf.create {basicConf with labelNames = [| "with some label" |]} |> registry.registerMetric |> ignore

  testCase "register metric with no labels" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.registerMetricWithNoLabels
    gauge.inc 10.
    let gauge = gaugeConf |> registry.registerMetric
    gauge.noLabels.inc 10.
    let _, MetricInfo.Gauge gaugeInfo = gauge.noLabels.explore ()
    gaugeInfo.gaugeValue |> Expect.equal "registerMetricWithNoLabels is just a shotcut for registerMetric(gaugeConf).noLabels" 20.


  testCase "register metric with labels" <| fun () ->

    Expect.throws "should throws when gauge with label value but no label names set up" <| fun () ->
      let registry = new MetricRegistry()
      let gauge = gaugeConf |> registry.registerMetric
      (gauge.labels [| "some queue name" |]).set 10.

    Expect.throws "should throws when label values's length not equal label names's length" <| fun () ->
      let registry = new MetricRegistry()
      let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.registerMetric
      (gauge.labels [| "some queue name"; "some other value" |]).set 10.

    Expect.throws "should throws when label values's length not equal label names's length" <| fun () ->
      let registry = new MetricRegistry()
      let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.registerMetric
      (gauge.labels [||]).set 10.

    let registry = new MetricRegistry()
    let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.registerMetric
    (gauge.labels [| "some queue name" |]).set 10.
    gauge.noLabels.set 20.

    let metricInfos = registry.getMetricInfos () |> Array.ofSeq
    metricInfos.Length |> Expect.equal "should have only one metric" 1

    let _, metricInfoDetails = metricInfos.[0]
    let mutable metricInfoDetailsLength = 0
    for metricInfoDetail in metricInfoDetails do
      match metricInfoDetail with
      | MetricInfo.Gauge gaugeInfo ->
        match gaugeInfo with
        | gaugeInfo when gaugeInfo.labels = Map.empty ->
          gaugeInfo.gaugeValue  |> Expect.equal "no label name metric should have gauge value 20." 20.
        | _ ->  gaugeInfo.gaugeValue  |> Expect.equal "metric with label name should have gauge value 10." 10.
      | _ -> failtest "should not occur"
      metricInfoDetailsLength <- metricInfoDetailsLength + 1

    metricInfoDetailsLength |> Expect.equal "should have two metric info, one with label name, one with no labels" 2

  testCase "get metric info from metric registry" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = {basicConf with labelNames = [| "queue name" |]} |> GaugeConf.create |> registry.registerMetric
    let metricInfos = registry.getMetricInfos () |> Array.ofSeq
    let basicInfo, metricInfoDetails = metricInfos.[0]
    basicInfo.name |> Expect.equal "should have same name"  gaugeConf.basicInfo.name
    basicInfo.description |> Expect.equal "should have same description"  gaugeConf.basicInfo.description
    metricInfoDetails |> Expect.isEmpty "should have empty metric detail since there's no gauge operation"

    (gauge.labels [| "some queue name" |]).set 10.
    let _, metricInfoDetails = registry.getMetricInfos () |> Seq.head
    metricInfoDetails |> Expect.sequenceEqual "should have one gauge detail" [ MetricInfo.Gauge { labels= [("queue name", "some queue name")] |> Map.ofSeq ; gaugeValue = 10. } ]

  testCase "gauge" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> registry.registerMetricWithNoLabels
    gauge.set 200.
    gauge.inc 100.
    gauge.inc -50.
    gauge.dec 50.
    gauge.dec -50.
    let _, MetricInfo.Gauge gaugeInfo = gauge.explore ()
    gaugeInfo.gaugeValue |> Expect.equal "gauge value should be 250" 250.

  testList "gauge with histogram" [
    let testBuckets = [| 5.; 10.; 50.; 100.; |]
    let registry = new MetricRegistry()
    let gauge = gaugeConf |> GaugeConf.withHistogram testBuckets |> registry.registerMetricWithNoLabels

    gauge.set 200.
    gauge.dec 140. // 60
    gauge.dec 15.  // 45
    gauge.dec 5.   // 40
    gauge.dec 15.  // 25
    gauge.dec 10.  // 15
    gauge.dec 8.   // 7
    gauge.inc 18.  // 25

    let metricInfos = registry.getMetricInfos () |> Array.ofSeq

    yield testCase "gauge detail" <| fun () ->
      let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = gaugeConf.basicInfo.name)
      metricDetails |> Expect.sequenceEqual "should have one gauge detail" [ MetricInfo.Gauge { labels= Map.empty ; gaugeValue = 25. } ]

    yield testCase "histogram detail" <| fun () ->
      let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = sprintf "%s_histogram" gaugeConf.basicInfo.name)
      let bucketsInfo = [(5.,0.); (10.,1.); (50., 5.); (100., 1.); (Double.PositiveInfinity, 1.)] |> Map.ofSeq
      metricDetails |> Expect.sequenceEqual "should have histogram detail" [ MetricInfo.Histogram { labels= Map.empty ; bucketsInfo = bucketsInfo; sumInfo = float(200+60+45+40+25+15+7+25) } ]
  ]

  testCase "gauge with histogram but no gauge occur, should exported a empty result" <| fun () ->
    let testBuckets = [| 5.; 10.; 50.; 100.; |]
    let registry = new MetricRegistry()
    let histogramConf = HistogramConf.create("some histogram", "some histogram") |> HistogramConf.buckets testBuckets
    let gauge = gaugeConf |> GaugeConf.enableHistogram histogramConf |> registry.registerMetricWithNoLabels

    let metricInfos = registry.getMetricInfos () |> Array.ofSeq
    let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = gaugeConf.basicInfo.name)
    metricDetails |> Expect.sequenceEqual "should have one empty gauge details" [ MetricInfo.Gauge { labels= Map.empty ; gaugeValue = 0. } ]

    let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = histogramConf.basicInfo.name)
    let bucketsInfo = testBuckets |> Array.map (fun i -> (i, 0.)) |> Map.ofSeq |> Map.add Double.PositiveInfinity 0.
    metricDetails |> Expect.sequenceEqual "should have empty histogram detail" [ MetricInfo.Histogram { labels= Map.empty ; bucketsInfo = bucketsInfo; sumInfo = 0. } ]


  testCase "histogram with label" <| fun () ->
    let testBuckets = [| 5.; 10.; 50.; 100.; |]
    let registry = new MetricRegistry()

    let histogramConf =
      {name = "some histogram"; description = "some histogram"; labelNames = [| "queue name" |]; avoidHighCardinality = Some BasicConf.defaultHighCardinalityLimit}
      |> HistogramConf.create
      |> HistogramConf.buckets testBuckets

    let histogram = histogramConf |> registry.registerMetric

    (histogram.labels [| "queue a" |]).observe 15.
    (histogram.labels [| "queue b" |]).observe 85.

    let metricInfos = registry.getMetricInfos () |> Array.ofSeq
    let _, metricDetails = metricInfos |> Array.find (fun (basicInfo , _) -> basicInfo.name = histogramConf.basicInfo.name)
    let bucketsInfo = testBuckets |> Array.map (fun i -> (i, 0.)) |> Map.ofSeq |> Map.add Double.PositiveInfinity 0.
    metricDetails |> Expect.containsAll "should contains histogram detail" [
        MetricInfo.Histogram { labels= [("queue name", "queue a")] |> Map.ofSeq ; bucketsInfo = bucketsInfo |> Map.add 50. 1. ; sumInfo = 15. };
        MetricInfo.Histogram { labels= [("queue name", "queue b")] |> Map.ofSeq ; bucketsInfo = bucketsInfo  |> Map.add 100. 1. ; sumInfo = 85. }
      ]


  testCase "histogram get linearBuckets" <| fun () ->
    let histogramConf = HistogramConf.create("some histogram", "some histogram")
    histogramConf.buckets |> Expect.sequenceEqual "should have default buckets" HistogramConf.defaultBuckets
    let histogramConf = histogramConf |> HistogramConf.linearBuckets 5. 5. 5
    histogramConf.buckets |> Expect.sequenceEqual "should have buckets" [ 5.; 10.; 15.; 20.; 25.; ]

  testCase "histogram get exponentialBuckets" <| fun () ->
    let histogramConf = HistogramConf.create("some histogram", "some histogram") |> HistogramConf.exponentialBuckets 5. 5. 5
    histogramConf.buckets |> Expect.sequenceEqual "should have buckets" [ 5.; 25.; 125.; 625.; 3125.; ]

  testCase "avoid high cardinality" <| fun () ->
    let registry = new MetricRegistry()
    let gauge = {basicConf with labelNames = [| "user_id" |]} |> GaugeConf.create |> registry.registerMetric
    for userId in 1..150 do
      gauge.labels [| string userId |] |> ignore

    Expect.throws "should throw if reach the high cardinality limit" (fun () ->
      gauge.labels [| "151" |] |> ignore
    )
]