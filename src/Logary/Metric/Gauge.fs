namespace Logary.Metric

open Logary

[<Struct>] // since it only has sizeof<*void> size, and thus should be easy to use in CPU registers.
type GaugeConf =
  { metricConf: MetricConf
    histogramConf: HistogramConf option }

  interface MetricBuilder<IGauge> with
    member x.build registry labels =
      let histogramMetric =
        x.histogramConf
        |> Option.map (fun conf ->
          let labelValues = conf.metricConf.labelNames |> Array.map (fun name -> labels.[name])
          conf |> registry.getOrCreate |> Metric.labels labelValues)
      Gauge(x, labels, histogramMetric) :> IGauge

    member x.conf = x.metricConf

  /// Note: having a histogram conf in the Gauge is a bit funky. Prefer to use a Histogram then.
  static member create (name, description, ?units, ?labelNames, ?histogramConf) =
    let units = defaultArg units U.Scalar
    let basic = MetricConf.create(name, description, units, ?labelNames=labelNames)
    { metricConf = basic; histogramConf = histogramConf }

  static member create (innerConf: MetricConf) =
    { metricConf = innerConf; histogramConf = None }

and Gauge(gaugeConf, labels, histogram) =
  let mutable gaugeValue = DoubleAdder()
  let updateGaugeSem = obj ()

  interface IGauge with
    member x.inc value =
      match histogram with
      | Some histogram ->
        lock updateGaugeSem <| fun () ->
          gaugeValue.Add value
          gaugeValue.Sum () |> histogram.observe
      | None ->
        gaugeValue.Add value

    member x.dec value =
      (x :> IGauge).inc -value

    member x.set value =
      gaugeValue <- DoubleAdder(value)
      histogram |> Option.iter (fun histogram -> histogram.observe value)

    member x.explore () =
      let info = { labels = labels; value = gaugeValue.Sum(); unit = gaugeConf.metricConf.unit }
      gaugeConf.metricConf.asInfo, MetricInfo.Gauge info

module GaugeConf =
  /// Consider using Histogram separately.
  ///
  /// Technically, a gauge can also be a histogram, so if you want to measure some histograms when gauge something,
  /// you can use this method. However, because the gauge can use the inc/dec method compared to the histogram,
  /// if you need to use the histogram when gauge, it will cause the sum operation after inc/dec.
  /// In order to ensure the histogram is more accurate under multi-threading, the lock will be performed.
  /// But if it is a simple gauge, which does not use lock for multi-threaded inc/dec
  let enableHistogram histogramConf gaugeConf =
    { gaugeConf with histogramConf = Some histogramConf }

  /// Consider using histogram separately.
  ///
  /// Technically, a gauge can also be a histogram, so if you want to measure some histograms when you have a gauge,
  /// you can use this method. However, because the gauge can use the inc/dec method as opposed to the histogram,
  /// if you need to use the histogram when gauge, it will cause the sum operation after inc/dec.
  ///
  /// In order to ensure the histogram is more accurate under multi-threading, a lock will be held during updating.
  /// But if it is a simple gauge, which does not use lock for multi-threaded inc/dec.
  let withHistogram buckets gaugeConf =
    let basicInfo = gaugeConf.metricConf
    let histogramMetricName = sprintf "%s_histogram" basicInfo.name
    let histogramDescription = sprintf "histogram of gauge: '%s'" basicInfo.description
    let histogramConf =
      HistogramConf.create { gaugeConf.metricConf with name=histogramMetricName; description = histogramDescription }
      |> HistogramConf.buckets buckets
    { gaugeConf with histogramConf = Some histogramConf }