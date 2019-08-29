namespace Logary.Metric

[<Struct>] // since it only has sizeof<*void> size, and thus should be easy to use in CPU registers.
type GaugeConf =
  { conf: BasicConf
    histogramConf: HistogramConf option
  }

  interface MetricBuilder<IGauge> with
    member x.build registry labels =
      let histogramMetric =
        x.histogramConf
        |> Option.map (fun conf ->
          let labelValues = conf.conf.labelNames |> Array.map (fun name -> labels.[name])
          conf |> registry.getOrCreate |> Metric.labels labelValues)
      new Gauge(x, labels, histogramMetric) :> IGauge

    member x.conf = x.conf

  static member create (name, description, ?labelNames, ?histogramConf) =
    let basic = BasicConf.create(name, description, ?labelNames=labelNames)
    { conf = basic; histogramConf = histogramConf }

  static member create innerConf =
    { conf = innerConf; histogramConf = None }

and Gauge(conf, labels, histogram) =
  let mutable gaugeValue = new DoubleAdder()
  let changeGaugeValueLock = obj ()

  interface IGauge with
    member x.inc value =
      match histogram with
      | Some histogram ->
        lock changeGaugeValueLock <| fun () ->
          gaugeValue.Add value
          gaugeValue.Sum () |> histogram.observe
      | None ->
        gaugeValue.Add value

    member x.dec value =
      (x :> IGauge).inc -value

    member x.set value =
      gaugeValue <- new DoubleAdder(value)
      histogram |> Option.iter (fun histogram -> histogram.observe value)

    member x.explore () =
      let confInfo = conf.conf
      let basicInfo = { name = confInfo.name; description = confInfo.description }
      let gaugeValue = gaugeValue.Sum()
      let metricInfo = { labels = labels; gaugeValue = gaugeValue }
      basicInfo, MetricInfo.Gauge metricInfo

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
  /// But if it is a simple gauge, which does not use lock for multithreaded inc/dec.
  let withHistogram buckets gaugeConf =
    let basicInfo = gaugeConf.conf
    let histogramMetricName = sprintf "%s_histogram" basicInfo.name
    let histogramDescription = sprintf "histogram of gauge: '%s'" basicInfo.description
    let histogramConf =
      HistogramConf.create { gaugeConf.conf with name=histogramMetricName; description = histogramDescription }
      |> HistogramConf.buckets buckets
    { gaugeConf with histogramConf = Some histogramConf }