namespace Logary.Metric


type GaugeConf =
  {
   basicInfo: BasicConf
   histogramConf: HistogramConf option
  }

  interface MetricBuilder<IGauge> with
    member x.build labels registry =
      let histrgomMetric =
        x.histogramConf
        |> Option.map (fun conf ->
          let labelValues = conf.basicInfo.labelNames |> Array.map (fun name -> labels.[name])
          conf |> registry.registerMetric |> Metric.labels labelValues)
      new Gauge(x, labels, histrgomMetric) :> IGauge

    member x.basicConf = x.basicInfo

  static member create (name, description) =
    let basic = { name =  name; description = description; labelNames = [||] }
    {basicInfo = basic; histogramConf = None}

  static member create (basic) =
    {basicInfo = basic; histogramConf = None}

and Gauge(conf, labels, histogram) =

  let mutable gaugeValue = new DoubleAdder()

  let tryDoHistogramObserve () =
    histogram |> Option.iter(fun histogram ->
      let sum = gaugeValue.Sum ()
      histogram.observe sum)

  interface IGauge with
    member x.inc value =
      gaugeValue.Add value
      tryDoHistogramObserve()

    member x.dec value =
      gaugeValue.Add -value
      tryDoHistogramObserve()

    member x.set value =
      gaugeValue <- new DoubleAdder(value)
      tryDoHistogramObserve()

    member x.explore () =
      let confInfo = conf.basicInfo
      let basicInfo = { name= confInfo.name; description = confInfo.description }
      let gaugeValue =  gaugeValue.Sum()
      let metricInfo = { labels = labels; gaugeValue = gaugeValue }
      (basicInfo, MetricInfo.Gauge metricInfo)

module GaugeConf =

  /// Use this method carefully, since this will calculate each gague's change value as the ovserve data for histogram.
  /// Consider using histogram seperately
  let enableHistogram histogramConf gaugeConf =
    { gaugeConf with histogramConf = Some histogramConf }

  /// Use this method carefully, since this will calculate each gague's change value as the ovserve data for histogram
  /// Consider using histogram seperately
  let withHistogram buckets gaugeConf =
    let basicInfo = gaugeConf.basicInfo
    let histogramMetricName = sprintf "%s_histogram" basicInfo.name
    let histogramDescription = sprintf "histogram of gauge:`%s`" basicInfo.description
    let histogramConf =
      HistogramConf.create {gaugeConf.basicInfo with name=histogramMetricName; description = histogramDescription;}
      |> HistogramConf.buckets buckets
    { gaugeConf with histogramConf = Some histogramConf }