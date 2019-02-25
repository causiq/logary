namespace Logary.Metric

open System.Collections.Concurrent

/// used for register metric, and can export metric infos
type MetricRegistry() =
  let metricBackStore = new ConcurrentDictionary<string, MetricExporter>()

  /// register a metric to manager if not exist, otherwise return the registered one
  abstract registerMetric<'t when 't:> IMetric> : MetricBuilder<'t> -> Metric<'t>

  /// get metric infos for exporting to backends like prometheus
  abstract getMetrictInfos: unit -> seq<BasicInfo * seq<MetricInfo>>

  default x.registerMetric builder =
    let metricName = builder.basicConf.name
    let metric = metricBackStore.GetOrAdd(metricName, fun _ -> new Metric<_>(builder) :> MetricExporter)
    if metric.basicConf <> builder.basicConf then
      failwithf "metric with same name needs have same basic conf: registered one: %A, newer one: %A"
        metric.basicConf builder.basicConf
    else downcast metric

  default x.getMetrictInfos () =
    metricBackStore.Values |> Seq.map (fun metric -> metric.export ())
