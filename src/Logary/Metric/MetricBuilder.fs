namespace Logary.Metric

open System.Collections.Concurrent

type MetricBuilder<'t when 't :> IMetric> =
  abstract basicConf: BasicConf
  abstract build: Map<string,string> -> MetricRegistry -> 't

and Metric<'t when 't :> IMetric> (builder: MetricBuilder<'t>, registry: MetricRegistry) =

  let emptyLabel = Map.empty
  let metricStore = new ConcurrentDictionary<Map<string,string>, 't>()
  let noLabelMetric = new Lazy<'t>(fun _ -> metricStore.GetOrAdd(emptyLabel, fun labels -> builder.build labels registry))

  /// label values of label names, should in same order
  abstract labels: string[] -> 't

  override x.labels labelValues =
    let labelNames = builder.basicConf.labelNames
    let labels =
      match labelNames.Length, labelValues.Length with
      | 0, 0 -> Map.empty
      | 0, _ -> failwith "metric has no label names but provide label values, maybe you need invoke noLabels"
      | _, 0 -> failwith "metric has label names but not provide label values, maybe you need invoke noLabels"
      | a, b when a = b -> Array.zip labelNames labelValues |> Map.ofSeq
      | _ -> failwith "metric labels should have same name/value length"
    let metric = metricStore.GetOrAdd(labels, fun labels ->  builder.build labels registry)
    metric

  member x.noLabels = noLabelMetric.Value

  interface MetricExporter with
    member x.basicConf = builder.basicConf
    member x.export () =
      let basicConf = builder.basicConf
      let basicInfo = { name= basicConf.name; description = basicConf.description }

      if basicConf.labelNames.Length = 0 then
        // https://prometheus.io/docs/practices/instrumentation/#avoid-missing-metrics
        noLabelMetric.Value |> ignore

      let metricInfos = metricStore.Values |> Seq.map (fun metric ->
        let _, info = metric.explore()
        info)
      (basicInfo, metricInfos)

/// used for register metric, and can export metric infos
and MetricRegistry() =
  let metricBackStore = new ConcurrentDictionary<string, MetricExporter>()

  /// register a metric to manager if not exist, otherwise return the registered one
  abstract registerMetric<'t when 't:> IMetric> : MetricBuilder<'t> -> Metric<'t>

  /// get metric infos for exporting to backends like prometheus
  abstract getMetricInfos: unit -> seq<BasicInfo * seq<MetricInfo>>

  default x.registerMetric builder =
    let metricName = builder.basicConf.name
    let metric = metricBackStore.GetOrAdd(metricName, fun _ -> new Metric<_>(builder, x) :> MetricExporter)
    if metric.basicConf <> builder.basicConf then
      failwithf "metric with same name needs have same basic conf: registered one: %A, newer one: %A"
        metric.basicConf builder.basicConf
    else downcast metric

  default x.getMetricInfos () =
    metricBackStore.Values |> Seq.map (fun metric -> metric.export ())

  member x.registerMetricWithNoLabels builder = (x.registerMetric builder).noLabels


module Metric =
  let inline labels labelValues (metric: Metric<_>) =
    metric.labels labelValues
  let inline noLabels (metric: Metric<_>) =
    metric.noLabels