namespace Logary.Metric

open Logary.Internals
open System.Collections.Concurrent

type MetricBuilder<'t when 't :> IMetric> =
  abstract basicConf: BasicConf
  abstract build: Map<string,string> -> MetricRegistry -> 't

and Metric<'t when 't :> IMetric> (builder: MetricBuilder<'t>, registry: MetricRegistry) =

  let emptyLabel = Map.empty
  let metricStore = new ConcurrentDictionary<Map<string,string>, 't>()
  let noLabelMetric = new Lazy<'t>(fun _ -> metricStore.GetOrAdd(emptyLabel, fun labels -> builder.build labels registry))

  let doWithFailStrategy (strategy: FailStrategy) message =
    match strategy with
    | Default -> registry.defaultFailBehavior message
    | Throw -> failwith message

  member x.labels (labelValues: string[]) =
    let doWithFailStrategy = doWithFailStrategy builder.basicConf.failStrategy

    builder.basicConf.avoidHighCardinality |> Option.iter (fun cardinality ->
      if metricStore.Count >= cardinality then
        doWithFailStrategy (sprintf "Do not use labels with high cardinality (more then %d label values), such as user IDs, email addresses, or other unbounded sets of values." cardinality)
    )

    let labelNames = builder.basicConf.labelNames
    let labels =
      match labelNames.Length, labelValues.Length with
      | 0, 0 -> Map.empty
      | 0, _ ->
        doWithFailStrategy "metric has no label names but provide label values, maybe you need invoke noLabels"
        Map.empty
      | _, 0 ->
        doWithFailStrategy "metric has label names but not provide label values, maybe you need invoke noLabels"
        Map.empty
      | a, b when a = b -> Array.zip labelNames labelValues |> Map.ofSeq
      | _ ->
        doWithFailStrategy "metric labels should have same name/value length"
        Map.empty

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
  let defaultFailBehaviorD = DVar.create failwith

  member x.registerMetric<'t when 't:> IMetric> (builder: MetricBuilder<'t>): Metric<'t> =
    let metricName = builder.basicConf.name
    let metric = metricBackStore.GetOrAdd(metricName, fun _ -> new Metric<_>(builder, x) :> MetricExporter)
    if metric.basicConf <> builder.basicConf then
      sprintf "metric with same name needs have same basic conf: registered one: %A, newer one: %A"
              metric.basicConf builder.basicConf
      |> x.defaultFailBehavior

    downcast metric

  member x.getMetricInfos () =
    metricBackStore.Values |> Seq.map (fun metric -> metric.export ())

  member x.registerMetricWithNoLabels builder = (x.registerMetric builder).noLabels

  member x.changeDefaultFailBehavior behavior = DVar.set defaultFailBehaviorD behavior

  member x.defaultFailBehavior = DVar.get defaultFailBehaviorD



module Metric =
  let inline labels labelValues (metric: Metric<_>) =
    metric.labels labelValues
  let inline noLabels (metric: Metric<_>) =
    metric.noLabels