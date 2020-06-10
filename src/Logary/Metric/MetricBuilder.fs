namespace Logary.Metric

open Logary.Internals
open System.Collections.Concurrent

type MetricBuilder<'t when 't :> IMetric> =
  abstract conf: BasicConf
  abstract build: MetricRegistry -> Map<string,string> -> 't

and Metric<'t when 't :> IMetric> (builder: MetricBuilder<'t>, registry: MetricRegistry) =
  let _noLabels = Map.empty
  let metricStore = ConcurrentDictionary<Map<string, string>, 't>()
  let noLabelMetric = new Lazy<'t>(fun _ -> metricStore.GetOrAdd(_noLabels, builder.build registry))
  let fail message =
    match builder.conf.failStrategy with
    | FailStrategy.Default -> DVar.get registry.failBehaviour message
    | FailStrategy.Throw -> failwith message

  let checkCardinalityOrFail () =
    match builder.conf.avoidHighCardinality with
    | Some cardinality when metricStore.Count >= cardinality ->
      fail (sprintf "Do not use labels with high cardinality (more than %d label values), such as UserId:s, e-mail addresses, or other unbounded sets of values." cardinality)
    | _ -> ()

  let labelsOfValues (values: string[]) =
    match builder.conf.labelNames.Length, values.Length with
    | 0, 0 ->
      Map.empty
    | 0, _ ->
      fail "metric has no label names but provide label values, maybe you need invoke noLabels"
      Map.empty
    | _, 0 ->
      fail "metric has label names but not provide label values, maybe you need invoke noLabels"
      Map.empty
    | a, b when a = b ->
      Array.zip builder.conf.labelNames values |> Map.ofSeq
    | _ ->
      fail "metric labels should have same name/value length"
      Map.empty

  member x.labels (labels: Map<string, string>) =
    do checkCardinalityOrFail ()
    metricStore.GetOrAdd(labels, builder.build registry)

  member x.labels (values: string[]) =
    x.labels (labelsOfValues values)

  /// Most of your custom-created Gauges should be without labels.
  member x.noLabels =
    noLabelMetric.Value

  interface MetricExporter with
    member x.basicConf = builder.conf
    member x.export () =
      let basicConf = builder.conf
      let basicInfo = { name = basicConf.name; description = basicConf.description }

      if basicConf.labelNames.Length = 0 then
        // https://prometheus.io/docs/practices/instrumentation/#avoid-missing-metrics
        noLabelMetric.Value |> ignore

      let metricInfos = metricStore.Values |> Seq.map (fun metric ->
        let _, info = metric.explore()
        info)
      (basicInfo, metricInfos)

/// Used to register metrics, and can export MetricInfo:s.
and MetricRegistry() =
  let metricBackStore = ConcurrentDictionary<string, MetricExporter>()
  let _failBehaviourD = DVar.create failwith

  member x.getOrCreate<'t when 't:> IMetric> (builder: MetricBuilder<'t>): Metric<'t> =
    let metricName = builder.conf.name
    let metric = metricBackStore.GetOrAdd(metricName, fun _ -> Metric<_>(builder, x) :> MetricExporter)
    if metric.basicConf <> builder.conf then
      let fail = DVar.get _failBehaviourD
      sprintf "metric with same name needs have same basic conf: registered one: %A, newer one: %A"
              metric.basicConf builder.conf
      |> fail

    downcast metric

  member x.getMetricInfos () =
    metricBackStore.Values |> Seq.map (fun metric -> metric.export ())

  member x.registerMetricWithNoLabels builder =
    x.getOrCreate(builder).noLabels

  member x.setFailBehaviour behaviour =
    DVar.set _failBehaviourD behaviour

  member x.failBehaviour: DVar<_> =
    _failBehaviourD

module Metric =
  let labels (labelValues: string[]) (metric: Metric<_>) = metric.labels labelValues
  let noLabels (metric: Metric<_>) = metric.noLabels