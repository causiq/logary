namespace Logary.Metric

open Logary.Internals
open System.Collections.Concurrent

type MetricBuilder<'TMetric when 'TMetric :> IMetric> =
  /// This particular instance's metric configuration.
  abstract conf: MetricConf

  /// Builds a concrete, fully labelled, `IMetric` instance from the registry and the `Map<string, string>` of labels.
  abstract build: registry: MetricRegistry -> labels: Map<string, string> -> 'TMetric

/// A store for this metric (base name of a measurement); which in turn can hold multiple unique Histograms or Gauges
/// keyed by their own labels.
and Metric<'TMetric when 'TMetric :> IMetric> (builder: MetricBuilder<'TMetric>, registry: MetricRegistry) =
  // If we have `builder.conf.avoidHighCardinality` enabled, this dict should never contain more than that configured
  // number (the conf's cardinality-number) of keys. Every key results in a metric.
  let metrics = ConcurrentDictionary<Map<string, string>, 'TMetric>()
  let unlabelledMetric = new Lazy<'TMetric>(fun _ -> metrics.GetOrAdd(Map.empty, builder.build registry))

  // PRIVATE MEMBERS

  let fail message =
    match builder.conf.failStrategy with
    | FailStrategy.RegistryDecides -> registry.failWith message
    | FailStrategy.Throw -> failwith message

  let checkCardinalityOrFail () =
    match builder.conf.maxCardinality with
    | Some maxLabelCount when metrics.Count >= maxLabelCount ->
      fail (sprintf "Do not use labels with high cardinality (more than %d label values), such as UserId:s, e-mail addresses, or other unbounded sets of values." maxLabelCount)
    | _ -> ()

  let labelsOfValues (values: string[]) =
    match builder.conf.labelNames.Length, values.Length with
    | 0, 0 ->
      Map.empty
    | 0, _ ->
      fail "metric has no label names but provided label values, maybe you need use metric.unlabelled"
      Map.empty
    | _, 0 ->
      fail "metric has label names but not provided label values, maybe you need invoke metric.unlabelled"
      Map.empty
    | a, b when a = b ->
      Array.zip builder.conf.labelNames values |> Map.ofArray
    | _ ->
      fail "metric labels should have same name/value length"
      Map.empty

  // PUBLIC MEMBERS:

  member x.withLabels (labels: Map<string, string>) =
    do checkCardinalityOrFail ()
    metrics.GetOrAdd(labels, builder.build registry)

  /// Returns a new metric with the given values, ordinally associated with the `builder.metricConf.labelNames`.
  member x.withLabelValues (values: string[]) =
    let labels = labelsOfValues values
    x.withLabels labels

  /// Most of your custom-created Gauges should be without labels.
  member x.unlabelled =
    unlabelledMetric.Value

  interface MetricExporter with
    member x.conf = builder.conf
    member x.export() =
      let basicConf = builder.conf

      if basicConf.labelNames.Length = 0 then
        // https://prometheus.io/docs/practices/instrumentation/#avoid-missing-metrics
        unlabelledMetric.Force() |> ignore

      basicConf.asInfo,
      metrics.Values
        |> Seq.map (fun metric -> let _, info = metric.explore() in info)
        |> Array.ofSeq

/// Used to register metrics, and can export MetricInfo:s; has a backing store of metrics, keyed by their labels.
and MetricRegistry() =
  let exporters = ConcurrentDictionary<string, MetricExporter>()
  let failD = DVar.create failwith

  member x.getOrCreate<'t when 't:> IMetric>(builder: MetricBuilder<'t>): Metric<'t> =
    let metric =
      exporters.GetOrAdd(
        builder.conf.name,
        fun _ -> Metric<_>(builder, x) :> MetricExporter)

    if metric.conf <> builder.conf then
      DVar.get failD <|
        sprintf "The Metric had a previously seen name, so it needs to have an identical MetricConf value. Existing one: %A, newer one: %A"
                metric.conf builder.conf

    downcast metric

  member x.getOrCreateUnlabelled builder =
    x.getOrCreate(builder).unlabelled

  /// Iterates through all the metrics stored in the MetricRegistry, exporting their current state as readonly info
  /// DTOs.
  member x.exportAll() =
    exporters.Values |> Seq.map (fun metric -> metric.export())

  /// Changes the way this MetricRegistry treats failures. Failures primarily revolve around high-cardinality Metric
  /// instances.
  member x.setFailWith behaviour =
    DVar.set failD behaviour

  /// Invokes the configured `failWith` function for this registry.
  member x.failWith message = DVar.get failD message

module Metric =
  let labels (labelValues: string[]) (metric: Metric<_>) = metric.withLabelValues labelValues
  let unlabelled (metric: Metric<_>) = metric.unlabelled