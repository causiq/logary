namespace Logary.Metric

open System.Collections.Concurrent

type MetricBuilder<'t when 't :> IMetric> =
  abstract basicConf: BasicConf
  abstract build: Map<string,string> -> 't

type Metric<'t when 't :> IMetric> (builder: MetricBuilder<'t>) =

  let emptyLabel = Map.empty
  let metricStore = new ConcurrentDictionary<Map<string,string>, 't>()
  let noLabelMetric = new Lazy<'t>(fun _ -> metricStore.GetOrAdd(emptyLabel, fun lables -> builder.build lables))

  /// label values of label names, should in same order
  abstract labels: string[] -> 't

  override x.labels labelValues =
    let labelNames = builder.basicConf.labelNames
    let labels =
      match labelNames.Length, labelValues.Length with
      | 0, 0 -> Map.empty
      | 0, _ -> failwith "metric has no label names but provide label values, maybe you need invoke noLables"
      | _, 0 -> failwith "metric has label names but not provide label values, maybe you need invoke noLables"
      | a, b when a = b -> Array.zip labelNames labelValues |> Map.ofSeq
      | _ -> failwith "metric labels should have same name/value length"
    let metric = metricStore.GetOrAdd(labels, fun labels ->  builder.build labels)
    metric

  member x.noLabels = noLabelMetric.Value

  interface MetricExporter with
    member x.basicConf = builder.basicConf
    member x.export () =
      let basicConf = builder.basicConf
      let basicInfo = { name= basicConf.name; description = basicConf.description }
      let metricInfos = metricStore.Values |> Seq.map (fun metric ->
        let _, info = metric.explore()
        info)
      (basicInfo, metricInfos)

