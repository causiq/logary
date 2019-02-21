namespace Logary.Metric

open System.Collections.Concurrent

[<AbstractClass>]
type Metric<'t when 't :> IMetric> (builder: MetricBuilder<'t>) =

  let emptyLabel = Map.empty
  let metricStore = new ConcurrentDictionary<Map<string,string>, 't>()
  let noLabelMetric = new Lazy<'t>(fun _ -> metricStore.GetOrAdd(emptyLabel, fun _ -> builder.build().wrapper))

  /// label values of label names, should in same order
  abstract labels: string[] -> 't

  abstract wrapper: 't

  override x.labels labelValues =
    let labelNames = builder.basicConf.labelNames
    let labels =
      match labelNames.Length, labelValues.Length with
      | 0, 0 -> Map.empty
      | 0, _ -> failwith "metric has no label names but provide label values, maybe you need invoke noLables"
      | _, 0 -> failwith "metric has label names but not provide label values, maybe you need invoke noLables"
      | a, b when a = b -> Array.zip labelNames labelValues |> Map.ofSeq
      | _ -> failwith "metric labels should have same name/value length"
    let metric = metricStore.GetOrAdd(labels, fun _ ->  builder.build().wrapper)
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

and MetricBuilder<'t when 't :> IMetric> =
  abstract basicConf: BasicConf
  abstract build: unit -> Metric<'t>


type GaugeConf =
 {
   basicInfo: BasicConf
 }

 interface MetricBuilder<IGauge> with
   member x.build () = new Gauge(x) :> Metric<IGauge>

   member x.basicConf = x.basicInfo

 static member create name description =
   let basic = { name =  name; description = description; labelNames = [||] }
   {basicInfo = basic}
   
and Gauge(conf) =
 inherit Metric<IGauge>(conf)

 let mutable gaugeValue = new DoubleAdder()

 override x.wrapper = x :> IGauge

 interface IGauge with
   member x.inc value = gaugeValue.Add value
   member x.dec value = gaugeValue.Add -value
   member x.set value = gaugeValue <- new DoubleAdder(value)
   member x.explore () = 
     let confInfo = conf.basicInfo
     let basicInfo = { name= confInfo.name; description = confInfo.description }
     let gaugeValue =  gaugeValue.Sum()
     let metricInfo = { labels = Map.empty; gaugeValue = gaugeValue }
     (basicInfo, MetricInfo.Gauge metricInfo)



//
//type HistogramConf<'t when 't :> IMetric> =
//  {
//    basicInfo: BasicConf
//    buckets: float array
//  }
