namespace Logary.Metric


type GaugeConf =
 {
   basicInfo: BasicConf
 }

 interface MetricBuilder<IGauge> with
   member x.build labels = new Gauge(x, labels) :> IGauge

   member x.basicConf = x.basicInfo

 static member create name description =
   let basic = { name =  name; description = description; labelNames = [||] }
   {basicInfo = basic}

and Gauge(conf, labels) =

 let mutable gaugeValue = new DoubleAdder()

 interface IGauge with
   member x.inc value = gaugeValue.Add value
   member x.dec value = gaugeValue.Add -value
   member x.set value = gaugeValue <- new DoubleAdder(value)
   member x.explore () =
     let confInfo = conf.basicInfo
     let basicInfo = { name= confInfo.name; description = confInfo.description }
     let gaugeValue =  gaugeValue.Sum()
     let metricInfo = { labels = labels; gaugeValue = gaugeValue }
     (basicInfo, MetricInfo.Gauge metricInfo)