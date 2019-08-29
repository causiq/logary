namespace Logary.Metric

type BasicInfo =
  { name: string
    description: string
  }

type GaugeInfo =
  { labels: Map<string,string>
    gaugeValue: float
  }

type HistogramInfo =
  { labels: Map<string,string>
    bucketsInfo: Map<float,float>
    sumInfo: float
  }

type MetricInfo =
  | Gauge of GaugeInfo
  | Histogram of HistogramInfo

type IMetric =
  abstract explore: unit -> BasicInfo * MetricInfo

type IGauge =
  inherit IMetric

  abstract inc: float -> unit
  abstract dec: float -> unit
  abstract set: float -> unit

type IHistogram =
  inherit IMetric

  abstract observe: value:float -> unit
  abstract observe: value:float * count:float -> unit


[<RequireQualifiedAccess>]
type FailStrategy =
  /// The default FailStrategy is to call `MetricRegistry.defaultFailBehavior`
  | Default
  /// The Throw FailStrategy is to raise an unhandled uncatchable exception.
  | Throw

type BasicConf =
  { name: string
    description: string
    labelNames: string[]
    avoidHighCardinality: int option
    failStrategy: FailStrategy
  }

module BasicConf =
  /// https://prometheus.io/docs/practices/instrumentation/#do-not-overuse-labels
  let defaultHighCardinalityLimit = 150

  let labelNames labelNames conf =
    { conf with labelNames = labelNames }

  let throwWhenFail conf =
    { conf with failStrategy = FailStrategy.Throw }

type BasicConf with
  static member create (name, description, ?labelNames) =
    { name = name
      description = description
      labelNames = defaultArg labelNames [||]
      avoidHighCardinality = Some BasicConf.defaultHighCardinalityLimit
      failStrategy = FailStrategy.Default }

/// used for exporting data
type MetricExporter =
  abstract basicConf: BasicConf
  abstract export: unit -> BasicInfo * seq<MetricInfo>
