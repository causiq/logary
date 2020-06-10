namespace Logary.Metric

type BasicInfo =
  { name: string
    description: string }

type GaugeInfo =
  { /// The labels this Histogram "is for"; like its name.
    labels: Map<string, string>
    /// The current value of the gauge
    value: float
    /// Which unit is this measurement in?
    unit: Logary.U
  }

type HistogramInfo =
  { /// The labels this Histogram "is for"; like its name.
    labels: Map<string, string>
    /// A mapping between bucket upper bounds, inclusive, and its count
    buckets: Map<float, float>
    /// Total sum of observed values
    sum: float
    /// Which unit is the base unit for this histogram?
    unit: Logary.U
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
  abstract observe: value: float -> unit
  abstract observe: value: float * count: float -> unit


[<RequireQualifiedAccess>]
type FailStrategy =
  /// The default FailStrategy is to call `MetricRegistry.defaultFailBehavior`
  | Default
  /// The Throw FailStrategy is to raise an unhandled uncatchable exception.
  | Throw

type BasicConf =
  { name: string
    unit: Logary.U
    description: string
    labelNames: string[]
    avoidHighCardinality: int option
    failStrategy: FailStrategy
  }
  member x.asInfo: BasicInfo =
    { name=x.name; description=x.description }

module BasicConf =
  /// https://prometheus.io/docs/practices/instrumentation/#do-not-overuse-labels
  let defaultHighCardinalityLimit = 150

  let labelNames labelNames conf =
    { conf with labelNames = labelNames }

  let throwWhenFail conf =
    { conf with failStrategy = FailStrategy.Throw }

type BasicConf with
  static member create (name, description, units, ?labelNames) =
    { name = name
      unit = units
      description = description
      labelNames = defaultArg labelNames [||]
      avoidHighCardinality = Some BasicConf.defaultHighCardinalityLimit
      failStrategy = FailStrategy.Default }

/// Used for reading the state of the metric.
type MetricExporter =
  abstract basicConf: BasicConf
  abstract export: unit -> BasicInfo * seq<MetricInfo>
