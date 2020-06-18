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

  member x.unit =
    match x with
    | Gauge g ->
      g.unit
    | Histogram h ->
      h.unit

  member x.kind =
    match x with
    | Gauge _ -> "gauge"
    | Histogram _ -> "histogram"

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
  /// The default FailStrategy is to call `registry.failWith` on the `MetricRegistry`.
  | RegistryDecides
  /// The Throw FailStrategy is to raise an unhandled uncatchable exception.
  | Throw

type MetricConf =
  { name: string
    unit: Logary.U
    description: string
    labelNames: string[]
    maxCardinality: int option
    failStrategy: FailStrategy
  }
  member x.asInfo: BasicInfo =
    { name=x.name; description=x.description }

module MetricConf =
  /// https://prometheus.io/docs/practices/instrumentation/#do-not-overuse-labels
  let DefaultMaxCardinality = 150

  let labelNames labelNames (conf: MetricConf) =
    { conf with labelNames = labelNames }

  let description desc (conf: MetricConf) =
    { conf with description = desc }

  let failByThrowing (conf: MetricConf) =
    { conf with failStrategy = FailStrategy.Throw }

  let ignoreHighCardinality (conf: MetricConf) =
    { conf with maxCardinality = None }

  let setMaxCardinality maxCardinality (conf: MetricConf) =
    { conf with maxCardinality = Some maxCardinality }

  let setUnit unit (conf: MetricConf) =
    { conf with unit = unit }


type MetricConf with
  static member create (name, description, unit, ?labelNames) =
    { name = name
      unit = unit
      description = description
      labelNames = labelNames |> Option.map Array.ofSeq |> Option.defaultValue Array.empty
      maxCardinality = Some MetricConf.DefaultMaxCardinality
      failStrategy = FailStrategy.RegistryDecides }

/// Used for reading the state of the metric.
type MetricExporter =
  abstract conf: MetricConf
  abstract export: unit -> BasicInfo * MetricInfo[]
