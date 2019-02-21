namespace Logary.Metric
open System.Collections.Concurrent


type BasicInfo =
  {
    name: string
    description: string
  }

type GaugeInfo =
  {
    labels: Map<string,string>
    gaugeValue: float
  }

type HistogramInfo =
  {
    labels: Map<string,string>
    x: int
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

  abstract observe: float -> unit


type BasicConf =
  {
    name: string
    description: string
    labelNames: string []
  }

/// used for exporting data
type MetricExporter =
  abstract basicConf: BasicConf
  abstract export: unit -> BasicInfo * seq<MetricInfo>
