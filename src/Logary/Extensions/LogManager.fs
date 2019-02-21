namespace Logary

open Logary.Metric

open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module LogManagerEx =
  let defaultMetricRegistry = new MetricRegistry()

  type LogManager with
    member x.getLogger name =
      x.getLogger (PointName.parse name)
      
    static member DefaultMetricRegistry = defaultMetricRegistry

