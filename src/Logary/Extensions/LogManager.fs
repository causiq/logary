namespace Logary

open Logary.Metric

open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module LogManagerEx =
  let defaultMetricRegistry = new MetricRegistry()

  type LogManager with
    member x.getLogger (loggerName: string) =
      x.getLogger (PointName.parse loggerName)
    member x.getLoggerWithMiddleware (loggerName: string, middleware) =
      x.getLoggerWithMiddleware (PointName.parse loggerName, middleware)
    static member DefaultMetricRegistry = defaultMetricRegistry
