namespace Logary.Configuration

open System
open NodaTime
open Hopac
open Logary

type MetricsConfBuild =
  // TODO: nest one more level here...
  abstract member AddMetric : Duration * string * Func<PointName, Job<Logary.Metric.Metric>> -> MetricsConfBuild