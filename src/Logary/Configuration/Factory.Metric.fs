namespace Logary.Configuration.Metric

open NodaTime
open Hopac
open System
open System.Text.RegularExpressions
open Logary

[<NoEquality; NoComparison>]
type SpecificMetricConf =
  abstract Build : string -> MetricConf

// TODO: make this good and properly written
[<NoEquality; NoComparison>]
type MetricConfBuild<'T when 'T :> SpecificMetricConf> =
  abstract Metric : 'T
  // TODO: nest one more level here...
  abstract member AddMetric : Duration * string * Func<PointName, Job<Metric.T>>
                           -> MetricConfBuild<'T>

type ParentCallback<'T when 'T :> SpecificMetricConf> =
  SpecificMetricConf -> MetricConfBuild<'T> ref