namespace Logary.Metric
open System

type HistogramConf=
  {
    basicInfo: BasicConf
    buckets: float array
  }

  interface MetricBuilder<IHistogram> with
    member x.basicConf = x.basicInfo
    member x.build lables registry = new Histogram(x, lables) :> IHistogram



and Histogram(conf, labels) =
  let sumCounter = new DoubleAdder()
  let originSortedBucket = conf.buckets |> Array.sort
  let sortedBuckets =
    if Double.IsPositiveInfinity(Array.last originSortedBucket) then originSortedBucket
    else Array.append originSortedBucket [| Double.PositiveInfinity |]

  let sortedBucketCounters = sortedBuckets |> Seq.map(fun upperBound -> (upperBound, new DoubleAdder())) |> Map.ofSeq

  interface IHistogram with
    member x.observe value =
      (x :> IHistogram).observe(value, 1.)

    member x.observe (value, count) =
      if not <| Double.IsNaN(value) then
        let firstUpperBound = Array.find (fun upperBound -> value <= upperBound ) sortedBuckets
        sortedBucketCounters.[firstUpperBound].Add count
        sumCounter.Add value

    member x.explore () =
      let confInfo = conf.basicInfo
      let basicInfo = { name= confInfo.name; description = confInfo.description }
      let sumInfo =  sumCounter.Sum()
      let bucketsInfo = sortedBucketCounters |> Map.map (fun upperBound counter -> counter.Sum())
      let metricInfo = { labels = labels; sumInfo = sumInfo; bucketsInfo = bucketsInfo }
      (basicInfo, MetricInfo.Histogram metricInfo)

module HistogramConf =

  let defaultBuckets = [| 0.005; 0.01; 0.025; 0.05; 0.075; 0.1; 0.25; 0.5; 0.75; 1.; 2.5; 5.0; 7.5; 10. |]

  let buckets buckets conf =
    { conf with buckets = buckets }

  let linearBuckets start width count conf =
    if count < 1 then failwith "count needs positive"
    let buckets = seq {
      for i in 0 .. count-1 do
        yield start + float i * width
      }
    { conf with buckets = buckets |> Array.ofSeq }


  let exponentialBuckets start factor count conf =
    if count < 1 then failwith "count needs positive"
    if start <= 0. then failwith "start needs positive"
    if factor <= 1. then failwith "factor needs > 1"

    let buckets = seq {
      for i in 0 .. count-1 do
        yield start * Math.Pow(factor,float i)
      }
    { conf with buckets = buckets |> Array.ofSeq }

open HistogramConf
type HistogramConf with
  static member create(name, description) =
    let basic =  { name =  name; description = description; labelNames = [||] }
    { basicInfo = basic; buckets = defaultBuckets }
