namespace Logary.Model

open System.Collections.Generic
open Logary
open Logary.Internals

[<Sealed>]
type HistogramMessage(labels, buckets, sum, ?timestamp, ?messageId, ?name, ?level, ?ctx, ?fs) =
  inherit LogaryMessageBase(MessageKind.Histogram, ?timestamp=timestamp, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=fs)

  new(m: Logary.HistogramMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    HistogramMessage(m.labels, m.buckets, m.sum, m.timestamp, m.id, m.name, m.level, ctx, fs)

  new (h: Logary.Metric.HistogramInfo) =
    let ts = Global.getTimestamp()
    let mid = Id.create()
    HistogramMessage(h.labels, h.buckets, h.sum, ts, mid)

  member val labels = labels with get, set
  member val buckets = buckets with get, set
  member val sum = sum with get, set

  member x.writeCopy cb =
    let m = HistogramMessage x
    cb m
    m

  override x.cloneAndUpdate builder =
    x.writeCopy builder :> LogaryMessageBase


  interface Logary.HistogramMessage with
    member x.labels = x.labels
    member x.buckets = x.buckets
    member x.sum = x.sum
