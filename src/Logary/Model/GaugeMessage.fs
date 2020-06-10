namespace Logary.Model

open Logary
open System.Collections.Generic
open Logary.Internals

[<Sealed>]
type GaugeMessage(gauge, labels, ?children, ?timestamp, ?messageId, ?name, ?level, ?ctx, ?fs) =
  inherit LogaryMessageBase(MessageKind.Gauge, ?timestamp=timestamp, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=fs, ?gauges=children)

  new (m: Logary.GaugeMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    GaugeMessage(m.gauge, m.labels, gauges, m.timestamp, m.id, m.name, m.level, ctx, fs)

  new (g: Logary.Metric.GaugeInfo) =
    let ts = Global.getTimestamp()
    let mid = Id.create()
    GaugeMessage(Gauge (Value.Float g.value, g.unit), g.labels, ?timestamp=Some ts, ?messageId=Some mid)

  member val gauge = gauge with get, set
  member val labels = labels with get, set

  interface Logary.GaugeMessage with
    member x.gauge = x.gauge
    member x.labels = x.labels
