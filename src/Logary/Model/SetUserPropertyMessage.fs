namespace Logary.Model

open Logary
open System.Collections.Generic

[<Sealed>]
type SetUserPropertyMessage(key, value, ?timestamp, ?messageId, ?name, ?level, ?ctx, ?fs, ?gauges) =
  inherit LogaryMessageBase(MessageKind.SetUserProperty, ?timestamp=timestamp, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=fs, ?gauges=gauges)

  new (m: Logary.SetUserPropertyMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    SetUserPropertyMessage(m.key, m.value, m.timestamp, m.id, m.name, m.level, ctx, fs, gauges)

  member val key = key with get, set
  member val value = value with get, set

  interface Logary.SetUserPropertyMessage with
    member x.key = x.key
    member x.value = x.value
