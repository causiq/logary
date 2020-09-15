namespace Logary.Model

open Logary
open System.Collections.Generic

[<Sealed>]
type ForgetUserMessage(userId, ?timestamp, ?messageId, ?name, ?level, ?ctx, ?fs, ?gauges) =
  inherit LogaryMessageBase(MessageKind.ForgetUser, ?timestamp=timestamp, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=fs, ?gauges=gauges)

  new (m: Logary.ForgetUserMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    ForgetUserMessage(m.userId, m.timestamp, m.id, m.name, m.level, ctx, fs, gauges)

  member val userId = userId with get, set

  member x.writeCopy cb =
    let m = ForgetUserMessage x
    cb m
    m

  override x.cloneAndUpdate builder =
    x.writeCopy builder :> LogaryMessageBase

  interface Logary.ForgetUserMessage with
    member x.userId = x.userId
