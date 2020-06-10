namespace Logary.Model

open Logary
open System.Collections.Generic

[<Sealed>]
type IdentifyUserMessage(prevUserId, newUserId, ?timestamp, ?messageId, ?name, ?level, ?ctx, ?fs, ?gauges) =
  inherit LogaryMessageBase(MessageKind.IdentifyUser, ?timestamp=timestamp, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=fs, ?gauges=gauges)

  new (m: Logary.IdentifyUserMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    IdentifyUserMessage(m.prevUserId, m.newUserId, m.timestamp, m.id, m.name, m.level, ctx, fs, gauges)

  member val prevUserId = prevUserId with get, set
  member val newUserId = newUserId with get, set

  interface Logary.IdentifyUserMessage with
    member x.prevUserId = x.prevUserId
    member x.newUserId = x.newUserId
