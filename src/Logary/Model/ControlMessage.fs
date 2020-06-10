namespace Logary.Model

open Logary
open System.Collections.Generic

[<Sealed>]
type ControlMessage(controlKind, ?children: Logary.ControlMessage[], ?timestamp, ?messageId, ?name, ?level, ?ctx, ?fs, ?gauges) =
  inherit LogaryMessageBase(MessageKind.Control, ?timestamp=timestamp, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=fs, ?gauges=gauges)

  let children = defaultArg children Array.empty

  new(m: Logary.ControlMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    ControlMessage(m.ckind, m.children, m.timestamp, m.id, m.name, m.level, ctx, fs, gauges)

  member x.writeCopy cb =
    // copy and return
    let y = ControlMessage x
    cb y
    y

  interface Logary.ControlMessage with
    member x.ckind = controlKind
    member x.children = children

