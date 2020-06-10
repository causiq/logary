namespace Logary.Model

open System.Collections.Generic
open Logary

[<Sealed>]
type EventMessage(event, monetaryValue, ?timestamp, ?messageId, ?name, ?level, ?ctx, ?fs, ?gauges, ?received, ?error) =
  inherit LogaryMessageBase(MessageKind.Event, ?timestamp=timestamp, ?messageId=messageId, ?name=name, ?level=level, ?ctx=ctx, ?fs=fs, ?gauges=gauges, ?received=received)

  new(event: string) = EventMessage(event, None)

  new(m: Logary.EventMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    EventMessage(m.event, m.monetaryValue, m.timestamp, m.id, m.name, m.level, ctx, fs, gauges)

  new (m: Logary.LogaryMessage) =
    let ctx = Dictionary<_,_>() in let fs = Dictionary<_,_>() in let gauges = Dictionary<_,_>()
    m.context |> Seq.iter (fun (KeyValue (k, v)) -> ctx.Add(k, v))
    m.fields |> Seq.iter (fun (KeyValue (k, v)) -> fs.Add(k, v))
    m.gauges |> Seq.iter (fun (KeyValue (k, v)) -> gauges.Add(k, v))
    EventMessage("Converted Logary.LogaryMessage to event message", None, m.timestamp, m.id, m.name, m.level, ctx, fs, gauges)

  member x.writeCopy cb =
    let y = EventMessage x
    cb y
    y

  override x.addExn(e, ?level) =
    level |> Option.iter (fun level -> x.level <- level)
    x.error <- Some (e.toErrorInfo())

  member val event = event with get, set
  member val monetaryValue = monetaryValue with get, set
  member val error = error with get, set

  interface Logary.EventMessage with
    member x.event = x.event
    member x.monetaryValue = x.monetaryValue
    member x.error = x.error
