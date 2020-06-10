module Logary.Trace.ActiveSpan

open System.Threading
open Logary
open Logary.Trace

let private asyncLocal: AsyncLocal<SpanContext> = AsyncLocal<SpanContext>()

let getContext () =
  let active = asyncLocal.Value
  if obj.ReferenceEquals(active, null) then None
  else Some active

let setContext (ctx: SpanContext option) =
  asyncLocal.Value <- match ctx with Some ctx -> ctx | _ -> Unchecked.defaultof<_>

let getSpan () =
  getContext () |> Option.map (fun ctx -> ctx.spanId)
