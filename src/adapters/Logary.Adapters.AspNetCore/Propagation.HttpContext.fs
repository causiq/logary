module internal Logary.Trace.Propagation.HttpContext

open System
open Logary.Trace.Propagation
open Microsoft.AspNetCore.Http

let getter: Getter<HttpContext> =
  fun ctx nameOrPrefix ->
    [ for h in ctx.Request.Headers do
        if h.Key.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase) then
          yield h.Key, List.ofArray (h.Value.ToArray())
    ]

let setter: Setter<HttpContext> =
  fun (k: string, vs) ctx ->
    ctx.Response.Headers.SetCommaSeparatedValues(k, Array.ofList vs)
    ctx

let UserStateLoggerKey = "Logary.Trace.SpanLogger"