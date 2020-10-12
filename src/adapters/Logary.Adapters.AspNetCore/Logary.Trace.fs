namespace Logary.Trace

open Logary
open Logary.Trace
open Logary.Trace.Propagation
open Microsoft.AspNetCore.Http
open System.Runtime.CompilerServices

[<Extension; AutoOpen>]
module LoggerEx =
  open Logary.Trace.Propagation.HttpContext

  let private startNewSpanFrom (x: Logger, ctx: HttpContext, propagator: Propagator option) =
    let propagator = propagator |> Option.defaultWith (fun () -> Combined.choose [ Jaeger.propagator; W3C.propagator ])
    let attrs, parentO = propagator.extract(getter, ctx)
    // https://github.com/open-telemetry/opentelemetry-specification/issues/210
    // https://github.com/open-telemetry/opentelemetry-python/pull/89/files
    let span =
      x.buildSpan(sprintf "%O %s" ctx.Request.Method ctx.Request.Path.Value, ?parent=parentO)
       .setKind(SpanKind.Server)
       .setAttribute("component", "http")
       .setAttribute("http.method", ctx.Request.Method)
       .setAttribute("http.route", ctx.Request.Path.Value)
       .setAttribute("http.query", ctx.Request.QueryString.Value)
       .setAttributes(attrs)
       .startWith(x)

    if ctx.Request.Query.ContainsKey "debug" then span.debug()

    ctx.Items.[UserStateLoggerKey] <- span

    span, ctx

  [<Extension; CompiledName "StartSpan">]
  let startSpan (x: Logger, ctx: HttpContext, propagator: Propagator option) =
    if isNull ctx then nullArg "ctx"
    match ctx.Items.TryGetValue UserStateLoggerKey with
    | false, _ ->
      startNewSpanFrom (x, ctx, propagator)
    | true, existing ->
      eprintfn "'startSpan' called more than once for a HttpContext"
      existing :?> SpanLogger, ctx

  type Logger with
    member x.startSpan (ctx: HttpContext, ?propagator: Propagator) =
      startSpan (x, ctx, propagator)


[<Extension; AutoOpen>]
module SpanLoggerEx =

  [<Extension; CompiledName "Finish">]
  let finish (x: SpanLogger, ctx: HttpContext, transform: (Model.SpanMessage -> unit) option) =
    if ctx.Response.StatusCode >= 500 then
      x.setStatus(SpanStatusCode.Error)
      x.setAttribute("error", true)
    elif
      ctx.Response.StatusCode >= 400 then x.setAttribute("http.bad_request", true)
    x.setAttribute("http.status_code", ctx.Response.StatusCode)

    match transform with
    | None ->
      x.finish ()
    | Some transform ->
      x.finish(transform)

  [<Extension; CompiledName "FinishWithException">]
  let finishWithExn (x: SpanLogger, _: HttpContext, ex: exn) =
    x.setAttribute("http.status_code", 500)
    x.setStatus(SpanStatusCode.Error, ex.ToString())
    x.setAttribute("error", true)
    x.error("Unhandled exception", fun m -> m.addExn ex)
    x.finish()

  type SpanLogger with
    member x.finish (ctx: HttpContext, ?transform: Model.SpanMessage -> unit) = finish (x, ctx, transform)
    member x.finishWithExn (ctx: HttpContext, ex: exn) = finishWithExn (x, ctx, ex)

